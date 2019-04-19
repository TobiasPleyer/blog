--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Arrow ((&&&))
import qualified Data.Map.Strict as M
import           Data.Maybe (maybeToList)
import           Data.Monoid (mappend)
import           System.FilePath
import           Text.Pandoc
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/**" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["links.markdown", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" blogCtx
            >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let tagCtx =
                    constField "title" title `mappend`
                    listField "posts" postCtx (return posts) `mappend`
                    blogCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/tags.html" tagCtx
                >>= loadAndApplyTemplate "templates/default.html" tagCtx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            snippets <- toSnippetsMap <$> loadAll ("code/**" .||. "site.hs")
            pandocCompilerWithCodeInsertion snippets
              >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
              >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
              >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    constField "title" "Archives" `mappend`
                    listField "posts" postCtx (return posts) `mappend`
                    blogCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["tags.html"] $ do
        route idRoute
        compile $ do
            let tagsCtx =
                    constField "title" "Tags collection" `mappend`
                    listField "tags" postCtx (traverse (makeItem . fst) (tagsMap tags)) `mappend`
                    blogCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag-list.html" tagsCtx
                >>= loadAndApplyTemplate "templates/default.html" tagsCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return (take 3 posts)) `mappend`
                    constField "title" "Home" `mappend`
                    blogCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "code/**" $ do
        route idRoute
        compile getResourceString

    match "site.hs" $ do
        compile getResourceString

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync --size-only -ave 'ssh -p 22' _site/ tpleyer.de@ssh.strato.de:.blog/"
    }

blogCtx :: Context String
blogCtx =
    constField "blog_title" "Tobi's blog" `mappend`
    constField "blog_name" "My blog about programming and other stuff" `mappend`
    defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    blogCtx

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

toSnippetsMap :: [Item String] -> M.Map FilePath String
toSnippetsMap is = M.fromList kvs
  where kvs = map ((toFilePath . itemIdentifier) &&& itemBody) is

pandocCompilerWithCodeInsertion :: M.Map FilePath String -> Compiler (Item String)
pandocCompilerWithCodeInsertion snippets =
  pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions (codeInclude snippets)

codeInclude :: M.Map FilePath String -> Pandoc -> Pandoc
codeInclude snippets = walk $ \block -> case block of
  div@(Div (ident,cs,kvs) bs) -> if "code-include" `elem` cs
                                 then codeBlockFromDiv snippets div
                                 else block
  _ -> block

fromPara (Para is) = is
fromStr (Str s) = s

codeBlockFromDiv snippets div@(Div (ident,cs,kvs) bs) =
  let mLexer = lookup "lexer" kvs
      css = maybeToList mLexer
      path = (fromStr . head . fromPara . head) bs
      content = M.lookup path snippets
  in maybe Null (CodeBlock ("",css,[])) content
codeBlockFromDiv _ _ = Null
