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
main = hakyll $ do
    match "images/*" $ do
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

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            -- body <- getResourceBody
            -- pandoc <- readPandocWith defaultHakyllReaderOptions body
            -- unsafeCompiler (do
            --     writeFile ((flip replaceExtension "pandoc" . toFilePath . itemIdentifier) pandoc) (show (itemBody pandoc))
            --     return pandoc)
            -- makeItem "test.txt" :: Compiler (Item String)
            snippets <- toSnippetsMap <$> loadAll "code/**"
            pandocCompilerWithCodeInsertion snippets
              -- >>= (\i -> unsafeCompiler (do writeFile ((flip replaceExtension "pandoc" . toFilePath . itemIdentifier) i) (show (itemBody i)); return i))
              >>= loadAndApplyTemplate "templates/post.html" postCtx
              >>= loadAndApplyTemplate "templates/default.html" postCtx
              >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    blogCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
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

    match "code/**" $ compile getResourceString

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
blogCtx :: Context String
blogCtx =
    constField "blog_title" "Tobi's blog" `mappend`
    constField "blog_name" "My blog about programming and other stuff" `mappend`
    defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    blogCtx

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