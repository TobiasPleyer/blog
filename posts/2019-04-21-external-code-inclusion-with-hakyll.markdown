---
title:  "External code inclusion with Hakyll"
date: 2019-04-21
tags: blog, hakyll
category: Programming
authors: Tobias Pleyer
summary: "I present to possible solutions to include external code snippets in your blog"
---

External code inclusion with Hakyll
===================================

The possibility to include external code snippets in your blog comes with a
bunch of niceties:

- It keeps your blog posts tidier, more concise and focused on the textual
  content
- It saves you from constant copy/paste when you test and modify the code
- It saves you from remembering which post contains what code if you change
  something later
- You can share code between several blog posts

In this blog post I want to present two solutions how to achieve code inclusion
for Hakyll.

Template based solution
-----------------------

Hakyll's template engine already comes with a template context that allows to
include the textual content of another file:
[snippetField](https://jaspervdj.be/hakyll/reference/Hakyll-Web-Template-Context.html#v:snippetField).

However, this alone is not enough as the documentation also states:

    The contents of the included file will not be interpolated.

Which means without further measures we won't get syntax highlighting, which is
typically what you want when including code.

But this brings us already very close to the solution we want. So our strategy
is as follows:

1. Wrap the included code in code markup annotations
2. Render the template **before** the Pandoc conversion to HTML

We need to render the template before the Pandoc conversion, because Pandoc
will give us syntax highlighting, so Pandoc needs to know this is code and thus
we have to annotate it beforehand.

But this brings up another small problem: All of Hakyll's `pandocCompiler*`
flavored functions are meant to be at the beginning of the compiler chain. Why?
Because they don't take an `Item a` input parameter, but instead fetch the
markup source internally via `getResourceBody`. You can have a look at the
[source](https://jaspervdj.be/hakyll/reference/src/Hakyll.Web.Pandoc.html#pandocCompilerWithTransformM)
to understand what I mean.

So to make our solution work we have to write our own, slightly modified
compiler that receives the markup code as input parameter. Here is the code for
that:

```haskell
pandocCompilerForCodeInsertion :: Item String -> Compiler (Item String)
pandocCompilerForCodeInsertion content = do
  itemPandoc <- readPandocWith defaultHakyllReaderOptions content
  itemPandoc' <- traverse (return . id) itemPandoc
  return $ writePandocWith defaultHakyllWriterOptions itemPandoc'
```

Now we can write a function template that will wrap the contents of a file in a
code block, which is almost the implementation of `snippetField`, but with the
additional markup support:

```haskell
codeIncludeField :: Context String
codeIncludeField = functionField "code-include" f
  where
    f (contentsPath : lexer : []) _ = fmap (wrapCode lexer) (loadBody (fromFilePath contentsPath))
    f _ i = error $ "codeIncludeField needs a filepath and a lexer " ++ show (itemIdentifier i)
    wrapCode lexer code = "```" ++ lexer ++ "\n" ++ code ++ "\n```"
```

With these changes we are now able to compile our posts. If your post compiler
rule looks like this:

```haskell
match "posts/*" $ do
    route $ setExtension "html"
    compile $ do
        pandocCompiler
          >>= loadAndApplyTemplate "templates/post.html" postCtx
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls
```

Then you have to rewrite this rule like this:

```haskell
match "posts/*" $ do
    route $ setExtension "html"
    let codeCtx = codeIncludeField <> postCtx
    compile $ do
        getResourceBody
          >>= applyAsTemplate codeCtx
          >>= pandocCompilerForCodeInsertion
          >>= loadAndApplyTemplate "templates/post.html" codeCtx
          >>= loadAndApplyTemplate "templates/default.html" codeCtx
          >>= relativizeUrls
```

This works because between loading the post content with `getResourceBody` and
compiling the code to HTML the `code-include` template is applied. Now we are
able to write something like this in our blog posts:

```markdown
$code-include("code/some_file.hs", "haskell")$
```

and the syntax highlighted code of the respective file will be rendered into
the output.

**Caveats**

I, personally, am not very keen on this solution because it presents a very
Hakyll specific solution and mixes two DSLs, namely markdown and Hakyll's
template language, in the same file.

But the much bigger problem is that you have to keep an eye on the dollar signs
in your blog post! Dollar signs (*$*) indicate special template elements for
Hakyll's template engine. If your blog posts contains "stray" dollar signs that
are not meant as template fields, e.g. because you include some verbatim
Haskell code with some dollar signs, this will lead to unexpected and badly
rendered results.

Solution based on fenced divs
-----------------------------

**Edit on 2019-04-26:** Made the function `codeBlockFromDiv` more concise, and
made the file path a keyword of the *Div*.

[Fenced divs](https://pandoc.org/MANUAL.html#extension-fenced_divs) is a nice
extension of Pandoc that allows to embed custom generic blocks within your
markup code. These blocks are then represented as
[Div](https://hackage.haskell.org/package/pandoc-types/docs/Text-Pandoc-Definition.html#t:Block)
blocks in Pandoc. These Div blocks can be given arbitrary attributes.

But we don't want the Div blocks to be written to HTML. Instead we will
intervene between Pandoc's reader (parser) and writer (compiler) process to
rewrite the respective `Div` into a `CodeBlock`, which will subsequently be
properly syntax highlighted.

For this to work we only need two things:

1. A snippet map that maps a file's name to its content
2. A modified Pandoc compiler that will rewrite Divs to CodeBlocks

I keep all my code snippets in a folder named *code*, so to make the snippets
known to Hakyll we need the following `Rule`:

```haskell
match "code/**" $ do
    route idRoute
    compile getResourceString
```

This makes all the files in the *code* folder loadable within the monadic
compiler code of Hakyll. Further we need the following helper function:

```haskell
toSnippetMap :: [Item String] -> M.Map FilePath String
toSnippetMap is = M.fromList kvs
  where kvs = map ((toFilePath . itemIdentifier) &&& itemBody) is
```

which transforms a list of `Item`s holding a string into a `Map` from file name
to file content.

Now to the compiler. Hakyll already comes with exactly the function that we
need:
[pandocCompilerWithTransform](https://jaspervdj.be/hakyll/reference/Hakyll-Web-Pandoc.html#v:pandocCompilerWithTransform).
This function takes as its third parameter a function that transforms a
`Pandoc` into a `Pandoc`. When we call this compiler with the right
transformation function we are done.

Conveniently we do not have to work too hard to write such a function, because
the Pandoc class is an instance of the
[Walkable](https://hackage.haskell.org/package/pandoc-types-1.19/docs/Text-Pandoc-Walk.html#t:Walkable)
type class, which exposes the `walk` function. The `walk` function takes a per
`Block` element transformation function and applies it over the complete AST of
parsed blocks. With this knowledge we are ready to write the function we want:

```haskell
pandocCompilerWithCodeInsertion :: M.Map FilePath String -> Compiler (Item String)
pandocCompilerWithCodeInsertion snippetMap =
  pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions (codeInclude snippetMap)

codeInclude :: M.Map FilePath String -> Pandoc -> Pandoc
codeInclude snippetMap = walk $ \block -> case block of
  div@(Div (_,cs,_) _) -> if "code-include" `elem` cs
                          then codeBlockFromDiv snippetMap div
                          else block
  _ -> block

codeBlockFromDiv snippetMap div@(Div (_,_,kvs) _) =
  let classes = maybeToList $ lookup "lexer" kvs
      content = lookup "file" kvs >>= (`M.lookup` snippetMap)
  in maybe Null (CodeBlock ("",classes,[])) content
codeBlockFromDiv _ _ = Null
```

Now we are able to write our `Rule` to create our posts:

```haskell
match "posts/*" $ do
    route $ setExtension "html"
    compile $ do
        snippetMap <- toSnippetMap <$> loadAll "code/**"
        pandocCompilerWithCodeInsertion snippetMap
          >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
          >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
          >>= relativizeUrls
```

We are done! With these changes we are from now on able to include external
code with the following syntax:

```markdown
::: {.code-include lexer="haskell" file="path/to/file"}
:::
```

I like this solution because it does not interfere with Hakyll's template
mechanism, but uses conventional markdown syntax to achieve the desired
behavior. For reference, below is my complete `site.hs` file to generate my
blog. Have a look at the
[raw markdown source](https://raw.githubusercontent.com/TobiasPleyer/blog/master/posts/2019-04-21-external-code-inclusion-with-hakyll.markdown)
to convince yourself that it is included via a fenced div:

::: {.code-include lexer="haskell" file="code/site_2019-04-26.hs"}
:::
