---
title:  "Haskell Pipes Walkthrough"
date: 2018-02-21
tags: haskell
category: Programming
authors: Tobias Pleyer
summary: "A complete evaluation of a simple pipeline in Haskell"
---

Haskell Pipes Walkthrough
=========================

Intro
-----

Lately I discovered Haskell's [pipes
package](https://hackage.haskell.org/package/pipes). It's a really nice
package, but not to grasp so easily. The author, Gabriel Gonzalez, has
an execellent knowledge of Haskell. I had a quick look at his other
stuff and he always writes crystall clear, well documented and he loves
category theory...

I'd like to understand more of the code of the pipes package and how it
works. As usual you best learn something when you work with it, so what
follows is a complete evaluation of a simple program using pipes.

**Note:** I have verified that each step does in fact lead to the same
results.

**Note 2:** Simply installing the pipes package and other package
dependencies is not enough to run the code snippets below, because
functions like [\_bind]{.title-ref} and data types like
[Proxy]{.title-ref} are not exported by the pipes package (not public).
So what I did is download the package sources and then created my own
[stack](https://docs.haskellstack.org/en/stable/README/) project on top
of it in which I explicitly export these definitions.

Evaluation in 32 steps
----------------------

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation1.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation2.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation3.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation4.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation5.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation6.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation7.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation8.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation9.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation10.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation11.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation12.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation13.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation14.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation15.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation16.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation17.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation18.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation19.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation20.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation21.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation22.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation23.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation24.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation25.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation26.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation27.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation28.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation29.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation30.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation31.hs"}
:::
equals...

::: {.code-include lexer="haskell" file="code/pipes_evaluation/pipes_evaluation32.hs"}
:::