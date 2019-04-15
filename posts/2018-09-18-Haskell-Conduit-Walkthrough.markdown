---
title:  "Haskell Conduit Walkthrough"
date: 2018-09-18
tags: haskell, conduit, streams
category: Programming
authors: Tobias Pleyer
summary: "A complete evaluation of a simple conduit pipeline in Haskell"
---

Haskell Conduit Walkthrough
===========================

[In a previous post](./2018-02-21-Haskell-Pipes-Walkthrough.html) I
provided a complete series of transformations of a simple streaming
application using the [pipes
package](https://hackage.haskell.org/package/pipes).

In the Haskell library eco system exists another competing streaming
library: [conduit](https://hackage.haskell.org/package/conduit) authored
by Michael Snoyman. Since Michael has created an absurd amount of
libraries, using conduit in most of them, conduit has an impressive user
base and provides solid and carefully engineered code with a lot of high
level combinators for ease of use.

In this post I want to provide a walkthrough of code very similar to the
original post. The following code snippet is identical in look and
behavior of the original:

::: {.code-include lexer="haskell"}
code/post54/Main1.hs
:::

This piece of code will serve as the basis of the following
transformations.

**Note:** The above code uses conduit's primitives in order to be as
close to the original post as possible. However, even though correct,
this is not really idiomatic conduit code. A more terse and readable
version would be

``` {.sourceCode .haskell}
module Main where

import Conduit

main :: IO ()
main = runConduit $ yieldMany [1,2] .| mapC (+1) .| printC
```

**Note 2:** As was the case with the pipes code the following conduit
code requires slight modifications to the original package for all the
imports to be available. This is the case because we use functions and
data type constructors which are not exported by the internal package
code. The way I achieve this is by creating an empty
[stack](https://docs.haskellstack.org/en/stable/README/) project,
copying the conduit package into the project as a library, modify the
package.yaml file and then export everything I need in the respective
modules. The actual code can be found [on my
github](https://github.com/TobiasPleyer/Playground/tree/master/Haskell/myconduit).

Evaluation in 45 steps
----------------------

The code above equals...

::: {.code-include lexer="haskell"}
code/post54/Main2.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main3.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main4.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main5.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main6.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main7.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main8.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main9.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main10.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main11.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main12.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main13.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main14.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main15.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main16.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main17.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main18.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main19.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main20.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main21.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main22.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main23.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main24.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main25.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main26.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main27.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main28.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main29.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main30.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main31.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main32.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main33.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main34.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main35.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main36.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main37.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main38.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main39.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main40.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main41.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main42.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main43.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main44.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main45.hs
:::

equals...

::: {.code-include lexer="haskell"}
code/post54/Main46.hs
:::
