---
title:  "callCC in Haskell"
date: 2019-02-02
tags: haskell, continuation passing style, functional programming
category: Programming
authors: Tobias Pleyer
summary: "A short investigation of the callCC function and how it works"
---

callCC in Haskell
=================

Intro
-----

Continuation passing style is a very interesting programming concept. In
Haskell we have the continuation monad
[ContT](https://hackage.haskell.org/package/transformers-0.5.5.0/docs/src/Control.Monad.Trans.Cont.html#ContT),
implemented in the [transformers
package](https://hackage.haskell.org/package/transformers-0.5.5.0/docs/Control-Monad-Trans-Cont.html).
An introduction to the topic can be found in the [open Haskell wiki
book](https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style)
and Gabriel Gonzalez wrote a [very nice
article](http://www.haskellforall.com/2012/12/the-continuation-monad.html)
about why continuation passing style matters.

The wiki book article also mentions the *callCC* function. I think in
general the explanation and motivation for this function is conveyed
well in this article and the article is also not short on examples. The
only thing I'm missing in it is an example of a full evaluation
involving *callCC*. I think once you see how the function actually works
things become much clearer. That will be the goal of this article. I
won't give any extra explanations, instead I will take one of the
examples from the article and do a full evaluation of it.

callCC in action
----------------

**Note:** I will use the definitions from the transformers package, not
from the wiki book article.

Here is the full example we will evaluate:

::: {.code-include lexer="haskell" file="code/post59/bar.hs"}
:::
We will transform the bar computation. Desugaring the outer do-notation
yields

::: {.code-include lexer="haskell" file="code/post59/bar2.hs"}
:::
Using the definition of the ContT monad's bind (\>\>=) function and
giving the anonymous function to callCC a name yields

::: {.code-include lexer="haskell" file="code/post59/bar3.hs"}
:::
Now we use [the definition of
callCC](https://hackage.haskell.org/package/transformers-0.5.5.0/docs/src/Control.Monad.Trans.Cont.html#callCC)

::: {.code-include lexer="haskell" file="code/post59/bar4.hs"}
:::
Now we can also desugar the inner do computation, which yields

::: {.code-include lexer="haskell" file="code/post59/bar5.hs"}
:::
Now we can fill in the definition of check, unpack the delayed
continuation with [runContT]{.title-ref} and then apply the continuation
to it

::: {.code-include lexer="haskell" file="code/post59/bar6.hs"}
:::
Now we can substitute f by its definition and write the lengthy line in
several lines

::: {.code-include lexer="haskell" file="code/post59/bar7.hs"}
:::
Once again using the definition ContT monad's bind (\>\>=) function we
end up with

::: {.code-include lexer="haskell" file="code/post59/bar8.hs"}
:::
Unwrapping the inner [runContT ContT \...]{.title-ref} layer and
substituting for [when\_f]{.title-ref} yields

::: {.code-include lexer="haskell" file="code/post59/bar9.hs"}
:::
Now we are able to discuss the possible outcomes of the computation.

### Calling bar with 'hello'

Let's assume we call [bar]{.title-ref} with the string 'hello'. In this
case the predicate function of [when]{.title-ref} returns
[True]{.title-ref} and [when]{.title-ref} will simply evaluate to its
second argument and we end up with

::: {.code-include lexer="haskell" file="code/post59/bar10.hs"}
:::
Which we can simplify to

::: {.code-include lexer="haskell" file="code/post59/bar11.hs"}
:::
If we cheat a bit we can rewrite it a bit further. The following won't
compile, because we are using variables out of scope, but it helps to
see things a bit better

::: {.code-include lexer="haskell" file="code/post59/bar12.hs"}
:::
Now we can run the [main]{.title-ref} function (again in a sort of
pseudo code)

::: {.code-include lexer="haskell" file="code/post59/bar13.hs"}
:::
They key point to note here is that [f]{.title-ref} ignored its
argument, thus it was completely irrelevant what the actual value of
[k]{.title-ref} was. That is exactly the "early return" behavior that
*callCC* aims to provide. If you look way to the beginning of the code
transformation process you'll notice that the definition of
[f]{.title-ref} came from the application of *callCC*.

### Calling bar with 'other'

Let's now assume we call [bar]{.title-ref} with the string 'other'. In
this case the predicate function of [when]{.title-ref} returns
[False]{.title-ref} and [when]{.title-ref} will simply evaluate to [pure
()]{.title-ref}, which in the case of the continuation monad equals
[ContT (\$ ())]{.title-ref}.

::: {.code-include lexer="haskell" file="code/post59/bar14.hs"}
:::
Which we can simplify to

::: {.code-include lexer="haskell" file="code/post59/bar15.hs"}
:::
And now the main function can be evaluated

::: {.code-include lexer="haskell" file="code/post59/bar16.hs"}
:::
As can be seen we evaluated the inner compuatation of *callCC*'s
argument to the end and did not make any use of the early return.

### Key concept

It was probably not so easy to see the most key point in the above
series of transformations, so I want to take the chance to show it once
more. The argument to the argument of *callCC* ([k]{.title-ref} in our
case) has the following signature

``` {.sourceCode .haskell}
k :: a -> ContT r m b
```

So it takes a value of type [a]{.title-ref} and returns a continuation
monad transformer. In our case [k]{.title-ref} had the following
definition

``` {.sourceCode .haskell}
k = (\x -> ContT $ \_ -> c1 x)
```

where [c1]{.title-ref} was the actual continuation passed to *callCC*
and captured as a closure. Let's now assume [a]{.title-ref} is the type
[Int]{.title-ref} and that we have the following computation:

``` {.sourceCode .haskell}
computation = do
    compA
    ret <- callCC $ \k -> do
        compB
        compC
        k 42
        -- everything below here will be 'c2'
        compD
        compE
    -- everything below here will be 'c1'
    more1
    more2
```

Then we can also write this like that

``` {.sourceCode .haskell}
computation = do
    compA
    ret <- callCC $ \k -> do
        compB
        compC
        k 42 >>= (\n -> compF)
    -- everything below here will be 'c1'
    more1
    more2
  where
    -- This will be 'c2'
    compF = do
            compB
            compC
```

Using what we know about [k]{.title-ref} we get

``` {.sourceCode .haskell}
computation = do
    compA
    ret <- callCC $ \k -> do
        compB
        compC
        (ContT $ \_ -> c 42) >>= (\n -> compF)
    -- everything below here will be 'c1'
    more1
    more2
  where
    -- This will be 'c2'
    compF = do
            compB
            compC
```

Again using the definition of (\>\>=) we get

``` {.sourceCode .haskell}
computation = do
    compA
    -- here c1 comes to life
    ret <- callCC $ \k -> do
        compB
        compC
        ContT $ \c2 -> runContT (ContT $ \_ -> c1 42) (\x -> runContT ((\n -> compF) x) c2)
    -- everything below here will be 'c1'
    more1
    more2
  where
    -- This will be 'c2'
    compF = do
            compB
            compC
```

I marked for which part in the code [c1]{.title-ref} and
[c2]{.title-ref} pose the continuation. When we focus at the lengthy
term in the middle we can see that it is equal to

``` {.sourceCode .haskell}
ContT $ \c2 -> (\_ -> c1 42) (\x -> runContT ((\n -> compF) x) c2)
=
ContT $ \c2 -> c1 42
```

This shows that **no matter what follows after a call to k will be
ignored**. No matter how many [compX]{.title-ref} terms there are (they
could be arbitrarily many) and however complex [c2]{.title-ref} actually
is, we will use [c1]{.title-ref} as the continuation. And we've been
given [c1]{.title-ref} by *callCC.* The nice thing is that laziness
helps us to end up with efficient code, since we only evaluate terms
once we need their result. Thus [c2]{.title-ref} in our example (i.e.
[do compB; compC]{.title-ref}) will never be evaluated because we never
actually need it!