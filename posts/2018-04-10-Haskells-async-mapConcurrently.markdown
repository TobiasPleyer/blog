---
title:  "Haskell's async.mapConcurrently"
date: 2018-04-10
tags: haskell, async
category: Programming
authors: Tobias Pleyer
summary: "Another code walk, this time for the mapConcurrently function of Haskell's async package"
---

Haskell's async.mapConcurrently
===============================

Recently I parallelized
[chefkoch](https://github.com/TobiasPleyer/chefkoch), so multiple URL
fetches would happen simultaneously. I used Haskell's [async
package](http://hackage.haskell.org/package/async) to achieve this. I
only used one function, [mapConcurrently]{.title-ref}, which allows you
to run an **IO** function in parallel over a list of values and then
collect the results back in a list. The signature looks like this

``` {.sourceCode .haskell}
mapConcurrently :: Traversable t => (a -> IO b) -> t a -> IO (t b)
```

Awesomely enough its implementation is also an one-liner, and quite
readable actually

``` {.sourceCode .haskell}
mapConcurrently f = runConcurrently . traverse (Concurrently . f)
```

I wanted to use this implementation as the motive for this post and
another code run. I think it shows the beauty and elegance that
functional programming with Haskell can offer. Before we dive into the
implementation and walk through it I want to give a few remarks

> 1.  The implementation makes use of the **Traversable** type class. I
>     will specialize this for lists, which are an instance of
>     **Traversable**
> 2.  The monad we are dealing with is the **IO** monad. Typically the
>     IO monad is opaque, hiding its internals. As a consequence in
>     normal Haskell code you can't deconstruct IO values. For a better
>     visualization I assume the following: Applying the function
>     myAction to the value dataX will result in the IO action IO resX,
>     i.e. will have the value resX when extracted from the IO action.

With these assumptions in place let's "run" some code:

::: {.code-include lexer="haskell"}
code/post48/mapConcurrently.hs
:::

Comments
--------

Thanks to [traverse]{.title-ref} the implementation of
[mapConcurrently]{.title-ref} almost comes naturally. Here is the quote
from the documentation of traverse:

> Map each element of a structure to an action, evaluate these actions
> from left to right, and collect the results.

But this gives us only the collection of the results, not automatically
concurrency, or in this case parallelism. This comes from the
[Concurrently]{.title-ref} type class, more precisely the
[Applicative]{.title-ref} instance of [Concurrently]{.title-ref}. But
let's first look at the definition of [Concurrently]{.title-ref} and its
[Functor]{.title-ref} implementation:

``` {.sourceCode .haskell}
newtype Concurrently a = Concurrently { runConcurrently :: IO a }

instance Functor Concurrently where
  fmap f (Concurrently a) = Concurrently $ f <$> a
```

So a value of type [Concurrently]{.title-ref} is just a thin wrapper
around an [IO]{.title-ref} action. The Functor implementation says that
applying a function to a value of a concurrent action just applies the
function to the IO action and then runs it concurrently.

Now let's move to the [Applicative]{.title-ref} implementation:

``` {.sourceCode .haskell}
instance Applicative Concurrently where
  pure = Concurrently . return
  Concurrently fs <*> Concurrently as =
    Concurrently $ (\(f, a) -> f a) <$> concurrently fs as
```

So a pure concurrent action is an action which returns immediately, no
work done. Isn't it wonderful how close this comes to what we would
think of a "pure" action? But now to the central part: the
implementation of [(\<\*\>)]{.title-ref}. The implementation makes use
of the [concurrently]{.title-ref} primitive. It is not really a
primitive, it is a normal function which runs two concurrent actions and
returns both their results as a tuple. But I call it primitive because
it is the smallest interaction unit: it runs exactly two IO actions
concurrently, which is smallest in the sense that the next smallest
thing, running one action, is already not concurrency anymore.

In this sense we can formulate the implementation of
[(\<\*\>)]{.title-ref} as follows:

> If we have a cocncurrent IO action which produces a function, and a
> concurrent IO action which produces a value, then the effect of
> applying this function to the value concurrently is to run both
> actions concurrently and then apply the resulting function to the
> value.

When you write it out like that it really sounds like the most obvious
thing to do...

But the nice part is that this gives us enough to run an arbitrary
amount of concurrent computations, because the Concurrent class composes
and one or both of the arguemnts to [(\<\*\>)]{.title-ref} can consists
of more concurrent computations. This is exactly what we can observe in
the bottom part of the above code transformations: When we have
decomposed everything into one big [Concurrently]{.title-ref} value and
then unpacked it, all that was left was a nested call chain of
[concurrently]{.title-ref} function calls. Quite beautiful indeed!