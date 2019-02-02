callCC in Haskell
#################

:date: 2019-02-02
:tags: haskell, continuation passing style, functional programming
:category: Programming
:authors: Tobias Pleyer
:summary: A short investigation of the callCC functions and how it works


Intro
=====

Continuation passing style is a very interesting programming concept. In
Haskell the we have the continuation monad `ContT`_ implemented in the
`transformers package`_. An introduction to the topic can be found in the `open
Haskell wiki book`_ and Gabriel Gonzalez wrote a `very nice article`_ about why
continuation passing style matters.

.. _ContT: https://hackage.haskell.org/package/transformers-0.5.5.0/docs/src/Control.Monad.Trans.Cont.html#ContT
.. _transformers package: https://hackage.haskell.org/package/transformers-0.5.5.0/docs/Control-Monad-Trans-Cont.html
.. _open Haskell wiki book: https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style
.. _very nice article: http://www.haskellforall.com/2012/12/the-continuation-monad.html

The wiki book article also mentions the *callCC* function. I think in general
the explanation and motivation for this function is conveyed well in this
article and the article is also not short on examples. The only thing I'm
missing in it is an example of a full evaluation involving *callCC*. I think
once you see how the function actually works things become much clearer. That
will be the goal of this article. I won't give any extra explanations, instead
I will take one of the examples from the article and do a full evaluation of
it.

callCC in action
================

**Note:** I will use the definitions from the transformers package, not from
the wiki book article.

Here is the full example we will evaluate:

.. code-include:: code/post59/bar.hs
    :lexer: haskell

We will transform the bar computation. Desugaring the outer do-notation yields

.. code-include:: code/post59/bar2.hs
    :lexer: haskell

Using the definition of the ContT monad's bind (>>=) function and giving the
anonymous function to callCC a name yields

.. code-include:: code/post59/bar3.hs
    :lexer: haskell

Now we use `the definition of callCC`_

.. _the definition of callCC: https://hackage.haskell.org/package/transformers-0.5.5.0/docs/src/Control.Monad.Trans.Cont.html#callCC

.. code-include:: code/post59/bar4.hs
    :lexer: haskell

Now we can also desugar the inner do computation, which yields

.. code-include:: code/post59/bar5.hs
    :lexer: haskell

Now we can fill in the definition of check, unpack the delayed continuation
with `runContT` and then apply the continuation to it

.. code-include:: code/post59/bar6.hs
    :lexer: haskell

Now we can substitute f by its definition and write the lengthy line in several
lines

.. code-include:: code/post59/bar7.hs
    :lexer: haskell

Once again using the definition ContT monad's bind (>>=) function we end up
with

.. code-include:: code/post59/bar8.hs
    :lexer: haskell

Unwrapping the inner `runContT ContT ...` layer and substituting for `when_f`
yields

.. code-include:: code/post59/bar9.hs
    :lexer: haskell

Now we are able to discuss the possible outcomes of the computation.

Calling bar with 'hello'
------------------------

Let's assume we call `bar` with the string 'hello'. In this case the predicate
function of `when` returns `True` and `when` will simply evaluate to its second
argument and we end up with

.. code-include:: code/post59/bar10.hs
    :lexer: haskell

Which we can simplify to

.. code-include:: code/post59/bar11.hs
    :lexer: haskell

If we cheat a bit we can rewrite it a bit further. The following won't compile,
because we are using variables out of scope, but it helps to see things a bit
better

.. code-include:: code/post59/bar12.hs
    :lexer: haskell

Now we can run the `main` function (again in a sort of pseudo code)

.. code-include:: code/post59/bar13.hs
    :lexer: haskell

They key point to note here is that `f` ignored its argument, thus it was
completely irrelevant what the actual value of `k` was. That is exactly the
"early return" behavior that *callCC* aims to provide. If you look way to the
beginning of the code transformation process you'll notice that the definition
of `f` came from the application of *callCC*.

Calling bar with 'other'
------------------------

Let's now assume we call `bar` with the string 'other'. In this case the
predicate function of `when` returns `False` and `when` will simply evaluate to
`pure ()`, which in the case of the continuation monad equals `ContT ($ ())`.

.. code-include:: code/post59/bar14.hs
    :lexer: haskell

Which we can simplify to

.. code-include:: code/post59/bar15.hs
    :lexer: haskell

And now the main function can be evaluated

.. code-include:: code/post59/bar16.hs
    :lexer: haskell

As can be seen we evaluated the inner compuatation of *callCC*'s argument to
the end and did not make any use of the early return.

Key concept
-----------

It was probably not so easy to see the most key point in the above series of
transformations, so I want to take the chance to show it once more. The argument
to the argument of *callCC* (`k` in our case) has the following signature

.. code:: haskell

    k :: a -> ContT r m b

So it takes a value of type `a` and returns a continuation monad transformer.
In our case `k` had the following definition

.. code:: haskell

    k = (\x -> ContT $ \_ -> c1 x)

where `c1` was the actual continuation passed to *callCC* and captured as a
closure. Let's now assume `a` is the type `Int` and that we have the following
computation:

.. code:: haskell

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

Then we can also write this like that

.. code:: haskell

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

Using what we know about `k` we get

.. code:: haskell

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

Again using the definition of (>>=) we get

.. code:: haskell

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

I marked for which part in the code `c1` and `c2` pose the continuation. When
we focus at the lengthy term in the middle we can see that it is equal to

.. code:: haskell

    ContT $ \c2 -> (\_ -> c1 42) (\x -> runContT ((\n -> compF) x) c2)
    =
    ContT $ \c2 -> c1 42

This shows that **no matter what follows after a call to k will be ignored**.
No matter how many `compX` terms there are (they could be arbitrarily many) and
however complex `c2` actually is, we will use `c1` as the continuation. And
we've been given `c1` by *callCC.* The nice thing is that laziness helps us to
end up with efficient code, since we only evaluate terms once we need their
result. Thus `c2` in our example (i.e. `do compB; compC`) will never be
evaluated because we never actually need it!
