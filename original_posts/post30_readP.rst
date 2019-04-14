Text.ParserCombinators.ReadP
############################

:date: 2017-11-29
:tags: parser, haskell, cps
:category: Programming
:authors: Tobias Pleyer
:summary: Some explanations and notes about the ReadP module


ReadP
=====

The ReadP module is part of Haskell's base package and it is a really
interesting piece of work. It covers so many aspects of Haskell: algebraic data
types, `laziness`_, `monads`_ and `continuation passing style`_.

.. _laziness: https://en.wikibooks.org/wiki/Haskell/Laziness
.. _monads: https://en.wikibooks.org/wiki/Haskell/Understanding_monads
.. _continuation passing style: https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style

Even though the module is quite short it provides all combinators to write
arbitrarily complex parsers. One of the uses of this module is the
implementation of *read* - reading a value out of a string.

However it is not so trivial to understand the implementation (at least not for
me) and that is the purpose of this blog post. This blog post will dig into how
this library works and serves as a reference to myself to look back to in the
future.

The types
=========

The ReadP module has 3 data types

.. code:: haskell

    type ReadS a = String -> [(a, String)]

    data P a
      = Get (Char -> P a)
      | Look (String -> P a)
      | Fail
      | Result a (P a)
      | Final [(a,String)]
      deriving Functor

    newtype ReadP a = R (forall b . (a -> P b) -> P b)

A variable of type *ReadS*, the **S** stands for *String* I suppose, is
basically a function that takes a string and returns you all possible
interpretations of it (interpretation means parser result of course). The
definition of ReadS is the typical type signature for a parser in Haskell.

Next up is *P*. Nowhere in the documentation of the source it is mentioned what
the P stands for, but I guess it stands for primitive, because all the
constructors of P represent a primitive operation of a parser:

* Get the next character of the input
* Look at the remaining input, but non-destructivly
* The parser must be able to fail
* The parser must be able to reach a result
* And finally a parser must reach a final state in order to be successful

*Get*, *Look* and *Result* are interesting, because they hold the information
**how to continue**. That is a very important first insight. This idea is the
heart of every parser: a parser is nothing more than a bunch of primitive
(step-wise) operations and rules that say how to continue based on the result
of the current step. For example, as we will see more below, *Get* is
responsible to consume one char from the remaining input and also holds the
function that decides what will be the next primitive operation based on the
current result. Likewise for *Look*. Result can be interpreted like a
milestone. We succeeded to parse part of the input and from there we try to
parse the remainder.

The last type is *ReadP*. The definition of *ReadP* is identical to the
definition of `ContT`_ and that is no coincidence. *ContT* stands for the
continuation monad transformer. A continuation is a computation that
explicitely receives the piece of code that will take the computation result
and executes based on its result.

.. _ContT: http://hackage.haskell.org/package/transformers/docs/src/Control-Monad-Trans-Cont.html#ContT

*ReadP* is exactly that: A fully assembled pipeline of primitive parser actions
that just needs the continuation to pass its result to. The net type of `ReadP
k`, where *k* is a continuation, is *P a*, i. e. a parser primitive. All of
this will hopefully become a bit clearer in a moment.

There exist 4 insights to enlightment.

Insight 1: The Monad definitions
================================

*P a* and *ReadP a* are both instances of *Monad*

.. code:: haskell

    instance Monad P where
      (Get f)      >>= k = Get (\c -> f c >>= k)
      (Look f)     >>= k = Look (\s -> f s >>= k)
      Fail         >>= _ = Fail
      (Result x p) >>= k = k x <|> (p >>= k)
      (Final r)    >>= k = final [ys' | (x,s) <- r, ys' <- run (k x) s]

      fail _ = Fail

    instance Monad ReadP where
      fail _    = R (\_ -> Fail)
      R m >>= f = R (\k -> m (\a -> let R m' = f a in m' k))

We can try to express the meaning of these definitions. The *bind* operation
for `Get f` can be written as `Get f >>= k = Get f'`. Since Get holds the
continuation to pass the next char to, this means we are changing the
continuation, i.e. the piece of code that consumes the next character and then
moves on. The behaviour of the new continuation is pretty straight forward:
Take the char, produce a new parser primitive (e.g. Fail) and *bind* (>>=) it
to k. *Look* works analogous. If we reach a *Fail* that means the parser did
not succeed, thus there is no need to continue parsing. *Fail* acts as a
shortcut identical to *Nothing* in the `Maybe monad`_. If we reach a *Result*
we fork the parser in order to follow both paths: the one continuing with p
followed by k and the one just with k.

.. _Maybe monad: http://hackage.haskell.org/package/base-4.10.0.0/docs/src/GHC.Base.html#line-729

The monad definition of *ReadP* is identical to the one for `ContT`_, for
reasons we mentioned above.

But what does the above definition actually mean?!

What makes the whole continuation passing style implementation so hard to grasp
is its upside-down nature compared to usual sequential code. But there is one
important thing to remember: **The continuation will receive the final
result**. That means it must come last. That is the reason why *k* appears in
the inner most paranthesis level and has to be tunneled through. The
implementation of `(m >>= f) k` explained in words is: *"perform the action m,
take its result, pass the result to f to calculate the new action to perform,
perform the new action and finally pass its value to the continuation k"*. `m
>>= f` thus a new action on its own, which needs an explicit continuation
parameter.

*ReadP* can be interpreted as a *'build rule'* for a parser, made up only of
primitive parser actions. The Monad implementation for ReadP states how two
build rules can be made into one, bigger build rule. This behaviour is the key
to build complicated parsers and many higher level functions such as *many*,
*choice*, etc.

Insight 2: The run function
===========================

So we know how to combine *ReadP* values. But what we really want is a parser, i.e.
a function that accepts a string as input and returns us all possible parses of
that string, what we want is *ReadS*.

Conveniently the ReadP library exports a function that does exactly that, and
its implementation is stunningly elegant

.. code:: haskell

    readP_to_S :: ReadP a -> ReadS a
    readP_to_S (R f) = run (f return)

All that *readP_to_S* does is provide the final continuation to the *ReadP*
primitive parser, which will result in a value of type *P a*, and then hand this
value over to the `run function`_.

.. _run function: http://hackage.haskell.org/package/base/docs/src/Text.ParserCombinators.ReadP.html#run

The *run* function knows how to handle the primitive parser instructions. Its
implementation is again stunningly concise

.. code:: haskell

    run :: P a -> ReadS a
    run (Get f)      (c:s) = run (f c) s
    run (Look f)     s     = run (f s) s
    run (Result x p) s     = (x,s) : run p s
    run (Final r)    _     = r
    run _            _     = []

Let's really quickly explain what each of the above pattern matches does

#. If run finds a Get, it pops one char off the remaining string and hands it
   over to the continuation held by the Get
#. If run sees a Look, it hands over the whole remaining input string to the
   continuation held by Look, but does not alter it
#. If run finds a Result, it prepends the result and the remaining input to
   the list of all found matches and then continues with the parser p held by
   the Result value
#. A Final value stops further evaluation and just returns the value wrapped by
   Final
#. Everything else, i.e. Fail, yields to an abort. An empty list is returned,
   which means no result, or no success respectively.

So to sum it all up: The *ReadP* Monad constructs the parser, while the run
function is the glue code that executes them.

Insight 3: Laziness
===================

If you try to think the above implementations through in your head, it very
quickly starts to become confusing. It is not recommended to deconstruct the
whole bind (>>=) chain of a *ReadP* do block. Same holds true for how run deals
with its input value. In fact the compiler does it neither. One of Haskell's
need features is laziness: values are only evaluated when they are needed.
Haskell builds and records the build rule (thunk) for a value, but postpones
the evaluation until is is really needed.

So in the above constructions Haskell just recors what it has to do with the
values but doesn't actually do anything with them until their results are
required.

Insight 4: Showcase
===================

Ok talk is cheap, let's get our hands dirty! The concepts make much more sense
once you see them on actual code.

I will exercise a full parser run. Here is our parser

.. code:: haskell

    my_parser = do
        c1 <- get
        char 'x'
        c2 <- get
        return [c1, c2]

And now let's *'run'* it with the input "axczzz"

.. code-include:: code/readP_parser_run_example.hs
    :lexer: haskell

Notice how the very first continuation, `\x -> Result x Fail`, remained
untouched throughout the whole transformation process. The actions were
performed sequentially within the imperative looking do block and only at the
very end the continuation was used to produce the final result.

Let's run some code. The output below is produced `by this script
<{filename}/code/readP_example.hs>`_.

.. code:: bash

    $ stack readP_example.hs axczzz
    ("ac","zzz")
    $ stack readP_example.hs ayczzz
    Parser failed
    $ stack readP_example.hs ax
    Parser failed
    $ stack readP_example.hs gxhParserSuccess
    ("gh","ParserSuccess")

Summary
=======

I really like the implementation of the *ReadP* library. The code is wonderfully
elegant and is very well suited to show and study all the nice stuff Haskell is known
for. The idea of starting with absolutely primitive operations (*Get, Look, Result,
Final* and *Fail*) and build an expressive fully featured parser library out of them
is really fascinating.

The concept of continuations is a very fascinating concept itself and worth knowing about.
In this blog post it was shown how they are used in practice and what it means to
make the further program flow explicit in a program.
