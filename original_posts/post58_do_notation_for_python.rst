Do-notation for Python
######################

:date: 2019-01-15
:tags: python, monads, functional programming
:category: Programming
:authors: Tobias Pleyer
:summary: How to use do notation with monadic code in Python


Recap
=====

In a `previous post <{filename}/post55_monadic_computations_in_python.rst>`_ I
introduced monadic computations for Python. `In this post
<{filename}/post55_monadic_computations_in_python.rst>`_ I extended the example
of the original post. Make sure you read these posts first before continuing
with this one.

In the closing words of the original post I admitted that the given example
code looked a tiny but ugly and unreadable and explained the lack of
expressiveness with the lack of support for do-notation in Python. The very
last coding example of the aforementioned blog post showed the example written
in Haskell. The fact that Haskell's do-notation spares the programmer from
exessive use of parentheses and other syntactic clutter is a real win.

Do-notation in Haskell is actually just syntactic sugar. A Haskell compiler
will rewrite computations given in do-notation using lambda functions and the
monad's bind (>>=) function.

So you can ask: If Haskell do-notation is just a facade, so to speak, what
should stop us from doing the same in Python? Nothing as it turns out. Of
course we don't want to modify Python's compiler, that would be quite a lot of
work, but we can write a function, `do`, that will do the trick (pun intended).

Do
==

Understanding do-notation
-------------------------

Before we can start writing our `do` function we have to understand how
do-notation actually works.

In total we have to deal with 4 cases:

* A monadic computation with the result being ignored (sequence)
* A monadic computation with the result not being ignored (bind)
* A let-expression
* A nested do-notation

Here is an example that makes use of all these cases:

.. code-include:: code/post58/do_demo.hs
    :lexer: haskell

.. code::

    $ sudo chmod +x do_demo.hs
    $ ./do_demo.hs
    How many gos do you want? 3
    gogogo!

And here is the de-sugared (rewritten) version

.. code-include:: code/post58/do_demo_desugared.hs
    :lexer: haskell

A lot harder to read, right? That's the reason do-notation was introduced: To
make monadic code nicer to write and read.

But as the simple example shows rewriting do-notation is not a hard thing to
do, it is very mechanical. But this means computers are really good in doing
it. Equipped with this knowledge we can start writing our do-function.

Test Driven Developement
------------------------

But before we start coding away we want to follow good coding pratices. One of
them is to install a test suite **before** any code is written, and then start
coding with respect to the constraints of the given tests. In this way we get
live feedback about the quality and correctness of our implementation. This
technique is called Test-Driven-Development, **TDD** in short.

Here is our Python script, with an empty do-function and the test case to test
it:

.. code-include:: code/post58/do_initial.py
    :lexer: python

As can be seen from the expectations, the output of `do` will be overly
parenthesized. This doesn't hurt the reader/programmer, because they will never
see that code, but makes deterministic rewriting much easier, because we don't
have to think about precedence etc.

Writing the do-function
-----------------------

If you look at the tests the expectation values are written very verbose. This
has three reasons:

* It is easy to connect it visually to the input
* It is easier to modify the test quickly
* It already gives hint for the solution strategy

This last point is of special interest to us, because we want to implement the
function now. To make things easy we assume the following:

* The do keyword has to be the last word of the line (nothing follows)
* ...

We will follow this strategy, for now ignoring nested do:

#. Define a class for a do-line
#. Create rewrite rules for the do-line class
#. Write a parser for a let expression
#. Write a parser for a monadic computation with ignored result
#. Write a parser for a monadic computation with the result bound to a variable
#. Write a parser that can parse a line, no matter if let, sequence or bind
#. Our do-function then has to

   * Split the input in lines
   * Filter out comments and empty lines
   * Parse the lines to do-lines
   * Make some assertions (e.g. no binds at the end)
   * Desugar (rewrite) the do-lines
   * Merge all strings to a final string
   * Balance the open parentheses with closing ones

This should give us a string that can be evaluated with Python's `eval`
function. The rest is then handled by the explicit monadic code.

Our parsing framework will be pyparsing. Pyparsing provides a nice API to build
up parsers. I already compared pyparsing with attoparsec in `this post
<{filename}/post46_attoparsec_pyparsing.rst>`_.

The do-line class
.................

The following class will be used to represent a basic do-line and check off
points 1 and 2.

.. code-include:: code/post58/do_line.py
    :lexer: python

As you can see the rewrite rules are very simple. Once we known the type of the
do-line and its components all we have to do is wrap the components in
parentheses and use the correct monadic function ((>>) or (>>=)).

The do-line parsers
...................

We will now work on points 3-6 of the above checklist. Here is our empty test
skeleton:

.. code-include:: code/post58/parsers_initial.py
    :lexer: python

Since the parsers are line based they are rather straight forward:

.. code-include:: code/post58/parsers_final.py
    :lexer: python

The implementation of the do-function
.....................................

Now we can write the do-function. If again included the `compose` and
`sequence` functions from the `extended example
<{filename}/post55_monadic_computations_in_python.rst>`_. With these functions
and a few simple helper functions we can basically directly translate every
bulletin point into a function composed together to form the overall
do-function:

.. code-include:: code/post58/do_impl.py
    :lexer: python

The complete code
-----------------

Here is the final version of our **do** implementation:

.. code-include:: code/post58/do_final.py
    :lexer: python

Using the do-function
=====================

We are finally able to rewrite the last example of `the original post
<{filename}/post55_monadic_computations_in_python.rst>`_ post using our newly
won do-function. Compare it with the equivalent Haskell code that was also
given.

.. code-include:: code/post58/branching_example.py
    :lexer: python

There it is - **do** in Python. That's a big gain in readability! Note that we
have to use `eval` to evaluate the code. We have created our own little `DSL`_
that our do-function translates (transcompiles) to Python source code that can
be evaluated by the Python interpreter.

.. _DSL: https://en.wikipedia.org/wiki/Domain-specific_language

Only one small drop of bitterness left: Because our code is a string, we loose
all the nice syntactic highlighting.
