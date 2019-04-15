---
title:  "Do-notation for Python"
date: 2019-01-15
tags: python, monads, functional programming
category: Programming
authors: Tobias Pleyer
summary: "How to use do notation with monadic code in Python"
---

Do-notation for Python
======================

Recap
-----

In a [previous
post](./2018-12-28-Monadic-computations-in-Python.html) I introduced monadic
computations for Python. [In this
post](./2019-01-12-Monadic-computations-in-Python-revisited.html) I extended
the example of the original post. Make sure you read these posts first before
continuing with this one.

In the closing words of the original post I admitted that the given
example code looked a tiny but ugly and unreadable and explained the
lack of expressiveness with the lack of support for do-notation in
Python. The very last coding example of the aforementioned blog post
showed the example written in Haskell. The fact that Haskell's
do-notation spares the programmer from exessive use of parentheses and
other syntactic clutter is a real win.

Do-notation in Haskell is actually just syntactic sugar. A Haskell
compiler will rewrite computations given in do-notation using lambda
functions and the monad's bind (\>\>=) function.

So you can ask: If Haskell do-notation is just a facade, so to speak,
what should stop us from doing the same in Python? Nothing as it turns
out. Of course we don't want to modify Python's compiler, that would be
quite a lot of work, but we can write a function, [do]{.title-ref}, that
will do the trick (pun intended).

Do
--

### Understanding do-notation

Before we can start writing our [do]{.title-ref} function we have to
understand how do-notation actually works.

In total we have to deal with 4 cases:

-   A monadic computation with the result being ignored (sequence)
-   A monadic computation with the result not being ignored (bind)
-   A let-expression
-   A nested do-notation

Here is an example that makes use of all these cases:

::: {.code-include lexer="haskell"}
code/post58/do\_demo.hs
:::

``` {.sourceCode .bash}
$ sudo chmod +x do_demo.hs
$ ./do_demo.hs
How many gos do you want? 3
gogogo!
```

And here is the de-sugared (rewritten) version

::: {.code-include lexer="haskell"}
code/post58/do\_demo\_desugared.hs
:::

A lot harder to read, right? That's the reason do-notation was
introduced: To make monadic code nicer to write and read.

But as the simple example shows rewriting do-notation is not a hard
thing to do, it is very mechanical. But this means computers are really
good in doing it. Equipped with this knowledge we can start writing our
do-function.

### Test Driven Developement

But before we start coding away we want to follow good coding pratices.
One of them is to install a test suite **before** any code is written,
and then start coding with respect to the constraints of the given
tests. In this way we get live feedback about the quality and
correctness of our implementation. This technique is called
Test-Driven-Development, **TDD** in short.

Here is our Python script, with an empty do-function and the test case
to test it:

::: {.code-include lexer="python"}
code/post58/do\_initial.py
:::

As can be seen from the expectations, the output of [do]{.title-ref}
will be overly parenthesized. This doesn't hurt the reader/programmer,
because they will never see that code, but makes deterministic rewriting
much easier, because we don't have to think about precedence etc.

### Writing the do-function

If you look at the tests the expectation values are written very
verbose. This has three reasons:

-   It is easy to connect it visually to the input
-   It is easier to modify the test quickly
-   It already gives hint for the solution strategy

This last point is of special interest to us, because we want to
implement the function now. To make things easy we assume the following:

-   The do keyword has to be the last word of the line (nothing follows)
-   ...

We will follow this strategy, for now ignoring nested do:

1.  Define a class for a do-line
2.  Create rewrite rules for the do-line class
3.  Write a parser for a let expression
4.  Write a parser for a monadic computation with ignored result
5.  Write a parser for a monadic computation with the result bound to a
    variable
6.  Write a parser that can parse a line, no matter if let, sequence or
    bind
7.  Our do-function then has to
    -   Split the input in lines
    -   Filter out comments and empty lines
    -   Parse the lines to do-lines
    -   Make some assertions (e.g. no binds at the end)
    -   Desugar (rewrite) the do-lines
    -   Merge all strings to a final string
    -   Balance the open parentheses with closing ones

This should give us a string that can be evaluated with Python's
[eval]{.title-ref} function. The rest is then handled by the explicit
monadic code.

Our parsing framework will be pyparsing. Pyparsing provides a nice API
to build up parsers. I already compared pyparsing with attoparsec in
[this post](./2018-03-30-Comparing-Haskells-Attoparsec-with-Pythons-pyparsing.html).

#### The do-line class

The following class will be used to represent a basic do-line and check
off points 1 and 2.

::: {.code-include lexer="python"}
code/post58/do\_line.py
:::

As you can see the rewrite rules are very simple. Once we known the type
of the do-line and its components all we have to do is wrap the
components in parentheses and use the correct monadic function ((\>\>)
or (\>\>=)).

#### The do-line parsers

We will now work on points 3-6 of the above checklist. Here is our empty
test skeleton:

::: {.code-include lexer="python"}
code/post58/parsers\_initial.py
:::

Since the parsers are line based they are rather straight forward:

::: {.code-include lexer="python"}
code/post58/parsers\_final.py
:::

#### The implementation of the do-function

Now we can write the do-function. If again included the
[compose]{.title-ref} and [sequence]{.title-ref} functions from the
[extended
example](./2019-01-12-Monadic-computations-in-Python-revisited.html). With
these functions and a few simple helper functions we can basically
directly translate every bulletin point into a function composed
together to form the overall do-function:

::: {.code-include lexer="python"}
code/post58/do\_impl.py
:::

### The complete code

Here is the final version of our **do** implementation:

::: {.code-include lexer="python"}
code/post58/do\_final.py
:::

Using the do-function
---------------------

We are finally able to rewrite the last example of [the original
post](./2018-12-28-Monadic-computations-in-Python.html) post
using our newly won do-function. Compare it with the equivalent Haskell
code that was also given.

::: {.code-include lexer="python"}
code/post58/branching\_example.py
:::

There it is - **do** in Python. That's a big gain in readability! Note
that we have to use [eval]{.title-ref} to evaluate the code. We have
created our own little
[DSL](https://en.wikipedia.org/wiki/Domain-specific_language) that our
do-function translates (transcompiles) to Python source code that can be
evaluated by the Python interpreter.

Only one small drop of bitterness left: Because our code is a string, we
loose all the nice syntactic highlighting.
