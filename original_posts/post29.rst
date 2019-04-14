Lexical Scanning in Go
######################

:date: 2017-10-18
:tags: go, lexer, parser, python
:category: Programming
:authors: Tobias Pleyer
:summary: Me playing around with a lexical scanner in Go


Intro
=====

The content and motivation of this talk is based on a talk by Rob Pike I
stumbled upon on `youtube`_. I also found the `slides`_. I also found a `blog
post`_ by Eli Bendersky taking the aforementioned talk as a basis for a
solution in Python.

.. _youtube: https://www.youtube.com/watch?v=HxaD_trXwRE
.. _slides: https://talks.golang.org/2011/lex.slide#1
.. _blog post: https://eli.thegreenplace.net/2012/08/09/using-sub-generators-for-lexical-scanning-in-python/

My shot at this
===============

So I watched the video and that it was a kinda cool idea, also as a chance to
highlight Go's features nicely. But I didn't want to dig further for the source
code nor was I particularily interested in the exact syntax and semantics of
the template engine mentioned in the talk. What I was going for instead was a
very simple toy implementation of a potential template engine very loosly
influenced from what I saw on the slides, just to get a feel for the idea and
Go itself.

The template engine
-------------------

So here is a description of the toy template eninge:

* Variable content, as in the talk, are kept between `{{` and `}}`
* An *"action"* inside these meta characters can have only one of these forms

    - `.ident`
    - `.ident!mod`
    - `.guard? .ident`
    - `.guard? .ident!mod`

* ident stands for **identifier**, mod for **modifier** and guard for a
  **guard expression**
* The identifier is a lookup string for a map object called the **context**. It
  is used to find the text that has to be substituted for the identifier
* The guard is a string that, if it reads "true" causes the meta action to be
  evaluated, otherwise it is ignored
* The modifier can be used to modifiy the string that is specified by ident, in
  this simple implementation the only supported modifiers are **lower** and
  **upper**
* The renderer of the template engine will accept a file path specifying the
  template, another file path specifying the output file and at last a context
  (Go map) holding all the variables to look up
* Everything else is interpreted as text and copied as is to the rendered
  document

The implementation
------------------

So without much more talking here comes the code. As already mentioned above a
lot of it is based on Rob Pike's work. I just filled in some gaps here and
there to make the the lexer fully working. In the end follows a little script
that tests the library.

`lexer.go <{filename}/code/lexer.go>`_

`parser.go <{filename}/code/parser.go>`_

`template_test.go <{filename}/code/template_test.go>`_

`template.txt <{filename}/code/template.txt>`_

The output
----------

.. code:: bash

    ./template_test /path/to/template.txt path/to/out.txt && cat path/to/out.txt
    Starting to lex
    path/to/template.txt
    (Text,Hello )
    (LeftMeta,{{)
    (Operator,.)
    (Identifier,print)
    (Operator,?)
    (Operator,.)
    (Identifier,who)
    (Operator,!)
    (Identifier,upper)
    (RightMeta,}})
    (Text,, from )
    (LeftMeta,{{)
    (Operator,.)
    (Identifier,print)
    (Operator,?)
    (Operator,.)
    (Identifier,name)
    (Operator,!)
    (Identifier,lower)
    (RightMeta,}})
    (Text,!
    )
    Finished lexing...
    Rendering template now to
    path/to/out.txt
    Hello WORLD, from lexer!
