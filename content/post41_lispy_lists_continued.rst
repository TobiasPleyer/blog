Lispy lists continued
#####################

:date: 2018-03-04
:tags: haskell, clojure
:category: Programming
:authors: Tobias Pleyer
:summary: Implementing the lispy lists Python implementation in real functional
          programming languages


`In a previous post <{filename}/post38_lispy_lists.rst>`_ a wrote about data
processing in Python using functional programming virtues like explicit state
and immutability. I also played around with a rather unpythonic syntax using
`list`, `map`, `filter` and `zip` and claimed it looks quite *"lispy"*.

As a logical consequence I thought it would be fun to implement yonder solution
in real functional programming languages, one of them even being a Lisp:
Haskell and Clojure.

Haskell
=======

.. code-include:: code/post41.hs
    :lexer: haskell

As was the case in the original Python post, there exists an alternative
representation of the program. Haskell also does have support for list
comprehensions, very similar to those in Python. I am not sure, but I think I
even read somewhere that Python lend the list comprehension idea from Haskell.

.. code-include:: code/post41_alternative.hs
    :lexer: haskell
