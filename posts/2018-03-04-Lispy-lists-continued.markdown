---
title:  "Lispy lists continued"
date: 2018-03-04
tags: haskell, clojure
category: Programming
authors: Tobias Pleyer
summary: "Implementing the lispy lists Python implementation in real functional programming languages"
---

Lispy lists continued
=====================

[In a previous post](./2018-02-05-Lispy-lists-in-Python.html) a wrote
about data processing in Python using functional programming virtues
like explicit state and immutability. I also played around with a rather
unpythonic syntax using [list]{.title-ref}, [map]{.title-ref},
[filter]{.title-ref} and [zip]{.title-ref} and claimed it looks quite
*"lispy"*.

As a logical consequence I thought it would be fun to implement yonder
solution in real functional programming languages: Haskell and Clojure.
Closure even is a real Lisp dialect.

Haskell
-------

::: {.code-include lexer="haskell" file="code/post41/post41.hs"}
:::
As was the case in the original Python post, there exists an alternative
representation of the program. Haskell also does have support for list
comprehensions, very similar to those in Python. I am not sure, but I
think I even read somewhere that Python lend the list comprehension idea
from Haskell.

::: {.code-include lexer="haskell" file="code/post41/post41_alternative.hs"}
:::
**Note:** In my first draft [mkDouble]{.title-ref} was called
[mkFloat]{.title-ref} and I used [Float]{.title-ref} instead of
[Double]{.title-ref}. It turned out that this precision was not enough
to produce correct results. This is actually a flaw of the whole
"design" of this demo task. The whole data evaluation logic is way to
fragile regarding the floating point values of the data. Just changing
the threshold from [1e-4]{.title-ref} to [1.1e-4]{.title-ref} totally
changes the results. This, however, does not change the general ideas
presented in this and the previous post.

Clojure
-------

::: {.code-include lexer="clojure" file="code/post41/post41-clojure/src/post41_clojure/core.clj"}
:::