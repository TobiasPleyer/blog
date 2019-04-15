---
title:  "Haskell: Comparing scalpel with parsec-tagsoup"
date: 2018-04-09
tags: haskell, parsec, tagsoup, scalpel, parsec-tagsoup
category: Programming
authors: Tobias Pleyer
summary: "A comparison between two HTML scrappers: scalpel and tagsoup-parsec."
---

Haskell: Comparing scalpel with parsec-tagsoup
==============================================

[Scalpel](#scalpel) and
[parsec-tagsoup](https://hackage.haskell.org/package/parsec-tagsoup) are
two options to extract contents from HTML files. Scalpel provides are
pretty high level interface via so called *selectors*, while tagsoup
parsec utilizes the [parsec](http://hackage.haskell.org/package/parsec)
parser combinator functions in combination with
[tagsoup](https://hackage.haskell.org/package/tagsoup).

Both of these packages allow one to extract data in between the
thousands of tags not of interest. As the name suggests, scalpel is very
good in cutting one very specific portions of interest, while
parsec-tagsoup gives you the absolute control how to handle every single
tag, at the cost of more boilerplate.

This short post will not go into detail how to use these two packages.
Instead we look at two constructed scenarios and how they can be solved
with both of the aforementioned packages.

Input
-----

The inputs are very basic stripped down plain HTML files.

[Input example 1](../code/post47/example)

[Input example 2](../code/post47/example2)

Scalpel
-------

### Example 1

::: {.code-include lexer="haskell"}
code/post47/with\_scalpel.hs
:::

### Example 2

::: {.code-include lexer="haskell"}
code/post47/with\_scalpel2.hs
:::

Parsec Tagsoup
--------------

### Example 1

::: {.code-include lexer="haskell"}
code/post47/with\_tparsec.hs
:::

### Example 2

::: {.code-include lexer="haskell"}
code/post47/with\_tparsec2.hs
:::

Conclusion
----------

Scalpel really shines when we want to extract data that shares common
attributes, possibly to be found scattered all over the HTML source. The
really nice part is that it alleviates the programmer from having to
write all the open/close tag boilerplate.

Parsec-tagsoup, backed up by the powerful parsec library, has its strong
sides when dealing with very detailed localized data, or when
conditional parsing is required.
