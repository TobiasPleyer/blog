---
title:  "Different Angles"
date: 2017-06-03
tags: rust, haskell, python, c
category: Programming
authors: Tobias Pleyer
summary: "Tackeling the same programming task with different languages"
---

Different Angles
================

Many roads lead to Rome
-----------------------

If you happen to know more than one programming language it can be a fun
challange to solve the same programming task with more than one
programming language. It can be both, a mental challenge and a chance to
look differently at the same thing.

The Task
--------

We have the following situation: A folder with subfolders with
subfolders. What we want is to iterate over all the subfolders of every
subfolder of the original folder and only keep the *N* newest, the
others shall be deleted.

In my concrete real life situation the subfolders were Git branches and
the subsubfolders were the commits of these branches.

The contestents will be: Python, C, Rust and Haskell

**Disclaimer:** I do not have equal skill levels for each of these
languages, so it can be that my solutions are far from ideal in some
cases. But that is not part of the challenge. We won't look at speed,
performance, memory footprint or alike. The only criterion is that the
implementation does the job. Error handling is optional.

Solutions
---------

### Python

::: {.code-include lexer="python" file="code/unstage.py"}
:::
### C-Code

::: {.code-include lexer="c" file="code/unstage.c"}
:::
### Rust

::: {.code-include lexer="rust" file="code/unstage.rs"}
:::
### Haskell

::: {.code-include lexer="haskell" file="code/unstage.hs"}
:::
Conclusion
----------

The code snippets shown above should not be directly compared. The focus
was on getting the job done. The maturity of the different versions is
not identical. Differences can be found in argument handling, output and
most significantly error handling. Many things are hard-coded.

Error handling wise the *Rust* version is the most complete. All kinds
of things that could possibly go wrong are taken care of and the program
will finish gracefully with a meaningfull error message. Thus the
lengths of the programming snippets shouldn't be taken too serious.

Yet it is obvious that different languages require a different level of
detail from the programmer. As an example, *Rust* exposes all the
possibilities of failure to the programmer due to the need of error
handling, while somebody coding in Python probably is never aware of the
fact that those kinda things could happen. In *C* many things need to be
explicitly provided, while Python comes in its typical "batteries
included".

The choice of programming language, if one has the choice, should depend
on what is aked from the application. Speed, reliability or fast
development are some of the key factors. Ot maybe just for fun :)