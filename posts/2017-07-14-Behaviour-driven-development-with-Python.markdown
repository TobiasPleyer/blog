---
title:  "Behaviour driven development with Python"
date: 2017-07-14
tags: python, bdd
category: Programming
authors: Tobias Pleyer
summary: "Behaviour driven developement is a very popular development style. Python's package index has a few options to support that"
---

Behaviour driven development with Python
========================================

*BDD* with lettuce and Aloe
---------------------------

Recently I came into contact with *Behaviour Driven Developement*. As
the name already indicates, very similar to *Test Driven Development*,
you start development by defining how you would expect your program to
behave in certain conditions. That means the behaviour specification
comes first, only then you start to write code to satisfy the
behavioural needs.

Naturally this means in the beginning your tests typically always fail,
because you don't have the code yet. This leads to the natural behaviour
driven development cycle: Describe the program =\> Define steps =\> Run
tests =\> See them fail =\> Write code =\> Watch tests pass. Hear is a
very nice picture, taken from lettuce's home page, depicting the
situation:

![behaviour driven development cycle](../images/bdd_flow.png)

When I started to search about the topic on the internet, the situation
was a little bit confusing. From what I understood one of the first
frameworks, or at least sort of the reference implementation, for *BDD*
was [Cucumber](https://cucumber.io/). It is written in
[Ruby](https://www.ruby-lang.org/en/) and is very well adopted in the
Ruby community.

As is the usual case with Ruby and Python, if one has a nice library, it
doesn't take long until it is ported to the other language. Python's
port of Ruby's cucumber was called [lettuce](http://lettuce.it/). The
pun is obvious...

But lettuce isn't Python3 compatible and apparently the maintainers were
not willing to take the effort. So a lettuce spin-off was created, named
[Aloe](https://github.com/aloetesting/aloe). Since Python3 is the future
of Python, so to speak Aloe is the future of BDD in Python.

When I read about this my thoughts were as follows: *"Ok, I want my
project to be in Python3, so I have to choose Aloe."* So far so good. I
opened [Aloe\'s readthedocs](http://aloe.readthedocs.io/en/latest/) and
started with the obligatory *Getting started tutorial*. But to my
surprise the first run gave me this error:

``` {.sourceCode .python}
...
ImportError: No module named '_curses'
```

Well, ok. At this point I should mention I ran this on a Windows
machine. (N)curses is known to be home in the Unix universe, so no
surprise that there is no curses module. But why do I get this message?!
Well it appears the developers of Aloe decided to completely ignore the
Windows user world (for now). To my relief is saw that [I\'m not the
only one that realized
that](https://github.com/aloetesting/aloe/issues/119).
