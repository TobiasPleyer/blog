HTML extraction in 4 languages
==============================

:date: 2018-04-19
:tags: haskell, clojure, python, go, html, scraping
:category: Programming
:authors: Tobias Pleyer
:summary: 1 Task, 4 languages: Scraping HTML in Haskell, Clojure, Python and Go


In this post I want to present the same task, solved four times in four
different languages.

**The task:** Parse the front page of the `New York Times`_ and extract all
main articles with their title, the author name(s) and the URL to the full
article. If author name or URL can't be extracted we skip this article.

.. _New York Times: https://www.nytimes.com

**The contenders:**

* Haskell: Static functional language with lazy evaluation
* Clojure: Dynamic functional language with persistent data types
* Python: Dynamic, duck-typed and interpreted language
* Go: Static procedural language

Haskell
-------

.. code-include:: code/post50/nyt.hs
    :lexer: haskell

Clojure
-------

.. code-include:: code/post50/nyt.clj
    :lexer: clojure

Python
------

.. code-include:: code/post50/nyt.py
    :lexer: python

Go
--

.. code-include:: code/post50/nyt.go
    :lexer: go

Remarks
-------

As in other similar posts before I beg to consider that I am not equally good
in all four languages. Furthermore the size and elegance of each solution is
strongly influenced by the choice of framework. I tried to choose the most
popular framework for every language or that I have the most experience with.

The amount of lines, not considering comments, blanks or import statements, is
fairly similar in all four cases. However there are a couple of differences,
some suttle, some very aparent.

Notice how the Haskell solution is the only one that doesn't require filtering
the articles for empty values. This is silently handled by the monad
implementation of the scraper, with *Maybe* behind the scenes.

From all four solutions the Python solution is the least satisfying for me.
This is largely due to the fact that BeautifulSoup does not provide easy
nesting of criteria as in the other solutions. This leads to a lot of manual
error checking boilerplate (see all the ifs in extract). Even worse the list
flattening list comprehension in the return statement, this will scale horribly
with more complicated scraping demands. Python also has a jQuery like library,
so using it probably would have let to similar code like in the Go version,
which uses goquery, but I wanted to use BeautifulSoup because of its popularity
within the Python community.
