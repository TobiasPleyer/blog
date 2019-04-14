Controlling the repository language on Github
=============================================

:date: 2018-03-26
:tags: github, linguist
:category: Programming
:authors: Tobias Pleyer
:summary: This post explains how to control Github's heuristics to determine
          a repositorie's language.


Background
----------

`In a previous post <{filename}/post43_chefkoch.rst>`_ I annouced the release of
version 1.0 of my Haskell application *chefkoch* `on github`_.

.. _on github: https://github.com/TobiasPleyer/chefkoch

The only problem I had: gihub thought it's HTML, or at least that was the
language displayed in the repository overview.

Solution
--------

Github uses the `linguist library`_ to determine the main language of a
repository. I don't know the internalas, but I guess in case of more than one
programming language being present in the repository the one with more lines of
code wins. Since I uploaded a few exemplary HTML files, this is probably the
reason why my repository was considered HTML.

.. _linguist library: https://github.com/github/linguist

Github provides a solution via a hidden file called *.gitattributes*. This file
is the place for many useful options that can be used to alter the behaviour of
github. One possiblity is to list directories which are not considered by
linguist in the process of determining the repo's language.

**Note:** This is not the same as ignoring the files via *.gitignore*, because
we definitely want to have these files in our repository!

Since I keep the HTML files in the directory *resources*, this one-liner was
enough to solve my problem:

.. code:: bash

    # .gitattributes
    resources/* linguist-vendored

Credits
-------

I found this solution on `stackoverflow`_ and `this blog post`_.

.. _stackoverflow: https://stackoverflow.com/questions/34713765/github-changes-repository-to-wrong-language
.. _this blog post: https://hackernoon.com/how-to-change-repo-language-in-github-c3e07819c5bb
