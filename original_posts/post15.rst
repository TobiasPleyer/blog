Patching the docutils rst parser
################################

:date: 2017-06-22
:tags: rst, python, docutils
:category: Programming
:authors: Tobias Pleyer
:summary: Small rst parser tweak for HTML generation

Tweaking the HTML generation of restructured text
=================================================

In an `older post <{filename}/post13.rst>`_ I described the problems I had with big pictures and what the possible solutions could be. Big, in this case, means pictures that are wider than the width my blog area has.

One of the options I mentioned was patching docutils by adding an additional option to the image directive and evaluate that option in the **HTMLTranslator**. Back then I wasn't sure if it was really that easy or if I was overlooking something. As it turned out it was that easy, at elast for my purposes. What follows is my patch in *diff* format between. The original version of docutils was 0.13.1. I applied this patch directly to the docutils package of my systems' Python installation.

.. code-include:: code/docutils_bigimage.diff
    :lexer: diff

From now on when I mark an image as "*scrollable*" the HTML *img* tag is enclosed by a *div* tag like so

.. code-include:: code/html_desired
    :lexer: text

If the image's width does not fit the blog's width, a vertical scrollbar appears.

Sum up
------

I was surprised that it was actually that easy to incorporate that change. I didn't have to read very deep into the sources. The image directive already has a bunch of options, so it was easy to follow the pattern. Currently the height of the image won't be limited, this could be changed in the future as a small improvement to above patch.
