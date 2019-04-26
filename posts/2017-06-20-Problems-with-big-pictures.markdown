---
title:  "Problems with big pictures"
date: 2017-06-20
tags: blog
category: Programming
authors: Tobias Pleyer
summary: "How to handle big pictures with restructured text"
---

Problems with big pictures
==========================

Big pictures in my blog
-----------------------

### The problem

In my [previous post](./2017-06-19-Helpfullness-of-Jenkins-error-messages.html) I encountered kind of
an annoying problem. The picture shown in this post is pretty big and
wider as my blog's width. The problem was that the picture got cut off
at the right border. I tried to fix the problem, but as far as I could
see that problem lies actually deeper than I originally thought. Maybe
I'm wrong, I am really not an expert for restructured text and Python's
docutils. In the following I will elaborate a bit further.

### How it works

I am writting my blog posts in plain restructured text files. How do
they become a website? The answer is:
[Pelican](https://blog.getpelican.com/) handles that. But what does
Pelican do behind the scenes?

Pelican delegates most of the work to docutils and its rst parser. The
translation from restructured text format to HTML is done by the
[HTMLTranslator
class](http://epydoc.sourceforge.net/docutils/private/docutils.writers.html4css1.HTMLTranslator-class.html).
The HTMLTranslator class translates the my documents to valid HTML and
returns that to the Pelican runtime. Pelican then inserts that HTML into
its [Jinja](http://jinja.pocoo.org/) templates:

::: {.code-include lexer="text" file="code/article_template"}
:::
The *article.content* part comes from docutils. That is more or less the
problem. The possibilities to manipulate the content are relatively
limited. I narrowed it down to more or less these options:

> -   Patch the rst image directive to accept one additional argument
>     and modifiy the *visit\_image* function of the HTMLTranslator
>     class
> -   Do a regex replace within the Jinja template engine
> -   Do a regex replace after the template has been generated

Especially the last option is unsatisfying because it doomes you to hard
code hidden fixes somewhere. The first option is probably the best and
most general one, but it will mean nobody else will be able to generate
my article unless they have my patched version of docutils.

### Desired result

I don't have a solution yet, but I already know how the end result
should look like. The relevant part of the current version of the
generated HTML file looks like this:

::: {.code-include lexer="text" file="code/html_current"}
:::
But it should look like this

::: {.code-include lexer="text" file="code/html_desired"}
:::
This will result in the picture having scroll bars at the sides. The
current workaround I use is to assign the above style attributes to the
*entry-content* style class in my *main.css*. This causes the whole
article to have scroll bars, which looks pretty ugly but does the job
with minimum effort.

I hope I can come back to this problem with a more satisfying solution.
