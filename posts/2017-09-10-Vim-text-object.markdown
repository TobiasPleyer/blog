---
title:  "Vim text object"
date: 2017-09-10
tags: vim
category: Programming
authors: Tobias Pleyer
summary: "Vim text objects are a handy thing to know"
---

Vim text object
===============

What are text objects?
----------------------

For quite a while now [Vim](https://vim.sourceforge.io/) is my text
editor of choice. Once accustomed to all its features Vim is an editor
hard to compete with. Even though I know a pretty substantial amount of
Vim's shortcuts by now, I had some kind of a "blind spot" - situations
where I felt there got to be a better way to do it, but didn't know how.

### A typical use case

You have a (longer) word that you want to change/delete/yank. Very often
your cursor is not at the beginning of the word, but commands like
**cw**/**dw**/**yw** will only do the trick if the cursor is at the
beginning of the word, otherwise just part of the word is going to be
affected.

This blind spot is covered by text objects. Text objects are a really
handy feature of Vim worth knowing about. Basically they offer some text
interpretation intelligence. This means Vim is able to tell what is a
**word**, a **paragraph** or **text enclosed by delimiters**. A good
introduction is given by [this
article](https://blog.carbonfive.com/2011/10/17/vim-text-objects-the-definitive-guide/).
Basically you have the key chars **i** for *inner* and **a** for *outer*
followed by a qualifier, e.g. **w** for word or **s** for *sentence*.

This means the shortcuts I was looking for are **ciw**, **diw** and
**yiw**.

Conclusion
----------

Text objects really help to select or change text faster, because Vim
helps you to apply logic to the text. This results in a boost of
productivity, even though you just save on a few keystrokes (this adds
up quickly). It is also possible to create your own text objects if you
have special needs.