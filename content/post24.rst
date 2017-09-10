How I currently use Vim
#######################

:date: 2017-09-10
:tags: vim spf13
:category: Programming
:authors: Tobias Pleyer
:summary: Vim is powerful right out of the box, but becomes even more powerful with the right configuration

Plain vanilla Vim
=================

I try to use `Vim`_ as much as possible. Sometimes I'm catching myself to just
wish for a simple editor like *gedit*, but then I quickly start to miss the
killer features of Vim. Using Vim can be tiring, just because it is so much
than your average editor, but don't give up - it pays off!

.. _Vim: https://en.wikipedia.org/wiki/Vim_(text_editor)

But using Vim out of the box is one thing, using it with plugins and a really
nice configuration file is a whole different story!

How I have configured my Vim
============================

Configuring Vim can be challenging for many reasons:

* You have to know a lot of plugins
* You have find a way to arrange/define your shortcuts with the least amount
  of overlaps
* You have to learn the syntax

All of this is manageable, but why not benefit from the work of others on the
internet? On one of my occasional roamings through the web I found `spf13`_.
Spf13 was created by Steve Francia and offers a really thorough collection of
plugins, accompanied by a well thought through configuration file. In addition
a simple overwrite and precendence mechanism allows for further customization.

.. _spf13: http://vim.spf13.com/

I use spf13 almost unchanged, the only thing I did is install two more plugins
and use a handful more configuration items. Additional configuration is made
possible by the spf13 distribution via *local* files, these are files with
**.local** appended to the name of the orignal file names. Anything written in
there overwrites the default configuration delivered with spf13. Here are my
*vimrc.bundles.local* file and my *vimrc.local* file.

vimrc.bundles.local
-------------------

.. code-include:: code/bundles_local
    :lexer: vim

vimrc.local
-----------

.. code-include:: code/vimrc_local
    :lexer: vim

Conclusion
==========

If you want to use the full power of Vim but don't have the energy or devotion
to write a comprehensive configuration yourself, then *spf13* is definitely
worth checking out. I takes time to get used to this **massive** amount of
shortcuts and plugins, but it's worth it.
