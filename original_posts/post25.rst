Comply to the 80 characters text width standard
###############################################

:date: 2017-09-10
:tags: vim format
:category: Programming
:authors: Tobias Pleyer
:summary: My current approach to the 80 characters text width standard in the Vim editor

Applying the formatter
======================

From the very early days of computer science we inherited the standard of
maximum 80 characters per line. Back then that limit was mandatory. Nowadays it
has been agreed upon that 80 character line widths are a good standard to keep.

But how do we keep it? In Vim there are several options. It's possible to force
the line break or to just visually comply to it via text wrapping. But I prefer
to write my text as I want and apply a formatter afterwards if needed, this way
I maintain full control. As a visual indicator for the 80 character width I
added the following line to my *vimrc.local* file

*set colorcolumn=80*

Vim is capable to reformat text selections. Vim can either internally handle
the formatting or hand the text over to an external program to decide what to
do and take its output as the replacement text. To apply a formatter to text
Vim has the **gq** command. Either select some text with visual mode and press
*gq* or combine *gq* with the usual Vim motion commands. I am not sure if this
is the standard case but at least in my Vim configuration pressing *<Enter>* in
normal mode selects the whole paragraph of the current cursor location. Thus in
order to format the current paragraph to comply to the 80 character standard
(which should be the default if you haven't changed it) simply type

*<Enter>gq*
