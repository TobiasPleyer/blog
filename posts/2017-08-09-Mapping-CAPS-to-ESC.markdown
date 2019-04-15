---
title:  "Mapping CAPS to ESC"
date: 2017-08-09
tags: linux, xkb, vim
category: Programming
authors: Tobias Pleyer
summary: "Binding the caps lock key to the escape key increases productivity"
---

Mapping CAPS to ESC
===================

WHO NEEDS CAPS?
---------------

I am using vim in my daily editing. As a vim user it is a pain to switch
back from insert or visual mode, because this is done with the escape
key. Sure, it's not a big deal, just move your left hand a bit up and
reach to the sad lonely button far to the top left of your keyboard. You
could almost feel bad for that poor little fellow to be so alone...

But when your fingers are otherwise glued to the home row of your
keyboard, this constant pressing of the escape key really becomes
annoying. It just breaks your flow a bit. On the other hand there is the
caps lock key which directly next to your left pinky, usually exra big
and juicy to press. Just that noone ever presses it!

Long story short: just switch both keys or make the caps lock key
another escape key. The documentation and block posts about this can
sometimes be confusing, mostly because currently a lot of info is
filling the internet which is now considered deprecated. The simplest
solution I know is to add the following line to your shell's *rc* file,
*\~/.zshrc* in my case.

``` {.sourceCode .bash}
setxkbmap -option caps:escape
```

The command is pretty self explanatory. A bit more verbose it reads

*Set the X keyboard mapping option for the caps lock key to the escape
key*

This only takes effect when your shell is started for the first time,
but since I use vim in console mode (not gvim), this works just fine for
me.