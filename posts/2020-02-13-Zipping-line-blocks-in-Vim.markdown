---
title: Interleaving line blocks in Vim
date: 2020-02-13
tags: vim, neovim
category: Programming
authors: Tobias Pleyer
summary: How to "interleave" two blocks of lines together
---

# Interleaving line blocks in Vim

In my day-to-day work I sometimes need to "interleave" several line blocks
together.  What do I mean by interleave? An example will explain it best:

```
-- block 1
a
b
c

-- block 2
x
y
z

-- block 1 and block 2 interleaved
a
x
b
y
c
z
```

## How to do that in (Neo)Vim

This can be done simply by using macros. Let's go back to our initial example
above and add line numbers so I can explain more clearly what I am doing

```
1  -- block 1
2  a
3  b
4  c
5  
6  -- block 2
7  x
8  y
9  z
```

If you want to have the lines of block 1 coming first, followed by a line of
block 2, here is how you do it:

- Jump/go to line 2 and set the mark a, keystrokes `ma` in Vim normal mode
- Jump/go to line 6 and set the mark b, keystrokes `mb` in Vim normal mode
- Now we can record the macro. Make the following keystrokes: `qa'bj:m
  'ajmaq` In words: *q*, *a*, *single quote*, *b*, *j*, *colon*, *space*,
  *single quote*, *a*, *enter*, *j*, *m*, *a*, *q*

`qa` starts to record a macro into register *a* (see :h q). Now every keystroke
will be recorded until pressing *q* again, which stops the recording.

**Note:** We are using the *move* command to move (see :h :m) one line after
the other from block 2 to block 1. If you want to copy them instead use `:t`.

**Note2:** You don't even have to type these keystrokes yourself. You can
actually just copy mine! Just copy them in some register with the following two
options:

- Vim normal mode command `:let @a="'bj:m 'ajma"` will copy the string
  content into register *a*.
- Selecting everything in visual mode and pressing `"ay` will yank it into
  register *a*

When you have recorded the macro or otherwise filled the contents of the
register you can simply execute the macro as often as there are lines. Let's
assume there are 15 lines, then in normal mode press `15@a` to execute the
macro 15 times, assuming you have saved your macro keystrokes to register *a.*

## Usage

One typical example would be some enum like data type, like this sum type in
Haskell:

```haskell
data MyType
  = A
  | B
  | C
```

Often we want to write a comment for each value, e.g from some table or other
external source and we can copy these into our code:

```haskell
data MyType
  = A
  | B
  | C

comment1
comment2
comment3
```

All we have to do now is bring the commentaries and the data constructors
together, but first the commentaries need to be commented out (easy with Vim's
visual block mode): 

```haskell
data MyType
  = A
  | B
  | C

  -- comment1
  -- comment2
  -- comment3
```

Now our macro can do the job. Mark the line `data MyType` with *a* and the
empty line above the first comment with *b* and execute it:

```haskell
data MyType
  -- comment1
  = A
  -- comment2
  | B
  -- comment3
  | C
```
