---
title: Zipping line blocks in Vim
date: 2020-02-13
tags: vim, neovim
category: Programming
authors: Tobias Pleyer
summary: How to "zip" two blocks of lines together
---

# Zipping line blocks in Vim

This is a follow up post to my
[previous post](./2020-02-13-Interleaving-line-blocks-in-Vim.markdown). Besides
interleaving lines, zipping lines is also very useful:

```
-- block 1
a
b
c

-- block 2
x
y
z

-- block 1 and block 2 zipped
ax
by
cz
```

## Helper functions

Because it is tedious to re-create the macros all the time or do the
copy-pasting, I wrote a few simple functions that do the job. These functions
can be sources on demand, put in your *vimrc* file or even become a plugin (:h
write-plugin). Here comes the code, I hope the comments are enough of a
documentation...

```vim
" A line block is a number of consecutive lines and will be shown as a generic
" placeholder and the newline character inbetween: A\nB\nC
"
" This function interleaves to blocks with lines with each other, i.e. if we
" have the lines A\nB\nC and the lines X\nY\nZ, then interleaving them will
" yield A\nX\nB\nY\nC\nZ
"
" This function makes the following assumptions:
"       1. The beginning of the first block, i.e. its first line, is marked
"          witht he mark a
"       2. The line above the second block, i.e. above its first line, is
"          marked with b
"
" Executing the function will
"       1. Jump to mark b (the line above the second block)
"       2. Go down one line
"       3. Move this line below mark a, reducing the second block by one
"          line and increasing the first block
"       4. Set the mark a to this freshly moved line (deleting the old mark)
"
" The argument cnt determines how often you want to execute the macro,
" typically as often as there are lines in block 2
function! Interleave(cnt)
    let @m="'bj:m 'a^Mjma"
    execute "normal " . a:cnt . "@m"
endfunction

" The range based version of Interleave to interleave the block marked with a
" with the current selection. This function executes the same steps as
" Interleave, but doesn't assume any mark b is set beforehand. Instead it will
" use the line before the current selection as the mark b and then run the
" macro for every line in the selection.
function! InterleaveR() range
    let @m="'bj:m 'a^Mjma"
    if a:firstline == 1
        normal ggO
    endif
    call cursor(a:firstline-1, 0)
    normal mb
    let cnt = a:lastline - a:firstline + 1
    execute "normal " . cnt . "@m"
endfunction

" This function zips to blocks with lines with each other using the argument
" text to join both lines, i.e. if we have the lines A\nB\nC and the lines
" X\nY\nZ and the argument t, then zipping them will yield AtX\nBtY\nCtZ
"
" This function makes the following assumptions:
"       1. The beginning of the first block, i.e. its first line, is marked
"          witht he mark a
"       2. The line above the second block, i.e. above its first line, is
"          marked with b
"
" Executing the function will
"       1. Jump to mark b (the line above the second block)
"       2. Go down one line
"       3. Move this line below mark a, reducing the second block by one
"          line and increasing the first block
"       4. Append the argument text to the line of the first block
"       5. Join both lines together
"       6. Set the mark a to this freshly moved line (deleting the old mark)
"
" The argument cnt determines how often you want to execute the macro,
" typically as often as there are lines in block 2
function! ZipWith(text, cnt)
    let @t=a:text
    let @m="'bj:m 'a^Mk$\"tpgJjma"
    execute "normal " . a:cnt . "@m"
endfunction

" The range based version of ZipWith to zip the block marked with a with the
" current selection. This function executes the same steps as ZipWith, but
" doesn't assume any mark b is set beforehand. Instead it will use the line
" before the current selection as the mark b and then run the macro for every
" line in the selection.
function! ZipWithR(text) range
    let @t=a:text
    let @m="'bj:m 'a^M$\"tpgJjma"
    if a:firstline == 1
        normal ggO
    endif
    call cursor(a:firstline-1, 0)
    normal mb
    let cnt = a:lastline - a:firstline + 1
    execute "normal " . cnt . "@m"
endfunction
```

**Note:** The `^M` above is actually one character an represents pressing the
enter key. If you record the macro yourself this will be handled automatically,
if you try to copy the macro from me you have to escape the key by pressing
CTRL-V+Enter. Read this
[stackoverflow post](https://stackoverflow.com/questions/2943555/how-to-save-a-vim-macro-that-contains-escape-key-presses)
for more info.

## Usage example

One typical example would be matching enums with their values. Here's an
examplary enum in Haskell and a function using it

```haskell
data MyType
  = A
  | B
  | C

f :: MyType -> Int
f A
f B
f C
```

Now we paste in the values from somewhere else, maybe a data table...

```haskell
data MyType
  = A
  | B
  | C

f :: MyType -> Int
f A
f B
f C

1
2
3
```

Now we can use a zip. Mark the line `= A` with *a* and the empty line above `1`
with *b* and execute the following ex command: `:call ZipWith(" = ", 3)`

```haskell
data MyType
  = A
  | B
  | C

f :: MyType -> Int
f A = 1
f B = 2
f C = 3

```
