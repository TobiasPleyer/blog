---
title: Quick batch changes with Vim 
date: 2019-06-17
tags: vim, neovim
category: Programming
authors: Tobias Pleyer
summary: "This post is an example how to quickly use vim's scripting possibilities to manipulate text and perform repetitive tasks"
---

# Quick batch changes with Vim 

**Note:** Everything mentioned in this post is valid for
[Vim](https://www.vim.org/) and [Neovim](https://neovim.io/). Throughout this
post I will refer to both of them as *vim*. If I refer to Vim's own
documentation I will do some using the the form (`:h <some text>`), where `:h`
is a reference to Vim's built-in *help* command.

## Vim

Vim is a text editor and processor loaded with a very strong dual approach: on
the one hand provide the user with a very comprehensive set of commands and key
combinations to manipulate text and on the other hand to allow a fully
programmatic access to the text. Vim's power is that *editing* and *scripting*
have a fluent transition and the borders are fuzzy.

## Repetitive editing with Vim

Quite often we are faced with the task to generate text or source code that
follows are pattern. Most of the text repeats and only a small portion of the
text is dynamic, typically build from some input fed in.

Tasks of that nature beg to be automated to some degree. Depending on the
complexity of the task vim offers a couple of alternatives:

- Single repeats, aka the "dot formula" (`:h single-repeat`)
- Line based repeats (`:h multi-repeat`)
- Complex repeats, aka Macros (`:h complex-repeats`)
- Write a script (`:h using-scripts`)

In this post I want to give an example for the latter.

## The task

Let's take this very basic *C* program as our starting point.

::: {.code-include lexer="c" file="code/quick-vim-batch-changes/example.c"}
:::

We can compile and run it:

```bash
$ gcc example.c -o example
$ ./example
Sunday is the 7th day of the week
```

In the code we have defined a enumeration for the days of the week and in the
main function we provide a printf statement that will tell us which day of the
week Sunday is (yes my week starts on a Monday).

Our task is to provide a similar printf statement for every other day in the
enumeration.

**Note:** Obviously this task could be easily done with any of the
aforementioned options. But it is quite easy to conceive a much more elaborate
example.

## Script based solution

Our solution is made up of 6 steps:

#. Find the proper search pattern for the enumeration lines
#. Mark the enum lines we are interested in via marks
#. Mark the line where we want to put (insert) our generated code
#. Write the function and execute it
#. Apply the function

### Find the proper search pattern for the enumeration lines

this task is much easier when you have incremental search (`:h incsearch`)
activated. Vim will highlight the matches for you while you're typing, giving
instant feedback if the pattern does the job. Once we found a pattern we can
save it for later. You can paste it in a line by pressing `"/p`.

This pattern does the job: `^\s\+\(\w\+\) = \(\d\+\),\?$`

### Mark the enum lines we are interested in via marks

This is just for convenience and reproducability. No need to write perfect
code. We are about to write a function that gets the job done once.

Here is a set of vim commands that mark our enum lines between the marks *a*
and *b*.

```bash
gg
/Monday<CR>
:mark a
/Saturday<CR>
:mark b
```

### Mark the line where we want to put (insert) our generated code

I want to insert my code as the first line of the main function, so I mark the
line right above (with the open curly brace) with the mark *p* for put.

```bash
/int main/+1<CR>
:mark p
```

### Write the function and execute it

We have these options:

- Write the function in the *C* file itself, yank it and execute the register
  containing it (`:h @`). This is ok for small functions but can get messy soon
- Write the function in an unnamed buffer in Vim and load the contents of the
  buffer via the command sequence:
  `let @s = join(getbufline(<bufnr>, 1, '$'), "\n") | @s | call HelperFunc()`
  You must know the buffer number for this. Use the *ls* command (`:h ls`)
- Write the function to a file and source it (`:h source`)

I will go with the latter option and write the code in a file called
*script.vim*.

Here is the code for the function:

::: {.code-include lexer="vi" file="code/quick-vim-batch-changes/script.vim"}
:::

### Apply the function

All we have to do is execute this onelinerin vim's command line:
`source script.vim | call HelperFunc()`

**Note:** The pipe character is used to sequence commands in vim. It is not to
confuse with the shell's pipe command.

**Note2:** User defined functions that are globally available must start with
an uppercase letter.

## End result

Our main function should look like this now:

```c
int main()                                                                                          
{                                                                                                   
// Automatically generated variables                                                                
    enum WeekDay monday = Monday; // 1                                                              
    enum WeekDay tuesday = Tuesday; // 2                                                            
    enum WeekDay wednesday = Wednesday; // 3                                                        
    enum WeekDay thursday = Thursday; // 4                                                          
    enum WeekDay friday = Friday; // 5                                                              
    enum WeekDay saturday = Saturday; // 6                                                          
// Automatically generated print statements                                                         
    printf("Monday is the %i%s day of the week\n", (int)monday, get_quantifier(monday));            
    printf("Tuesday is the %i%s day of the week\n", (int)tuesday, get_quantifier(tuesday));         
    printf("Wednesday is the %i%s day of the week\n", (int)wednesday, get_quantifier(wednesday));   
    printf("Thursday is the %i%s day of the week\n", (int)thursday, get_quantifier(thursday));      
    printf("Friday is the %i%s day of the week\n", (int)friday, get_quantifier(friday));            
    printf("Saturday is the %i%s day of the week\n", (int)saturday, get_quantifier(saturday));      
// Autogen End                                                                                      
    enum WeekDay sunday = Sunday;                                                                   
    printf("Sunday is the %i%s day of the week\n", (int)sunday, get_quantifier(sunday));            
}     
```
