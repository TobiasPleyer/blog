---
title: Change directory and list files in one go
date: 2020-01-18
tags:
category: Programming
authors: Tobias Pleyer
summary: Save on keystrokes by combining the effects of `cd` and `ls` in the terminal
---

# Change directory and list files in one go

I am using the terminal a lot, both at work and at home. I would even go so far
to say that I'm a terminal power user. When you use the terminal for a while
there are two commands that will constantly pop up: `cd` (change directory) and
`ls` (list files in directory). 9 out of 10 times when I change into a
directory with `cd` I follow that command with `ls` to see what I have in this
directory. After a while it becomes annoying to type both commands in sequence,
and since programmers like to save on keystrokes: let's make a new command that
gives us 2in1. I call it `cs` as a mnemonic of **c**d and l**s**.

To have this command available all you have to do is add the following to your
shell's .rc file (.zshrc in my case):

```bash
cs = function()
{
    cd $1
    ls
}
```

From that point on every new shell will have this command (function) available.
In the current shell you have to source the config file once more:

```bash
$ cs

Command 'cs' not found, but can be installed with:

sudo apt install csound

# You have to adapt the file path to your system's
$ source ~/.zshrc
$ cs ~
bin        Downloads  Pictures  Templates
Desktop    Github     Public    Videos
Documents  Gitlab     Music
```

### Trivia

Initially I used `cl` as the command's name, but I found `cs` to be easier to
type and it is also the closest to `cd` on the keyboard.
