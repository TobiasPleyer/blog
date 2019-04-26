---
title:  "Python Plugin for Neovim"
date: 2019-02-20
tags: vim, neovim, plugin, python
category: Programming
authors: Tobias Pleyer
summary: "How to write a simple text ID replacement plugin in Python for neovim"
---

Python Plugin for Neovim
========================

Intro
-----

I'm a big fan of [Vim](https://www.vim.org/) and use it as my editor of
choice. Recently I decided to make the switch to
[Neovim](https://neovim.io/).

At work we are dealing with a lot of texts in different languages. In
order to be able to translate the texts into the correct language the
source code does not contain the actual text, but a text ID which is
used as a lookup in a text database. this serves us well but
unfortunately we have no tooling around this text database. So everytime
when I browse the code and wonder what the text behind the ID is I have
to copy it, paste it into the query web interface of the database in the
browser and then wait for a while because this web interface is not very
fast for some reasons...

This is not all that bad but it tends to become a bit annoying,
especially because I have to leave my editor all the time and, even
worse, use the mouse :)

So I thought: "Hey, why not write a simple plugin for that?"

The Plugin
----------

Neovim makes it quite easy to integrate ordinary Python code as a
plugin. All you need is a python installation of your choice and then
install the neovim-rpc client library. In my case:

``` {.sourceCode .bash}
~$ python3.6 -m pip install pynvim
```

Afterwards you can write Python code that communicates with the Neovim
runtime via [msgpack](https://msgpack.org/) RPC calls. Here is the
relevant [Neovim remote plugin
documentation](https://neovim.io/doc/user/remote_plugin.html).

The procedure to write a Neovim Python plugin can be summarized like
this:

-   Install the pynvim module
-   Set the path to the correct Python executable in your init.vim
-   Write a Python module with a class that is decorated with
    \@pynvim.plugin
-   Within the class you can export methods as commands or functions to
    Neovim via the respective decorators
-   Save the file in a folder that is on Neovim's runtime path and
    conforms to Neovim's remote plugin folder policy
-   Once the code is ready you have to run :UpdateRemotePlugins within
    an nvim instance, so Neovim knows about it

I mentioned the remote plugin folder policy above. This means that for
every folder in the runtimepath of Neovim expects a certain hierarchy
for plugins and remote plugins. In the case of Python3 it searches
plugins under the path *rplugin/python3*. Speaking in my case

-   I'm using Linux
-   Under Linux the folder \$HOME/.config/nvim is Neovim's configuration
    folder and as such also on its runtimepath
-   I save the plugin as \$HOME/.config/nvim/rplugin/python3/textid.py

### The Plan

My plan is very simple:

-   Download the whole text database once and save it to a file (this
    takes a bit but is definitely doable)
-   Provide the filename as part of my Neovim configuration
-   At startup read the file from the plugin and safe it as a dictionary
-   The toggle between ID and text is done via simple regular expression
    replacement

**A small caveat:** Once a text ID has been translated it is simply
plain text. In this form it would be virtually impossible to translate
it back to a text ID, because we have to extract it from all the rest of
source code. So to make this easier I prepend and append two dashes
('--') to the text. This is not bullet proof of course but in my case
unusual enough that I can fund the text again later.

### The Code

Here is the full plugin code:

::: {.code-include lexer="python" file="code/post60/plugin.py"}
:::
And here are the relevant bits of my *init.vim* file:

::: {.code-include lexer="python" file="code/post60/init.vim"}
:::
Summary
-------

The presented plugin is nothing special or very robust. It is easy to
break it or bring it to its limits. But we have to keep in mind that
this is not production code. It is code for my personal needs in a very
narrow domain and I know I will use it the right way. The important
point is

> You can use the full power of Python within Neovim with just a few
> lines of code