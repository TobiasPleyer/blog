Bash autocomplete
#################

:date: 2017-04-17
:tags: bash, linux
:category: Programming
:authors: Tobias Pleyer
:summary: Configuration interface to Bash's autocomplete feature

Motivation
==========

Recently I worked with Git in my bash shell and when I typed *git stat<TAB>* and magically the words *git status* appeared I thought to myself: *"How does this actually work?"*

Autocomplete with Bash
======================

One of the very handy features of Bash is its autocompletion feature. Pressing tab at the prompt will give suggestions depending on the current context. Bash uses the `readline library`_ to provide the prompt editing features. But I don't want to speak about the internals and the readline implementation, because Bash actually provides a more controlled configuration interface for its autocompletion features.

.. _readline library: https://cnswww.cns.cwru.edu/php/chet/readline/rltop.html

Programmable Completion
-----------------------

The Bash documentation refers to this feature as `Programmable Completion`_. Bash provides three functions to handle most of the bulk work to make autocompletion available: *compgen*, *complete* and *compopt*.

.. _Programmable Completion: https://www.gnu.org/software/bash/manual/bash.html#Programmable-Completion

The *complete* function registers completer functions (often using *compgen*) for every command that should have autocompletion functionality. In order to register the function *foo_completer* to assist in autocompleting the command *foo* we would type

.. code-block:: bash

    ~$ complete -F foo_completer foo

`Part 1`_ and `Part 2`_ of this article give a very nice and concise explanation of the ongoings.

.. _Part 1: https://debian-administration.org/article/316/An_introduction_to_bash_completion_part_1
.. _Part 2: https://debian-administration.org/article/317/An_introduction_to_bash_completion_part_2

Programmable Completion for Git
-------------------------------

I just want to give the concrete example in case of Git on my PC. Most likely the mentioned paths differ for other distros. On my PC this is the default installation and location behaviour.

The Bash completion functions are searched in the file */etc/bash_completion*. In my case this file is only sourcing the file */usr/share/bash_completion*. In this file a lot of autocompletion for common command line commands like *cd* are initialized.

The completion for Git are located under */usr/share/bash-completion/completions/git*. This file is loaded with helper functions, but almost at the bottom comes the crucial part.

.. code-block:: bash
    :linenos: inline
    :linenostart: 2742

    # Setup completion for certain functions defined above by setting common
    # variables and workarounds.
    # This is NOT a public function; use at your own risk.
    __git_complete ()
    {
	    local wrapper="__git_wrap${2}"
	    eval "$wrapper () { __git_func_wrap $2 ; }"
	    complete -o bashdefault -o default -o nospace -F $wrapper $1 2>/dev/null \
		    || complete -o default -o nospace -F $wrapper $1
    }

    # wrapper for backwards compatibility
    _git ()
    {
	    __git_wrap__git_main
    }

    # wrapper for backwards compatibility
    _gitk ()
    {
	    __git_wrap__gitk_main
    }

    __git_complete git __git_main
    __git_complete gitk __gitk_main

Line 2749 does the actual registration for the autocompletion of the *git* command.

Location best practice
----------------------

Above mentioned locations are the locations for system installation. Custom user completion files should be saved under */etc/bash_completion.d/*. Every file in this directory will be sourced (loaded) by Bash and thus made available.
