---
title:  "Bash autocomplete"
date: 2017-04-17
tags: bash, linux
category: Programming
authors: Tobias Pleyer
summary: "Configuration interface to Bash's autocomplete feature"
---

Bash autocomplete
=================

Motivation
----------

Recently I worked with Git in my bash shell and when I typed *git
stat\<TAB\>* and magically the words *git status* appeared I thought to
myself: *"How does this actually work?"*

Autocomplete with Bash
----------------------

One of the very handy features of Bash is its autocompletion feature.
Pressing tab at the prompt will give suggestions depending on the
current context. Bash uses the [readline
library](https://cnswww.cns.cwru.edu/php/chet/readline/rltop.html) to
provide the prompt editing features. But I don't want to speak about the
internals and the readline implementation, because Bash actually
provides a more controlled configuration interface for its
autocompletion features.

### Programmable Completion

The Bash documentation refers to this feature as [Programmable
Completion](https://www.gnu.org/software/bash/manual/bash.html#Programmable-Completion).
Bash provides three functions to handle most of the bulk work to make
autocompletion available: *compgen*, *complete* and *compopt*.

The *complete* function registers completer functions (often using
*compgen*) for every command that should have autocompletion
functionality. In order to register the function *foo\_completer* to
assist in autocompleting the command *foo* we would type

``` {.sourceCode .bash}
~$ complete -F foo_completer foo
```

[Part
1](https://debian-administration.org/article/316/An_introduction_to_bash_completion_part_1)
and [Part
2](https://debian-administration.org/article/317/An_introduction_to_bash_completion_part_2)
of this article give a very nice and concise explanation of the
ongoings.

### Programmable Completion for Git

I just want to give the concrete example in case of Git on my PC. Most
likely the mentioned paths differ for other distros. On my PC this is
the default installation and location behaviour.

The Bash completion functions are searched in the file
*/etc/bash\_completion*. In my case this file is only sourcing the file
*/usr/share/bash\_completion*. In this file a lot of autocompletion for
common command line commands like *cd* are initialized.

The completion for Git are located under
*/usr/share/bash-completion/completions/git*. This file is loaded with
helper functions, but almost at the bottom comes the crucial part.

``` {.sourceCode .bash}
# Setup completion for certain functions defined above by setting common
# variables and workarounds.
# This is NOT a public function; use at your own risk.
__git_complete ()
{
```

> local wrapper="\_\_git\_wrap\${2}" eval "\$wrapper () {
> \_\_git\_func\_wrap \$2 ; }" complete -o bashdefault -o default -o
> nospace -F \$wrapper \$1 2\>/dev/null

> \|\| complete -o default -o nospace -F \$wrapper \$1

> }
>
> \# wrapper for backwards compatibility \_git () {

> \_\_git\_wrap\_\_git\_main

> }
>
> \# wrapper for backwards compatibility \_gitk () {

> \_\_git\_wrap\_\_gitk\_main

> }
>
> \_\_git\_complete git \_\_git\_main \_\_git\_complete gitk
> \_\_gitk\_main

Line 2749 does the actual registration for the autocompletion of the
*git* command.

### Location best practice

Above mentioned locations are the locations for system installation.
Custom user completion files should be saved under
*/etc/bash\_completion.d/*. Every file in this directory will be sourced
(loaded) by Bash and thus made available.