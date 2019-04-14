My Git config at work
#####################

:date: 2017-04-05
:tags: git, gerrit, bash, configuration
:category: Programming
:authors: Tobias Pleyer
:summary: A look at my .gitconfig and .bashrc files

Motivation
==========

I use Git at work a lot. The problem is that naturally the same commands come again and again. After a while I got really tired to type the same stuff over and over.

The Files
=========

In the following I want to show my current configuration files for Git and my MinGW bash shipping with it that really save me a lot of typing and increase my efficiency. I will first show all needed files and then speack about it.

.. code-block:: bash

    # ~/.gitconfig
    [user]
        name = username
        email = username@company.org
    [credential "therepourl"]
        username = username
    [alias]
        revbr = "!f(){ git push origin HEAD:refs/for/"$@";}; f"
        adda = add --all
        br = branch
        ci = commit
        st = status
        lg = log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit
        # shell functions for automated submodule update
        co = "!f(){ git checkout "$@"; git submodule update;}; f"
        pu = "!f(){ git pull $1; git submodule update;}; f"
    [gitreview]
        remote = origin
    [color]
        ui = true
    [diff]
        tool = meld
        guitool = meld
        submodule = log
    [merge]
        tool = meld
    [mergetool "meld"]
        prompt = false
        keepBackup = false
        keepTemporaries = false
        path = c:/Program Files (x86)/Meld/Meld.exe
    [status]
        submoduleSummary = true
    [push]
        default = matching
    [commit]
        template = ~/.gittemplate
    [credential]
	    helper = store
    [core]
	    autocrlf = true

.. code-block:: bash

    # ~/.bashrc
    if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
    fi

    export PATH=$PATH:"/c/Program Files (x86)/CMake/bin"

    PS1='\n\[\033[32m\]\u\[\033[0;35m\]@\[\033[33m\]\w\[\033[36m\]` __git_ps1 `\[\033[0m\]\n[ \[\033[0;31m\]\# (\!)\[\033[0m\] ] \$ '

.. code-block:: bash

    # ~/.bash_aliases
    # Make python interpreter work in shell
    alias python='winpty python.exe'

    # Shorthand for git add
    alias ga='git add'

    # Shorthand for git add interactively on each file (prompt)
    alias gai='git ls-files --modified | xargs -n1 -r -p git add'

    # Shorthand for git branch
    alias gb='git branch'

    # Shorthand for clear; git branch
    alias cgb='clear; git branch'

    # Create new branch from current HEAD
    alias gcb='git checkout -b'

    # Shorthand for git diff
    alias gd='git diff'

    # Shorthand for git status
    alias gs='git status'

    # Shorthand for git status with option porcelain (easy parsing)
    alias gsp='git status --porcelain'

    alias only_modified='grep '"'"' M '"'"' | cut -d '"'"' '"'"' -f3'  #noprint
    alias do_add='xargs -n1 git add'  #noprint
    alias filter='egrep -v "criteria1|criteria2|..."'  #noprint
    alias add_modified='gsp | only_modified | do_add'  #noprint
    alias filter_add_modified='gsp | only_modified | filter | do_add'  #noprint
    # Execute git status but clear screen first
    alias cgs='clear && git st'

    # Clear screen and execute git status, then pipe it through a custom filter to show only relevant files
    alias fcgs='cgs | filter'

    # Shorthand for git commit
    alias gci='git commit'

    # Shorthand for git commit --amend
    alias gca='git commit --amend'

    # Shorthand for git checkout
    alias gco='git co'

    #new command
    alias gru='git remote update'

    # Shorthand for checking out the develop branch
    alias gcod='git co develop'

    # Automatically add all modified files (from the index) that pass through the custom filter
    alias gfa='filter_add_modified && cgs'

    # Show the history of this branch, i.e. just the commits since the branching off develop
    alias glb='clear && git lg develop..HEAD'

    # Show the last 10 commits as concise and decorated graph
    alias glg='clear && git lg -n10'

    # Revert all file changes that only affect whitespace
    alias grw='git diff -b --numstat | egrep $'"'"'^0\t0\t'"'"' | cut -f3- | xargs git checkout HEAD --'
    alias lsalias="clear && cat ~/.bash_aliases | grep -v '#noprint' | cut -d ' ' -f2- | /usr/bin/gawk -f ~/color.gawk"  #noprint
    alias resource='source ~/.bashrc'  #noprint

.. code-block:: bash

    # ~/.gittemplate
    Ticket-xxx: xxx

    xxx


    Changes
        
        * xxx

    Author: Tobias Pleyer
            The Company XYZ
            some other info

Disussion
=========

The workhorse commands of Git are without a doubt **git status**, **git checkout**, **git add** and **git commit** with differing optional parameters. These are most likely amongst everyone's *"Top 5 Git commands"*. I got them covered with **cgs**, **gco**, **ga**, **gci**. Counting keystrokes (no tab complete, spaces included) this gives a speed up of exactly *300%*! And these commands I type a lot! The *c* in **cgs** stands for clear and is a handy addition which clears the terminal screen before showing Git's status, this ensures maximum overview.

Because I use a lot of aliases I chose to not spam my *.bashrc* with it, but instead move them to a separate file, *.bash_aliases*, which is sourced (included) by *.bashrc*. This allows for another handy trick: I wrote myself an aliases to list my aliases. Typing **lsalias** will list the contents of *.bash_aliases*, including comments, but only those that do not have a *#noprint* at the end of the line. That way I can hide irritating helper code and show the import stuff. Most of the aliases are self explanatory. Because I work a lot on our development branch I wrote an extra alias to switch to it: **gcod** (*git checkout develop*). In general the pattern for all aliases is to use the first letter of every involved word if possible. That way typing the command while mumbling it comes automatic and helps to remember them.

Two more things: In my *.gitconfig* file I registered a template for commits. That comes handy if all (or most) of your commands have a common structure. I work with *Vim* as my editor and made myself a map which maps *F5* to a search for *xxx* and *F6* to jump to the next find result, delete it and enter edit-mode. Thus the placeholder *xxx* can be quickly filled with meaningful text. At work we have auto-generated stuff which is only committed for releases, but not during development. These files should not be **git add**'ed and are cluttering the output of **git status**. Hence I wrote myself commands, **fcgs** and **gfa**, which filter these files away. This is quite useful to avoid mistakes.
