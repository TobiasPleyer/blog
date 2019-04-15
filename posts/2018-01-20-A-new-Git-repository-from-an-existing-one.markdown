---
title:  "A new Git repository from an existing one"
date: 2018-01-20
tags: git
category: Programming
authors: Tobias Pleyer
summary: "Command set to create a new Git repository from parts of an existing Git repository"
---

A new Git repository from an existing one
=========================================

I have a git repository called *Playground* in which I push all kinds of
ideas and tinker results without further concepts. But every now and
then things evolve and then I like to create a new dedicated repository
with only the changes realted to the project. In the following a list
the possible results to achieve this.

Option 1
--------

I found the following all in one solution on
[stackoverflow](https://stackoverflow.com/questions/5120038/is-it-possible-to-cherry-pick-a-commit-from-another-git-repository)

``` {.sourceCode .bash}
$ git --git-dir=../<some_other_repo>/.git format-patch -k -1 --stdout <commit SHA> | git am -3 -k
```

The nice thing about the above command is that we save the detour of
involving a git server, it's enough to have both repositories on the
local disk.

Option 2
--------

This solution is basically identical to the first option, but makes
every step more explicit. Note that below solution works on a range of
commits. This range is exactly the commits I was interested in

``` {.sourceCode .bash}
$ git format-patch <SHA_A>..<SHA_B> -o patches
$ cp -r patches path/to/fresh/repository
$ cd path/to/fresh/repository
$ git init
$ git am patches/*
$ rm -r patches
```

Changing directory layout
-------------------------

I use the one *Playground* repository for all of my programming
languages. Every language has a separate folder and in these folders
typically every new attempt has its own subfolder.

This kind of folder hierarchy is most certainly not what I want in a
dedicated repository. But the [git am]{.title-ref} command preserves the
folder hierarchy of the original patch source. Thus we have to rewrite
the folder hierarchy. Git typically refers to the folder hierarchy as
the *"tree"*, which can be manipulated via the [git
filter-branch]{.title-ref} command

``` {.sourceCode .bash}
$ tree
.
├── file1
├── file2
├── folder1
│   └── file3
└── folder2
    └── file4
$ git filter-branch --force --tree-filter 'mv folder1/* .'
$ git filter-branch --force --tree-filter 'mv folder2/* .'
$ tree
.
├── file1
├── file2
├── file3
└── file4
```

The above command sequence demonstrates how to flatten an existing
folder structure, thus allowing to hide the original hierarchy.