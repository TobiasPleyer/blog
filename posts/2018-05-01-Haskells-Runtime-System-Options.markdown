---
title:  "Haskell's Runtime System Options"
date: 2018-05-01
tags: haskell, runtime, options
category: Programming
authors: Tobias Pleyer
summary: "Just a short note about Haskell's RTS options"
---

Haskell's Runtime System Options
================================

This post is just a quick comment about Haskell's runtime system
options. Most tutorials explain to give those via [+RTS
\[options\]]{.title-ref}, e.g.
[here](https://wiki.haskell.org/Haskell_for_multicores). But most of the
time those "classic examples" run simple scripts without command line
arguments. But this obviously conflicts if we want to have command line
arguments too.

Luckily the Haskell runtime system is so nice to tell us what to do:

``` {.sourceCode .bash}
$ chefkoch +RTS -N2 --year=2017
chefkoch: unknown RTS option: --year=2017
chefkoch: 
chefkoch: Usage: <prog> <args> [+RTS <rtsopts> | -RTS <args>] ... --RTS <args>
chefkoch: 
chefkoch:    +RTS    Indicates run time system options follow
chefkoch:    -RTS    Indicates program arguments follow
chefkoch:   --RTS    Indicates that ALL subsequent arguments will be given to the
chefkoch:            program (including any of these RTS flags)
chefkoch: 
chefkoch: The following run time system options are available:
chefkoch: 
chefkoch:   -?       Prints this message and exits; the program is not executed
chefkoch:   --info   Print information about the RTS used by this program
chefkoch: 
chefkoch:   -K<size>  Sets the maximum stack size (default: 80% of the heap)
chefkoch:             Egs: -K32k -K512k -K8M
... more
```

So here for completeness what most of these tutorials miss to mention:
[+RTS]{.title-ref} "opens" the backdoor to Haskell's runtime system, but
this door can be closed again, with [-RTS]{.title-ref}!

So the rules are simple:

> 1.  The Haskell runtime system parses the command line first
> 2.  Everything after a [+RTS]{.title-ref} until the end of the line or
>     a [-RTS]{.title-ref} is consumed by the runtime, everything else
>     is forwarded to the application

So in our above example these two options do the trick:

``` {.sourceCode .bash}
$ chefkoch +RTS -N2 -RTS --year=2017
$ chefkoch --year=2017 +RTS -N2
```

This can also be read in the [official
documentation](https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/runtime-control.html).