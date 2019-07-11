---
title: How Nix provides GHC with packages
date: 2019-07-10
tags: haskell, nix
category: Programming
authors: Tobias Pleyer
summary: "In this blog post I want to have a look at the actual build step for the ghc executable with packages via Nix"
---

# How Nix provides GHC with packages

Recently I am making myself familiar with [Nix](https://nixos.org/). I
personally think that the actual Nix manual is a little bit overwhelming to
start with. Instead I recommend the
[Nix pills](https://nixos.org/nixos/nix-pills/index.html) as an
entry point to the Nix world.

My current interest circles around how to use Nix for Haskell development.
Nix's manual packages devotes an entire section to the
[Haskell Infrastructure in Nix](https://nixos.org/nixpkgs/manual/#users-guide-to-the-haskell-infrastructure).
In this blog post I want to investigate how Nix actually builds Haskell
targets, i.e. what drops out of all the \*.nix files.

## Nixpkgs and Haskell

All the Haskell related stuff of nixpkgs can be found under
`<nixpkgs>/pkgs/development/haskell-modules`, where `<nixpkgs>` is the path to
your local nixpkgs directory. You can find the path via `nix repl`:

```bash
$ nix repl
nix-repl> "${<nixpkgs>}"
"/nix/store/91lz3yamb6hr31hpjdkpbxjv806s651b-nixpkgs"
```

There's actually quite a bit of stuff in there:

```bash
$ ls /nix/store/91lz3yamb6hr31hpjdkpbxjv806s651b-nixpkgs/pkgs/development/haskell-modules
configuration-common.nix     configuration-hackage2nix.yaml  hackage-packages.nix     non-hackage-packages.nix
configuration-ghc-8.2.x.nix  configuration-nix.nix           hoogle-local-wrapper.sh  patches
configuration-ghc-8.4.x.nix  configuration-tensorflow.nix    hoogle.nix               stack-hook.sh
configuration-ghc-8.6.x.nix  default.nix                     initial-packages.nix     with-packages-wrapper.nix
configuration-ghc-head.nix   generic-builder.nix             lib.nix
configuration-ghcjs.nix      generic-stack-builder.nix       make-package-set.nix
```

Reading into all these files is quite a mouthful! But the nice thing is we
don't need to do that! The whole purpose of a Nix expression is to eventually
produce a derivation that will be built. As an example the following line will
drop us into a new shell environment with a ghc executable and the lens and mtl
libraries installed:

```bash
$ nix-shell --pure -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [lens mtl])"
[nix-shell]$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.6.3
[nix-shell]$ ghc-pkg list
/nix/store/llplrrsi5dqbvsl9j6kfhadfvf94yg4v-ghc-8.6.3-with-packages/lib/ghc-8.6.3/package.conf.d
    ...
    kan-extensions-5.2
    lens-4.17
    libiserv-8.6.3
    mtl-2.2.2
    parallel-3.2.2.0
    ...
```

We can use `nix repl` to tell us where the derivation can be found:

```bash
$ nix repl
Welcome to Nix version 2.2.1. Type :? for help.

nix-repl> d = haskellPackages.ghcWithPackages (pkgs: with pkgs; [lens mtl])
nix-repl> d
«derivation /nix/store/gnry81f0fx07jn8yqwsa53hhwz9m0y8h-ghc-8.6.3-with-packages.drv»
```

Now we know where to find the derivation and we can print it to a file.

```bash
$ nix show-derivation /nix/store/gnry81f0fx07jn8yqwsa53hhwz9m0y8h-ghc-8.6.3-with-packages.drv | python -m json.tool > ghc-8.6.3-with-packages.drv
```

**Note:** I used the Python JSON tools as described in a
[previous post](./2019-06-30-pretty-format-json-console-output.html).

This is the content of the derivation file:

::: {.code-include lexer="json" file="code/ghc-8.6.3-with-packages.drv"}
:::

This is actually pretty straight forward to read. We can ignore the "inputDrvs"
attribute, as it is mostly relevant for Nix, not to understand what's going on.
We want to look at "builder", "args" and "env". Nix will call the builder with
the given arguments from "args" and inject the variables from "env" into the
environment of the call to builder. Ignoring most of the environment variables
and the exact paths this will more or less result in the following call:

```bash
$ buildCommand='mkdir -p...' paths='...' bash -e default-builder.sh
```

The script `default_builder.sh` is just a wrapper around the generic build of
Nix's stdenv generic build. As the name suggests it is a very generic build
script capable to build almost everything. It provides a lot of functionality,
e.g. running different installation phases, and is controlled via hooks that
must be defined in the environment. Here's what's in `default_builder.sh`:

```bash
source $stdenv/setup
genericBuild
```

The setup script defines the `genericBuild` function. In our case it will
simply execute the script defined by the *buildCommand* attribute:

```bash
...
genericBuild() {
    if [ -f "${buildCommandPath:-}" ]; then
        local oldOpts="$(shopt -po nounset)"
        set +u
        source "$buildCommandPath"
        eval "$oldOpts"
        return
    fi
    if [ -n "${buildCommand:-}" ]; then
        local oldOpts="$(shopt -po nounset)"
        set +u
        eval "$buildCommand"
        eval "$oldOpts"
        return
    fi
...
```

The *buildCommand* attribute is written in one line and thus hard to read. I
translated the literal newlines into actual newlines by copying the line in a
Vim buffer and then issuing the command `:s/\\n/\r/g`. Here is the result:

::: {.code-include lexer="bash" file="code/ghcBuildCommand.sh"}
:::

All this script does is sym-linking every library in the output directory of
the derivation in the Nix store and then create small wrapper scripts for all
the necessary executables (ghc, runghc, etc.). Finally the packages are
recached into the respective installation of GHC.

Instead of looking at the `makeWrapper` function it is easier to just have a
look at the result. Here is the wrapper script for ghc:

```bash
$ cat /nix/store/llplrrsi5dqbvsl9j6kfhadfvf94yg4v-ghc-8.6.3-with-packages/bin/ghc
#! /nix/store/53wi068kjrqfr2j0hzcxhbw2xaa990jr-bash-4.4-p23/bin/bash -e
export NIX_GHC='/nix/store/llplrrsi5dqbvsl9j6kfhadfvf94yg4v-ghc-8.6.3-with-packages/bin/ghc'
export NIX_GHCPKG='/nix/store/llplrrsi5dqbvsl9j6kfhadfvf94yg4v-ghc-8.6.3-with-packages/bin/ghc-pkg'
export NIX_GHC_DOCDIR='/nix/store/llplrrsi5dqbvsl9j6kfhadfvf94yg4v-ghc-8.6.3-with-packages/share/doc/ghc/html'
export NIX_GHC_LIBDIR='/nix/store/llplrrsi5dqbvsl9j6kfhadfvf94yg4v-ghc-8.6.3-with-packages/lib/ghc-8.6.3'
exec "/nix/store/7874h075nf8yikvr47642xqrwqwyv99s-ghc-8.6.3/bin/ghc"  "-B$NIX_GHC_LIBDIR" "${extraFlagsArray[@]}" "$@"
```

So we call the specified version of ghc, ghc-8.6.3 in this case, and tell it
where to find its libraries. As it is typical for Nix the ghc executable is
located in its respective location in the store and the environment is set up
so that everything works together.
