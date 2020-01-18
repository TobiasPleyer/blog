---
title: nix show-derivation is your friend
date: 2020-01-17
tags: nix
category: Programming
authors: Tobias Pleyer
summary: Use nix show-derivation to pretty print Nix derivations
---

# nix show-derivation is your friend

[Nix](https://nixos.org/nix/manual/) expressions for real world scenarios can
be amazingly complex. The advanced functional programming techniques in use and
the fact that Nix is dynamically typed don't make it easier to grok what is
actually going on. As an example have a look at the
[default.nix from reflex-platform](https://github.com/reflex-frp/reflex-platform/blob/v0.4.2.0/default.nix).

The reason that many .nix files tend to become so large is due to the need of
configurability. Most packages support a variety of configuration options,
operating systems and output formats. All of these add complexity and more
cluttered up code. The whole purpose of the Nix programming language is to
produce a dictionary (attribute set in Nix jargon), the so called derivation.

What if I want to know what the derivation actually does? You can work your
way through the .nix file(s) and try to understand what they do, and indeed
this can be very enlightening and I recommend doing that from time to time...
but what we really need is to have a look at the produced derivation.

That is exactly the purpose of `nix show-derivation`. I find it incredibly
useful and I am surprised that it is not really documented in the
[Nix manual page](https://nixos.org/nix/manual/), but just briefly
mentioned as a plumbing command in the
[appendix for release 2.0](https://nixos.org/nix/manual/#ssec-relnotes-2.0).
It was merged with
[this commit](https://github.com/NixOS/nix/commit/e8d6ee7c1b90a2fe6d824f1a875acc56799ae6e2).

What does the command do? Well you can compare it with using the *-E* option
of g++: All variable references are resolved and replaced by the actual values,
everything not required is gone and you see only the inputs, build instructions
and the build environment. At that point it mostly boiles down to reading the
bash code of the builder. In addition the attribute set is pretty printed in
JSON format.

# Example

As an example we will create a derivation that will fetch a repository via Git.
nixpkgs contains a package called `fetchgit`. It can be found under
*nixpkgs/pkgs/build-support/fetchgit*. At the time of this writing its
*default.nix* has the following content:

```
{stdenvNoCC, git, cacert}: let
  urlToName = url: rev: let
    inherit (stdenvNoCC.lib) removeSuffix splitString last;
    base = last (splitString ":" (baseNameOf (removeSuffix "/" url)));

    matched = builtins.match "(.*).git" base;

    short = builtins.substring 0 7 rev;

    appendShort = if (builtins.match "[a-f0-9]*" rev) != null
      then "-${short}"
      else "";
  in "${if matched == null then base else builtins.head matched}${appendShort}";
in
{ url, rev ? "HEAD", md5 ? "", sha256 ? "", leaveDotGit ? deepClone
, fetchSubmodules ? true, deepClone ? false
, branchName ? null
, name ? urlToName url rev
, # Shell code executed after the file has been fetched
  # successfully. This can do things like check or transform the file.
  postFetch ? ""
, preferLocalBuild ? true
}:

/* NOTE:
   fetchgit has one problem: git fetch only works for refs.
   This is because fetching arbitrary (maybe dangling) commits may be a security risk
   and checking whether a commit belongs to a ref is expensive. This may
   change in the future when some caching is added to git (?)
   Usually refs are either tags (refs/tags/*) or branches (refs/heads/*)
   Cloning branches will make the hash check fail when there is an update.
   But not all patches we want can be accessed by tags.

   The workaround is getting the last n commits so that it's likely that they
   still contain the hash we want.

   for now : increase depth iteratively (TODO)

   real fix: ask git folks to add a
   git fetch $HASH contained in $BRANCH
   facility because checking that $HASH is contained in $BRANCH is less
   expensive than fetching --depth $N.
   Even if git folks implemented this feature soon it may take years until
   server admins start using the new version?
*/

assert deepClone -> leaveDotGit;

if md5 != "" then
  throw "fetchgit does not support md5 anymore, please use sha256"
else
stdenvNoCC.mkDerivation {
  inherit name;
  builder = ./builder.sh;
  fetcher = ./nix-prefetch-git;  # This must be a string to ensure it's called with bash.
  nativeBuildInputs = [git];

  outputHashAlgo = "sha256";
  outputHashMode = "recursive";
  outputHash = sha256;

  inherit url rev leaveDotGit fetchSubmodules deepClone branchName postFetch;

  GIT_SSL_CAINFO = "${cacert}/etc/ssl/certs/ca-bundle.crt";

  impureEnvVars = stdenvNoCC.lib.fetchers.proxyImpureEnvVars ++ [
    "GIT_PROXY_COMMAND" "SOCKS_SERVER"
  ];

  inherit preferLocalBuild;
}
```

We can use *nix repl* to create a derivation and build it:

```bash
$ nix repl
nix-repl> :l <nixpkgs>
Added 11323 variables.

nix-repl> d = fetchgit { url = "https://github.com/TobiasPleyer/blog"; rev = "d534112a6d750feac93e15f8827f744d369cf997"; sha256 = "0xpgbadav48p520z1780pdjy52lh1lnmm1nksiwbbg2i2zi011ff"; }

nix-repl> d
«derivation /nix/store/s1pkxykzknjr8azxi602g41l1a2q7i86-blog-d534112.drv»
```

The last command in the nix repl told us the store path of the derivation
created by the above fetchgit expression. Now we can use *nix show-derivation*
to inspect it:

```bash
$ nix show-derivation /nix/store/s1pkxykzknjr8azxi602g41l1a2q7i86-blog-d534112.drv
{
  "/nix/store/s1pkxykzknjr8azxi602g41l1a2q7i86-blog-d534112.drv": {
    "outputs": {
      "out": {
        "path": "/nix/store/gp9q35vwfpi5j4fg9vf4061mlby86b6v-blog-d534112",
        "hashAlgo": "r:sha256",
        "hash": "ce8500e21751bcb578d4d3865a2d0d908ae265bb009df081281791ad9a5aef76"
      }
    },
    "inputSrcs": [
      "/nix/store/83mnyskabyjv1d2111641bh8zjbhi3k8-nix-prefetch-git",
      "/nix/store/jr7l8xyyanakpqr954rvj58lyqal1vfb-builder.sh"
    ],
    "inputDrvs": {
      "/nix/store/84p0j6w8qlzaawkyqr7brgpbnbl260hs-nss-cacert-3.47.1.drv": [
        "out"
      ],
      "/nix/store/bg1lbq2w6nm9hxbfl6byxidwmz7r6256-stdenv-linux.drv": [
        "out"
      ],
      "/nix/store/ij7yr26mjpnpj78min707x88cbg35sl8-bash-4.4-p23.drv": [
        "out"
      ],
      "/nix/store/ygxgdlaqk4kf28375snswfc7l0l81ziy-git-2.24.1.drv": [
        "out"
      ]
    },
    "platform": "x86_64-linux",
    "builder": "/nix/store/xb062l4b76zyhq6grqf4iyfdikkpg8fl-bash-4.4-p23/bin/bash",
    "args": [
      "-e",
      "/nix/store/jr7l8xyyanakpqr954rvj58lyqal1vfb-builder.sh"
    ],
    "env": {
      "GIT_SSL_CAINFO": "/nix/store/86yih5l06czb14hdqxfx3g9vrkph2gwz-nss-cacert-3.47.1/etc/ssl/certs/ca-bundle.crt",
      "buildInputs": "",
      "builder": "/nix/store/xb062l4b76zyhq6grqf4iyfdikkpg8fl-bash-4.4-p23/bin/bash",
      "configureFlags": "",
      "deepClone": "",
      "depsBuildBuild": "",
      "depsBuildBuildPropagated": "",
      "depsBuildTarget": "",
      "depsBuildTargetPropagated": "",
      "depsHostHost": "",
      "depsHostHostPropagated": "",
      "depsTargetTarget": "",
      "depsTargetTargetPropagated": "",
      "doCheck": "",
      "doInstallCheck": "",
      "fetchSubmodules": "1",
      "fetcher": "/nix/store/83mnyskabyjv1d2111641bh8zjbhi3k8-nix-prefetch-git",
      "impureEnvVars": "http_proxy https_proxy ftp_proxy all_proxy no_proxy GIT_PROXY_COMMAND SOCKS_SERVER",
      "leaveDotGit": "",
      "name": "blog-d534112",
      "nativeBuildInputs": "/nix/store/f17kx9ibxsz09xk1izcwjnngz0m6hvli-git-2.24.1",
      "out": "/nix/store/gp9q35vwfpi5j4fg9vf4061mlby86b6v-blog-d534112",
      "outputHash": "0xpgbadav48p520z1780pdjy52lh1lnmm1nksiwbbg2i2zi011ff",
      "outputHashAlgo": "sha256",
      "outputHashMode": "recursive",
      "outputs": "out",
      "patches": "",
      "postFetch": "",
      "preferLocalBuild": "1",
      "propagatedBuildInputs": "",
      "propagatedNativeBuildInputs": "",
      "rev": "d534112a6d750feac93e15f8827f744d369cf997",
      "stdenv": "/nix/store/n9bfy6wrzvggp430hnq9nviv8zijn7ic-stdenv-linux",
      "strictDeps": "",
      "system": "x86_64-linux",
      "url": "https://github.com/TobiasPleyer/blog"
    }
  }
}
```

This output is much nicer and clearer to read in order to understand what is
going on. We can see the:

- input files: nix-prefetch-git and builder.sh
- input derivations: nss-cacert-3.47.1, stdenv-linux, bash-4.4-p23 and git-2.24.1
- build command: `bash -e builder.sh`
- the complete environment under *env*

If we want to we could now build the derivation, e.g. in the repl again:

```bash
nix-repl> :b d

this derivation produced the following outputs:
  out -> /nix/store/gp9q35vwfpi5j4fg9vf4061mlby86b6v-blog-d534112
```

# Conclusion

At work I very often want to know what a derivation actually does. Since
"reading the source" is often too time consuming *nix show-derivation* is a
really handy time saver.
