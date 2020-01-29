---
title: Nix overlay evaluation example
date: 2020-01-29
tags: nix
category: Programming
authors: Tobias Pleyer
summary: A full example showcasing overlays and fixpoint evaluation in Nix.
---

# Fixpoints, attribute sets and overlays

This post will be another code full evaluation example, i.e. I will start with
an expression in a functional language, [Nix](https://nixos.org/nix/manual/) in
this case, and continuously reduce it until it has reached its normal form. I
find those kind of reduction processes tremendously enlightening and helpful in
understanding important concepts.

Today's post will actually be a 2in1, because I want to give an example for
two important concepts used in Nix: fixpoint evaluation of self-referential
attribute sets and the concepts of overlays to manipulate this evaluation
process. Because these two concepts actually go hand-in-hand I only need one
reduction process to show both techniques, for more see below.

**Note:** Since this post is about basic concepts of Nix I of course don't
assume expert knowledge in order to follow, but I do assume a familiarity with
the syntax of the language and at least basic knowledge about the semantics.
That said I will make every reduction step below explicit, but will not explain
what a function is etc.

# Short aside

Before I get going I want to quickly explain what I am actually talking about.

When I talk about a self referential attribute set I mean a construct like
below:

```nix
self: { a = 3; b = 4; c = self.a+self.b; }
```

Strictly speaking this is merely a function taking one argument and returning
an attribute set. The tricky part is of course the *self* parameter. It will be
used to inject the set into itself via fixpoint evaluation. In this sense the
above construct can also be seen as:

- an "unfinished" attribute set
- a "blueprint" for an attribute set
- a build recipe for an attribute set

I will refer to it as a *self-referential* attribute set or as *explicitely
recursive* attribute set. In Nix those constructs are used to allow the
definition of sets for which some of their variables depend on the value of
other variables within the same set. Nix also comes with a *rec* keyword to
flag an attribute set as self-referential, but this is just syntactic sugar for
fixpoint evaluation.

Note however that *self* is merely a name by convention. It doesn't carry any
special meaning and could be renamed without loss of generality. Also, since
the above expression is just a function, you could of course call it with any
kind of arguments:

```nix
nix-repl> f = self: { a = 3; b = 4; c = self.a+self.b; }
nix-repl> f { a = 7; b = 3; c = 5; d = "something"; }
{ a = 3; b = 4; c = 10; }
```

## Fixpoint evaluation

Fixpoint evaluation means evaluating `fix f`, where `f` is some function and
`fix` the function defined by

```nix
fix = f: let x = f x; in x;
```

in other words `fix f` is the infinite application of f to itself:

```nix
fix f = f (f (f (f (...))))
```

In most languages that would just run forever and halt with an out of memory
error due to **strict evaluation**.  However **Nix is lazy**, so it has a
chance to finish as we will see. `fix f` is called the fixpoint of `f` and
evaluating it is what I call the fixpoint evaluation of `f`.

## Overlays

Conceptually in Nix an overlay alters the values of certain attributes in an
attribute set. It "lays" new values over some attributes, causing the dependent
attributes to change their values too.

In order to makes this possible overlays have to be injected into the fixpoint
evaluation of the set, i.e. **before** the set is fully evaluated. Overlays
must have a specific function signature what I call the "overlay pattern":

```nix
let overlay = self: super:
{
   ...
};
```

So an overlay must take two parameters, conventionally called *self* and
*super*, and must return an attribute set. *self* refers to the **fully
evaluated** set, so this is actually a look into the future! *super* refers to
the evaluation of the attribute set up until that point. The
[nixos wiki](https://nixos.wiki/wiki/Overlays) comes with a nice illustration:

![Overlays in Nix](../images/nix_overlays.png){ width=100% }

As the images shows all overlay "blocks" are connected to the same *self*
(you must read this kind of like a circuit diagram). The super input, however,
is only wired to the previous stage. All of this becomes much clearer once you
see it live and in action.

How are overlays used in Nix? One example is the *overlays* argument for the
`<nixpkgs>` expression:

```nix
import <nixpkgs> { overlays = [ overlay1 overlay2 ]; }
```

Via several twists and turns these overlays end up as part of the bootstrapping
stage of Nixpkgs, as can be seen in `toFix` in the file
[stage,nix](https://github.com/NixOS/nixpkgs/blob/19.09/pkgs/top-level/stage.nix).
Below I will evaluate an expression equivalent to `toFix`, just with less
stages and a simpler attribute set, but otherwise the mechanics are identical.

## Useful links

Much of the stuff is actually fairly well documented, either directly on the
NixOS site or on other websites. Here is a short list:

- [NixOS wiki about overlays](https://nixos.wiki/wiki/Overlays)
- [Nixpkgs manual](https://nixos.org/nixpkgs/manual/#chap-overlays)
- [Nix pill about overriding](https://nixos.org/nixos/nix-pills/nixpkgs-overriding-packages.html)
- [Nix's fixed-points.nix file](https://github.com/NixOS/nixpkgs/blob/19.09/lib/fixed-points.nix)
- [Nix's stage,nix file](https://github.com/NixOS/nixpkgs/blob/19.09/pkgs/top-level/stage.nix)

# The example

Our explicitely recursive set looks like this:

```nix
pkgs = self: { a = 1; b = 2; c = 3; d = self.a + self.b; e = self.c + self.d; }
```

And we will apply the following overlays to it

### overlay1

```nix
let overlay1 = self: super:
{
  a = 1;
  b = 2;
  c = 3;
  d = self.a + self.b;
  e = self.c + self.d;
};
```

### overlay2

```nix
let overlay2 = self: super:
{
  x = super.a;
  b = 22;
  c = 11;
};
```

### overlay3

```nix
let overlay3 = self: super:
{
  a = 8;
  y = self.d + 7;
};
```

## Overlay application

We will evaluate the following expression, which is similar to `toFix` in
[stage,nix](https://github.com/NixOS/nixpkgs/blob/19.09/pkgs/top-level/stage.nix).

```nix
toFix = lib.foldl' (lib.flip lib.extends) (self: {}) ([
  overlay1
  overlay2
  overlay3
])
```

For easier reference here are the definitions for the above library functions

```nix
# foldl op nul [x_1 x_2 ... x_n] == op (... (op (op nul x_1) x_2) ... x_n)
foldl = op: nul: list:
  let
    foldl' = n:
      if n == -1
      then nul
      else op (foldl' (n - 1)) (elemAt list n);
  in foldl' (length list - 1);

flip = f: a: b: f b a;

extends = f: rattrs: self: let super = rattrs self; in super // f self super;

extends' = flip extends
         = rattrs: f: self: let super = rattrs self; in super // f self super;
```

Finally we can start:

```nix
toFix = lib.foldl' (lib.flip lib.extends) (self: {}) ([
  overlay1
  overlay2
  overlay3
])
=
lib.foldl' lib.extends' (self: {}) ([
  overlay1
  overlay2
  overlay3
])
=
extends' (extends' (extends' (self: {}) overlay1) overlay2) overlay3 
=
extends' (extends' (self: let super = (self: {}) self; in super // overlay1 self super) overlay2) overlay3
=
extends' (extends' (self: {} // overlay1 self {}) overlay2) overlay3
=
extends' (self: let super = (self: {} // overlay1 self {}) self; in super // overlay2 self super) overlay3
=
extends' (self: ({} // overlay1 self {}) // overlay2 self ({} // overlay1 self {})) overlay3
=
self: let super = (self: ({} // overlay1 self {}) // overlay2 self ({} // overlay1 self {})) self in super // overlay3 self super
=
self: (({} // overlay1 self {}) // overlay2 self ({} // overlay1 self {})) // overlay3 self (({} // overlay1 self {}) // overlay2 self ({} // overlay1 self {}))
```

At this point we can start to evaluate the function calls to the overlays.

```nix
self: (({} // overlay1 self {}) // overlay2 self ({} // overlay1 self {})) // overlay3 self (({} // overlay1 self {}) // overlay2 self ({} // overlay1 self {}))
=
# overlay1 self {} = { a = 1; b = 2; c = 3; d = self.a + self.b; e = self.c + self.d; }
=
self: (({} // { a = 1; b = 2; c = 3; d = self.a + self.b; e = self.c + self.d; }) // overlay2 self ({} // { a = 1; b = 2; c = 3; d = self.a + self.b; e = self.c + self.d; })) // overlay3 self (({} // { a = 1; b = 2; c = 3; d = self.a + self.b; e = self.c + self.d; }) // overlay2 self ({} // { a = 1; b = 2; c = 3; d = self.a + self.b; e = self.c + self.d; }))
=
# {} // { a = 1; b = 2; c = 3; d = self.a + self.b; e = self.c + self.d; } = { a = 1; b = 2; c = 3; d = self.a + self.b; e = self.c + self.d; }
=
self: ({ a = 1; b = 2; c = 3; d = self.a + self.b; e = self.c + self.d; } // overlay2 self { a = 1; b = 2; c = 3; d = self.a + self.b; e = self.c + self.d; }) // overlay3 self ({ a = 1; b = 2; c = 3; d = self.a + self.b; e = self.c + self.d; } // overlay2 self { a = 1; b = 2; c = 3; d = self.a + self.b; e = self.c + self.d; })
=
#       overlay2 self { a = 1; b = 2; c = 3; d = self.a + self.b; e = self.c + self.d; } =
#       self: super:
#       {
#         x = super.a;
#         b = 22;
#         c = 11;
#       } self { a = 1; b = 2; c = 3; d = self.a + self.b; e = self.c + self.d; }
#       =
#       {
#         x = { a = 1; b = 2; c = 3; d = self.a + self.b; e = self.c + self.d; }.a;
#         b = 22;
#         c = 11;
#       }
#        =
#       {
#         x = 1;
#         b = 22;
#         c = 11;
#       }
=
self: ({ a = 1; b = 2; c = 3; d = self.a + self.b; e = self.c + self.d; } // { x = 1; b = 22; c = 11; }) // overlay3 self ({ a = 1; b = 2; c = 3; d = self.a + self.b; e = self.c + self.d; } // { x = 1; b = 22; c = 11; })
=
self: { a = 1; b = 22; c = 11; d = self.a + self.b; e = self.c + self.d; x = 1; } // overlay3 self { a = 1; b = 22; c = 11; d = self.a + self.b; e = self.c + self.d; x = 1; }
=
#       overlay3 self { a = 1; b = 22; c = 11; d = self.a + self.b; e = self.c + self.d; x = 1; } =
#       self: super:
#       {
#         a = 8;
#         y = self.d + 7;
#       } self { a = 1; b = 2; c = 3; d = self.a + self.b; e = self.c + self.d; }
#       =
#       {
#         a = 8;
#         y = self.d + 7;
#       }
=
self: { a = 1; b = 22; c = 11; d = self.a + self.b; e = self.c + self.d; x = 1; } // { a = 8; y = self.d + 7; }
=
self: { a = 8; b = 22; c = 11; d = self.a + self.b; e = self.c + self.d; x = 1; y = self.d + 7; }
```

At this point we reached an important milestone: all overlays have been fully
evaluated and the expression has reduced to a common explicitely recursive
attribute set.

**One very important thing to notice from above:** The *self* parameter has
been threaded through all overlay applications and set reductions. Always the
"inner" *self* was replaced by the "outer" *self*. Hence **throughout the whole
application self refers to the same thing**. This neat trick makes it possible
to end up with a self-referential set at the end with one "global" *self*
parameter.

Further things to note:

- Throughout the overlay application `a`, `b`, and `c` have changed their
  original values. After all that is exactly what overlays are meant for.
- `x` was assigned `super.a`, which refers to `a` at the current stage of
  evaluation. As you can see at the end result `x` has `a`'s old value `1`,
  while `a = 8`.
- It was no problem to define completely new keys depending on existing keys in
  the set, namely `y = self.d + 7`.
- Many expressions appeared more than once in the reduction process (e.g. the
  application of overlay1). This is normal due to the recursive nature of the
  reduction. The nice thing however is that it is enough to evaluate the
  expressions only once. Thanks to
  [purity](https://en.wikipedia.org/wiki/Pure_function) and
  [referential transparency](https://en.wikipedia.org/wiki/Referential_transparency)
  we know it is safe to replace all occurences of an expression with the
  evaluation of one of them.

This concludes the first functional programming technique applied by Nix:
overlays. In the next section we will look at evaluating a self-referential set
via fixpoint evaluation.

## Fixpoint evaluation

In the previous section we ended up with the following result:

```nix
set = self: { a = 8; b = 22; c = 11; d = self.a + self.b; e = self.c + self.d; x = 1; y = self.d + 7; }
```

We can reduce this self-referential attribute set to a normal attribute set
(i.e. getting rid of *self*) by evaluating its fixpoint.

```nix
res = fix set
= set (set (set ...))
= self: { a = 8; b = 22; c = 11; d = self.a + self.b; e = self.c + self.d; x = 1; y = self.d + 7; } (set (set ...))
= { a = 8; b = 22; c = 11; d = (set (set ...)).a + (set (set ...)).b; e = (set (set ...)).c + (set (set ...)).d; x = 1; y = (set (set ...)).d + 7; }
# (set (set ...)) = { a = 8; b = 22; c = 11; d = (set (set ...)).a + (set (set ...)).b; e = (set (set ...)).c + (set (set ...)).d; x = 1; y = (set (set ...)).d + 7; }
= { a = 8; b = 22; c = 11; d = { a = 8; b = 22; c = 11; d = (set (set ...)).a + (set (set ...)).b; e = (set (set ...)).c + (set (set ...)).d; x = 1; y = (set (set ...)).d + 7; }.a + { a = 8; b = 22; c = 11; d = (set (set ...)).a + (set (set ...)).b; e = (set (set ...)).c + (set (set ...)).d; x = 1; y = (set (set ...)).d + 7; }.b; e = { a = 8; b = 22; c = 11; d = (set (set ...)).a + (set (set ...)).b; e = (set (set ...)).c + (set (set ...)).d; x = 1; y = (set (set ...)).d + 7; }.c + { a = 8; b = 22; c = 11; d = (set (set ...)).a + (set (set ...)).b; e = (set (set ...)).c + (set (set ...)).d; x = 1; y = (set (set ...)).d + 7; }.d; x = 1; y = { a = 8; b = 22; c = 11; d = (set (set ...)).a + (set (set ...)).b; e = (set (set ...)).c + (set (set ...)).d; x = 1; y = (set (set ...)).d + 7; }.d + 7; }
# Lazy evaluation: We don't have to evaluate the full expression, only as much to get our answer
= { a = 8; b = 22; c = 11; d = 8 + 22; e = 11 + (set (set ...)).a + (set (set ...)).b; x = 1; y = (set (set ...)).a + (set (set ...)).b + 7; }
# We already know what (set (set ...)).a and (set (set ...)).b are, namely 8 and 22
= { a = 8; b = 22; c = 11; d = 8 + 22; e = 11 + 8 + 22; x = 1; y = 8 + 22 + 7; }
= { a = 8; b = 22; c = 11; d = 30; e = 41; x = 1; y = 37; }
```

Now we are at the end and know the exact value of every key in the set. The
trick was to dig deeper and deeper down the recursive definition of the set
until we reach a key with a value that does not reference *self*. This value we
can then "bubble up" one layer to reduce it and so forth until all references
to self have been resolved.
