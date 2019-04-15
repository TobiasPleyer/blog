---
title:  "Rust and Haskell"
date: 2017-06-02
tags: rust, haskell
category: Programming
authors: Tobias Pleyer
summary: "I have picked up on Rust and Haskell again"
---

Rust and Haskell
================

The unifying ideas behind Rust and Haskell
------------------------------------------

It has been a couple of years a go that I've been trying to wrap my head
around [Haskell](https://www.haskell.org/). Looking back at this time
from my current point of view I realize that I really didn't grasp many
of the concepts behind the language. It wasn't actually the logic of the
language itself, but the deeper meaning of why the things are done that
way. In my defense: Haskell is not the easieset language out there to
learn. It is very academic and for people that want to get going fast
probably tiresome. I finished the **Beginner's Track** of the [Haskell
wikibook](https://en.wikibooks.org/wiki/Haskell), tinkered a little bit
with Monads, but somehow couldn't get really warm with the language.
After a while other stuff went into focus and basically abandoned the
language. But I always had it in mind as "kind a cool and worth coming
back to at some point in the future".

Cut - We jump a few years ahead.

I was already pretty comfortable with the Python programming language
and start to dig around in the online community. One day I found [Armin
Ronacher\'s blog](http://lucumr.pocoo.org) and started to read his
posts. He's a smart guy and I really like the stuff that the
[pocoo](http://www.pocoo.org/) team is doing (Armin is part of that
team). It was in one of his blog posts that I first read about
[Rust](https://www.rust-lang.org). He found some good words for the
language and that got me interested. I read quite a bit documentation:
the Rust book, papers and other online resources. I liked the concepts,
but one the other hand the type system also scared me a bit. Again, as
with Haskell, I played around but didn't really achieve anything big
with the language. As with Haskell, I couldn't really motivate myself to
start a bigger project and these languages are also not seen a lot in
business environments. Eventually my Rust adventure came to an end as
well.

Cut again - We now have sometime earlier this year.

I decided to give Rust another go. I started to cross-read the official
online book again. This time I set myself a clear goal: choose a project
and do it. This went well and I'll come back at that on a different blog
post. The thing is that once again my mind had grown since the last time
I tried to learn Rust. It is really funny how you see things with
totally different eyes with one year time difference. I often gain that
insight that I didn't understand things very well back in the past. Like
with almost everything in life - context matters.

I had one of these epiphanies when looking the second time at Rust. Not
only was it all clearer now, I could really see common patterns. Rust is
such an interesting alternative for me because it unifies many of my
other programming interests. Firstly the languages that influenced Rust.
Obviously C (C++ not so much I guess), but also Python (e.g. Rust's
string formatting documentation explicitly mentions Python) and Haskell.
I am even brave enough to state:

**Rust is the imperative version of Haskell**

The reason why I say this is how similar Rust and Haskell are regarding
how their type system works, how they handle object orientation and how
they enforce error handling. A few examples:

1.  Rust has the *Option* type, which is exactly Haskell's *Maybe* type
2.  Rust has the *Error* type, which is Haskell's *Either* equivalent
3.  Both languages have type classes. There exists a separation between
    an object's interface and the data it holds. Objects can be a member
    of many type classes and every type class membership (*"the object
    instantiates the type class"*) is a promise that the object supports
    a certain interface. Rust calls them traits and I think that is a
    really good name, because if an object implements (*impl* in Rust) a
    trait it is like a change of character.
4.  Both languges make heavy use of pattern matching and unwrapping.

To make it short: Discovering these similarities reanimated my interest
and love for both languages. One thing though I think Rust made better
than Haskell is to answer the question *"Why do we want to deal with
this super strict type system?"*. The answer: It avoids stupid mistakes
and reduces unexpected program behaviour. At the beginning this system
feels for the unexperienced programmer like driving against a wall with
a car. I remember how lost I was to even get the simplest main functions
to compile. But after a while these types start to feel like an extra
safety net. You start to trust them and they start to be the torch in
the pitch-black night.