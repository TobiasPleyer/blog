---
title: It is always good to know what you are doing
date: 2020-01-14
tags: nix
category: Programming
authors: Tobias Pleyer
summary: How I ruined my nix.conf file by blindly following howto instructions
---

# It is always good to know what you are doing

I guess nobody will object to this statement, but in this post I just want to
share a mistake I made while trying to work with a software I am not yet very
familiar with. The problem I want to address is that it is sometimes pretty
hard for a newcomer to evaluate the information to be found on various blogs,
threads, or even the official web site.

With the start of the year I also started a new job. At work we are using the
[Obelisk framework](https://github.com/obsidiansystems/obelisk). Obelisk makes
heavy use of [Nix](https://nixos.org/nix/). Because Obelisk has a lot of
dependencies it makes use of so called binary caches. That means instead of
building everything locally on your machine you can download pre-built packages
from a server which leads to a massive reduction of build time.
[Obelisk's Github page](https://github.com/obsidiansystems/obelisk#installing-obelisk)
tells you how to configure the caches. I am fairly new to Nix so I blindly did
what I was told to do.

In my private time at home I started to play around with
[Leksah](https://github.com/leksah/leksah), an IDE for Haskell written in
Haskell. As of lately the only supported build method for Leksah is deploying
Nix. Leksah's wiki page recommends setting up the binary caches of IOHK and
refers to [their Github](https://github.com/input-output-hk/cardano-sl/blob/master/docs/nix.md#binary-cache)

I quote the instructions here:

    Add the following lines to /etc/nix/nix.conf. If the file does not exist,
    then create it.

The word **Add** can be very misleading here. I interpreted add like append, so
I appended the provided lines to my existing */etc/nix/nix.conf* file...

Admittedly if you compare the instructions and contents of both wiki pages you
probably would start to become a bit wary, but as a programmer you become so
accustomed to copy-pasting stuff from the internet that it is easy to miss that
point. At this point my *nix.conf* was broken. The problem is that most of the
instructions on wiki pages assume that you had just installed Nix and nothing
more. Of course it is impossible for the maintainers to know what crazy stuff
the people that use their software do. **BUT**: Sometimes it is worth
explaining a bit what the given config entries **actually do**. For example
*substituters* is a whitespace separated **list** and multiple occurences are
not ok. This can be read [here](https://www.mankier.com/5/nix.conf) for
example.

Strangely though Nix seems to silently ignore this error and just default to
building **everything!** I just came to realize something is wrong as my builds
became absurdly long and no messages about cache downloads were popping up
anymore.

# Conclusion

Configurations are a pain point most of the time, because there are simply too
many existing formats and standards out there. In addition you usually already
must know the application in order to understand what the options are. And last
but not least writing goog documentation is sadly most of the time neglected,
partly because of time constraints and partly because developers usually don't
like to write documentation...
