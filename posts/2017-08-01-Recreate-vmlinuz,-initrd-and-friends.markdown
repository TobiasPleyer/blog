---
title:  "Recreate vmlinuz, initrd and friends"
date: 2017-08-01
tags: linux, boot, initrd, howto
category: Programming
authors: Tobias Pleyer
summary: "Useful commands if you played to much with your Linux' */boot* folder"
---

Recreate vmlinuz, initrd and friends
====================================

How to save your */boot* folder
-------------------------------

### Preamble

When I installed Linux (Mint) on my PC I chose to reserve *\~300MB* for
the boot partition. In restrospect that was unnecessary stinginess.
Which personal computer nowadays cries for a few hundred megabytes?

However after a few kernel updates I ran out of memory. I made my
updates with Linux Mint's *Update Manger*. For every new kernel version
the new version was installed, but the old weren't deleted. I know it's
nice to have a few fallbacks, but to keep every version?

The smart guy I am I decided to just delete the old versions via *sudo
rm*

``` {.sourceCode .bash}
/boot$ sudo rm abi-4.4.0-*-generic initrd.img-4.4.0-*-generic System.map-4.4.0-*-generic vmlinuz-4.4.0-*-generic config-4.4.0-*-generic
```

This worked for the files - abi-4.4.0-xx-generic -
config-4.4.0-xx-generic - System.map-4.4.0-xx-generic -
vmlinuz-4.4.0-xx-generic

But *initrd.img-4.4.0-xx-generic* was recreated every time for every
*xx*. It took a while longer and I ran again out of memory. At this
point in time I still haven't realized the dilemma, so I decided to go
all in and delete all of the above file versions when the next kernel
update was ready, in the hope this will solve things. The net effect was
an intermediate state as a result of an out of memory abortion. With the
result that no bootable version was left. The old
*initrd.img-4.4.0-xx-generic* files were missing their counter parts and
the newest didn't have all other files because they couldn't be created.

**I was forced to solve this before the next restart!**

### Resolution

I saved my bash history with the relevant commands that helped me.

``` {.sourceCode .bash}
$ dpkg --list | grep linux-image # list all installed versions
$ sudo apt-get purge linux-image-4.4.0-{53,75,59,62,66,72,75,77,78,81,83}-generic # remove all installed versions (stops them from showing up again)
$ sudo apt-get purge linux-image-extra-4.4.0-{53,75,59,62,66,72,75,77,78,81,83}-generic
$ sudo dpkg --configure -a # Rerun debian package manager
$ sudo apt-get install -f # Fix broken packages
$ sudo apt-get upgrade # Upgrade packages
```

Nice to know:

``` {.sourceCode .bash}
$ sudo update-initramfs -u # Update existing initramfs
$ sudo update-initramfs -d -k 4.4.0-66-generic # Delete a specific version
$ sudo update-initramfs -c -k 4.4.0-87-generic # Create a specific version
```