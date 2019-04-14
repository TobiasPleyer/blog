Python Optional Dependencies
############################

:date: 2017-08-21
:tags: linux python dependencies install
:category: Programming
:authors: Tobias Pleyer
:summary: A reminder to myself about the packages that should be installed before installing Python from source

Python from Source
==================

Usually when I am on a new Linux system, or on a fresh install and I want to install Python I build it from source cloning the Git repository and checking out the tag I want.
I always make the same mistake to forget installing the optional dependencies prior to installing Python. You can install Python without them, but some of them are really useful, e.g. ncurses and SSL support.

As a reminder for myself, here is the line

.. code-block:: bash

    sudo apt-get install make build-essential libssl-dev zlib1g-dev libbz2-dev libsqlite3-dev libncursesw5-dev libreadline-gplv2-dev libgdbm-dev libc6-dev
