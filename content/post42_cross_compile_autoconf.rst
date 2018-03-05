Cross compiling with autoconf
#############################

:date: 2018-03-05
:tags: bash, gnu, autoconf
:category: Programming
:authors: Tobias Pleyer
:summary: How to cross compile a program using GNU autoconf


This post is just a simple self reminder how to cross compile programs which
are configurable with GNU autoconf.

.. code:: bash

    $ # I use the Linaro cross compilation suite here
    $ # Target: An ARMv7 based board
    $ # HOST: My x86 64bit machine running VirtualBox
    $ LINARO_BIN_FOLDER=/home/username/Linaro/gcc-linaro-7.2.1-2017.11-x86_64_arm-linux-gnueabihf/bin
    $ export PATH=$PATH:$LINARO_BIN_FOLDER
    $ export CC=arm-linux-gnueabihf-gcc
    $ export CXXC=arm-linux-gnueabihf-g++
    $ $CC --version # just a quick sanity check
    $ ./configure --host=arm-linux-gnueabihf --build=x86_64-linux-gnu
    $ make

Links
=====

* `stackoverflow`_
* `GNU website`_

.. _stackoverflow: https://stackoverflow.com/questions/15234959/cross-compiling-for-arm-with-autoconf
.. _GNU website: https://www.gnu.org/software/automake/manual/html_node/Cross_002dCompilation.html
