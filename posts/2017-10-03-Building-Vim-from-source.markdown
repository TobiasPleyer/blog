---
title:  "Building Vim from source"
date: 2017-10-03
tags: vim
category: Programming
authors: Tobias Pleyer
summary: "Step by step instruction to build the vim editor from source"
---

Building Vim from source
========================

Steps
-----

For a quick self reference. More details can be found on the original
[wiki
page](https://github.com/Valloric/YouCompleteMe/wiki/Building-Vim-from-source).

### Install pre-requisites

``` {.sourceCode .bash}
sudo apt-get install libncurses5-dev libgnome2-dev libgnomeui-dev \
    libgtk2.0-dev libatk1.0-dev libbonoboui2-dev \
    libcairo2-dev libx11-dev libxpm-dev libxt-dev python-dev \
    python3-dev ruby-dev lua5.1 lua5.1-dev libperl-dev git
```

### Remove old version

``` {.sourceCode .bash}
sudo apt-get remove vim vim-runtime gvim
```

### Build it

**Notes**

-   Only compile with either Python2 or Python3 support
-   Make sure you have the correct Python config dir! In my case:
    */usr/lib/python3.5/config-3.5m-x86\_64-linux-gnu* for Python3

``` {.sourceCode .bash}
cd ~/Git
git clone https://github.com/vim/vim.git
cd vim
./configure --with-features=huge \
            --enable-multibyte \
            --enable-rubyinterp=yes \
            --enable-pythoninterp=yes \
            --with-python-config-dir=/usr/lib/python2.7/config-x86_64-linux-gnu \
            --enable-python3interp=yes \
            --with-python3-config-dir=/usr/lib/python3.5/config-3.5m-x86_64-linux-gnu \
            --enable-perlinterp=yes \
            --enable-luainterp=yes \
            --enable-gui=gtk2 \
            --enable-cscope \
            --prefix=/usr/local
make VIMRUNTIMEDIR=/usr/local/share/vim/vim80
sudo make install
```

### Install as default on the system

``` {.sourceCode .bash}
sudo update-alternatives --install /usr/bin/editor editor /usr/local/bin/vim 1
sudo update-alternatives --set editor /usr/local/bin/vim
sudo update-alternatives --install /usr/bin/vi vi /usr/local/bin/vim 1
sudo update-alternatives --set vi /usr/local/bin/vim
```