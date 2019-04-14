Linking Rust libraries
######################

:date: 2017-08-01
:tags: linux, rust, linking, x11
:category: Programming
:authors: Tobias Pleyer
:summary: I learned an interesting lesson when I tried to link a Rust program against X11

My attempt to link Rust against X11
===================================

Yesterday I tried to write a simple program in Rust that links to X11. So I googled for a library and found `x11-rs`_. This library also comes with a couple examples. I chose the `hello-world.rs`_.

.. _x11-rs: https://github.com/Daggerbot/x11-rs
.. _hello-world.rs: https://github.com/Daggerbot/x11-rs/blob/master/x11/examples/hello-world.rs

Because I plan to write a real application I already had a very basic directory structure

.. code-block:: bash

    $ tree
    .
    ├── Cargo.lock
    ├── Cargo.toml
    ├── README.rst
    └── src
        ├── lib.rs
        ├── main.rs
        └── x11_api
            └── mod.rs

At that moment *lib.rs* and *mod.rs* were basically empty

.. code-block:: rust

    // lib.rs
    pub mod x11_api;

.. code-block:: rust

    // mod.rs
    // empty

*main.rs* was copied from the example. "Let's go", I thought

.. code-block:: bash

    $ cargo build
    ...
    Compiling rst v0.1.0 (file:///home/tobias/Git/rst)
    error: linking with `cc` failed: exit code: 1
      |
      = note: "cc" "-Wl,--as-needed" "-Wl,-z,noexecstack" "-m64" "-L" "/home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib" "/home/tobias/Git/rst/target/debug/deps/rst-16ed183b13c8f886.0.o" "-o" "/home/tobias/Git/rst/target/debug/deps/rst-16ed183b13c8f886" "-Wl,--gc-sections" "-pie" "-nodefaultlibs" "-L" "/home/tobias/Git/rst/target/debug/deps" "-L" "/home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib" "-Wl,-Bstatic" "-Wl,-Bdynamic" "/home/tobias/Git/rst/target/debug/deps/libx11-fe9add686c220c9a.rlib" "/home/tobias/Git/rst/target/debug/deps/liblibc-0b2eb2af9c1c96b4.rlib" "/home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/libstd-13f36e2630c2d79b.rlib" "/home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/librand-a2ef7979b4b3e1d5.rlib" "/home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/libcollections-d22754c8c52de3a1.rlib" "/home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/libstd_unicode-1cc5fcd37568ebc4.rlib" "/home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/libpanic_unwind-3b9d178f1de89528.rlib" "/home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/libunwind-93bb403c9fc56f72.rlib" "/home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/liballoc-c53f99154bf815c4.rlib" "/home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/liballoc_jemalloc-f1bb04f5989dcb98.rlib" "/home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/liblibc-739908a2e215dd88.rlib" "/home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/libcore-3f4289353c600297.rlib" "/home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/libcompiler_builtins-07bfb3bcb2a51da0.rlib" "-l" "util" "-l" "dl" "-l" "rt" "-l" "pthread" "-l" "gcc_s" "-l" "pthread" "-l" "c" "-l" "m" "-l" "rt" "-l" "util"
      = note: /home/tobias/Git/rst/target/debug/deps/rst-16ed183b13c8f886.0.o: In function `rst::main':
              /home/tobias/Git/rst/src/main.rs:27: undefined reference to `XOpenDisplay'
              /home/tobias/Git/rst/src/main.rs:34: undefined reference to `XDefaultScreen'
              /home/tobias/Git/rst/src/main.rs:35: undefined reference to `XRootWindow'
              /home/tobias/Git/rst/src/main.rs:38: undefined reference to `XWhitePixel'
              /home/tobias/Git/rst/src/main.rs:40: undefined reference to `XCreateWindow'
              /home/tobias/Git/rst/src/main.rs:48: undefined reference to `XStoreName'
              /home/tobias/Git/rst/src/main.rs:54: undefined reference to `XInternAtom'
              /home/tobias/Git/rst/src/main.rs:55: undefined reference to `XInternAtom'
              /home/tobias/Git/rst/src/main.rs:59: undefined reference to `XSetWMProtocols'
              /home/tobias/Git/rst/src/main.rs:62: undefined reference to `XMapWindow'
              /home/tobias/Git/rst/src/main.rs:68: undefined reference to `XNextEvent'
              /home/tobias/Git/rst/src/main.rs:88: undefined reference to `XCloseDisplay'
              collect2: error: ld returned 1 exit status
              

    error: aborting due to previous error

    error: Could not compile `rst`.

    To learn more, run the command again with --verbose.

Hm? As the message suggests *cc* cannot link against X11, which is no surprise because the above command misses the option *-l X11*. The proof (note the *-lX11* at the end):

.. code-block:: bash

    $ cc -Wl,--as-needed -Wl,-z,noexecstack -m64 -L /home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib /home/tobias/Git/rst/target/debug/deps/rst-16ed183b13c8f886.0.o -o /home/tobias/Git/rst/target/debug/deps/rst-16ed183b13c8f886 -Wl,--gc-sections -pie -nodefaultlibs -L /home/tobias/Git/rst/target/debug/deps -L /home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib -Wl,-Bstatic -Wl,-Bdynamic /home/tobias/Git/rst/target/debug/deps/libx11-fe9add686c220c9a.rlib /home/tobias/Git/rst/target/debug/deps/liblibc-0b2eb2af9c1c96b4.rlib /home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/libstd-13f36e2630c2d79b.rlib /home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/librand-a2ef7979b4b3e1d5.rlib /home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/libcollections-d22754c8c52de3a1.rlib /home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/libstd_unicode-1cc5fcd37568ebc4.rlib /home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/libpanic_unwind-3b9d178f1de89528.rlib /home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/libunwind-93bb403c9fc56f72.rlib /home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/liballoc-c53f99154bf815c4.rlib /home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/liballoc_jemalloc-f1bb04f5989dcb98.rlib /home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/liblibc-739908a2e215dd88.rlib /home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/libcore-3f4289353c600297.rlib /home/tobias/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/libcompiler_builtins-07bfb3bcb2a51da0.rlib -l util -l dl -l rt -l pthread -l gcc_s -l pthread -l c -l m -l rt -l util -lX11
    
    $

No error. But why? Why is cargo not linking correctly? I read the cargo docs up and down and ended up with a *build.rs* file and a modified *Cargo.toml*.

.. code-block:: rust

    // build.rs    
    fn main() {
        println!("cargo:rustc-link-lib=X11");
    }

.. code-block:: toml

    // Cargo.toml   
    [package]
    name = "test"
    version = "0.1.0"
    links = "X11"
    build = "build.rs"

    [features]
    xlib = []

    [dependencies]
    libc = "0.2.28"
    x11 = "2.14.0"

That's got to be it, right? Both the *links* keyword and the *build.rs* file clearly reference the X11 library. The result remained the same! This problem drove me crazy. I spent hours to solve it.

Then an idea came to mind. Is it possible that the library takes precedence if present? I moved all the code to *mod.rs* and just called the function from *main.rs*. **The result was a XWindow popping up.** Wow! To prove the assumption I removed the files *lib.rs* and *mod.rs* entirely and moved all program logic back to *main.rs*.

**Compiled and ran!**

Everything else remained unchanged. I am not sure about the why. I didn't read that anywhere, but probably this behaviour is documented somewhere. I guess the philosophy Rust tries to enforce here is that if we have a library, it should be the place to put all the main program logic (it's a library right?). The main shall be reduced to a bare minimum just calling into the library and other crates and Rust doesn't link against it. 
