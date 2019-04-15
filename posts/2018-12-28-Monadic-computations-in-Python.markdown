---
title:  "Monadic computations in Python"
date: 2018-12-28
tags: haskell, python, monads, functional programming
category: Programming
authors: Tobias Pleyer
summary: "Showcasing the effective use of monads to reduce boilerplate in Python"
---

Monadic computations in Python
==============================

Monads
------

Monads are a construct known from category theory. Newcomers to Haskell
quickly connect the term with mixed feelings, because the concept is
rather strange in the beginning, especially if you already know another
'traditional' language, but heavily used within the Haskell programming
language. The first contact with them is usually when [doing
IO](https://www.haskell.org/tutorial/io.html) in combination with
[do-notation](https://en.wikibooks.org/wiki/Haskell/do_notation).

Why are monads used so much in Haskell? Because they allow to sequence
computations in a functional manner. In addition monads can hide
behavioral boilerplate within their definitions: Define the behavior
once for the monad instance and get it 'for free' with do-notation.

Once the concept of a monad is understood the next level of insight are
so called monad transformers. Monad transformers are monads stacked atop
a base monad, i.e. monad stacks. Every stack layer adds another piece of
functionality and the final resulting monad posses all of these.

This post is not meant as an introduction to monads. Many excellent
books cover this topic and *yet another post about monads* does not add
any good. However the benefit of monads is not limited to the Haskell
programming language. In this post I want to show how monads can be used
in Python.

**Note:** The code samples in this post require at least Python v3.6.

Python, IO and Monads
---------------------

Python is an imperative language and thus fairly different from Haskell,
which is considered a functional language. Whilst IO is explicit in
Haskell it can occur anywhere within Python source code. Monads are not
necessary to sequence computations in Python, statements and expressions
are simply executed from top to bottom. Another difference is that
Haskell is a strongly-typed compiled language whereas Python is a
dynamic interpreted language. Thus from a language theory point of view
both languages are situated in completely opposite corners of the scale.

However even though you don't need the concept of a monad as a Python
programmer, doesn't mean that you can't benefit from the ideas of
monadic computations to represent behavior. In the following sections I
want to start with a real world scenario and a rather typical approach
to the problem and then evolve step by step to a monadic solution.

**Note:** There is no single truth about when a programming language is
considered a functional programming language. For an interesting
discussion about this see [this post of Michael
Snoyman](https://www.fpcomplete.com/blog/2018/10/is-rust-functional).
However the important thing is: **even imperative languages allow for a
functional style of programming**. The main differences are just to what
extent this kind of style is supported by the language's syntax,
semantics and the standard library.

To implement monads in Python I will use the following concepts:

> -   Closures
> -   Lambda functions
> -   Higher order functions
> -   Operator overloading

The Challenge
-------------

Let me introduce the task we try to solve. The example is taken from the
real world and is actually something I had to do in my job recently.

We want to control an external program with a Python script. A very
typical example for such a program is [git](https://git-scm.com/). Git
is an amazing version control system and I use it a lot at work. There
are a lot of graphical tools to support working with git but my favorite
is the plain old terminal interface. However working with git requires
to know a lot of different commands and to execute them in a certain
order and potentially every single command can fail. Git follows the
UNIX convention that a return code greater than zero signals failure.

In [continuous
integration](https://en.wikipedia.org/wiki/Continuous_integration)
scenarios you usually have to execute a lot of these git commands and of
course you don't want to continue if the previous command already
failed. Likewise you want to know what has been done, in the good and
bad case, so standard out and error should be captured.

So let's sum up the requirements:

> 1.  We want to call an external program from within our Python code
> 2.  Every invocation of the program can potentially fail
> 3.  If a command fails we don't want to continue, but instead give a
>     meaningful error message.
> 4.  We want to capture standard out and standard error of the program

**Note:** In the following I will not actually use git commands, but
rather invoke some dummy shell scripts that either exit with zero or
some value greater than zero, but this does not harm the generality of
the following code snippets.

Imperative Solution
-------------------

This solution will serve as the reference solution. Of course there are
many ways to fulfill the above requirements but I think the solution I
give is rather realistic and pythonic.

### Brute Force Solution

The [run]{.title-ref} function of the
[subprocess](https://docs.python.org/3.6/library/subprocess.html) module
takes a [check]{.title-ref} parameter. If this is set to
[True]{.title-ref} the return value of the process is checked and an
exception is thrown in case it signals error. Thus the probably simplest
method is to simply put everything in a try/except block.

::: {.code-include lexer="python"}
code/post55/brute\_force.py
:::

``` {.sourceCode .bash}
$ python naive.py
Now running: echo 'OK'; exit 0
OK

Now running: echo 'Again OK'; exit 0
Again OK

Now running: echo 'Problem!'; exit 1
An error occurred. Aborting
```

However raising exceptions is definitely not a good programming praxis
and you have to make sure to catch them, because exceptions will bubble
up! This is maybe ok if the users are programmers, but it is very ugly
if an end user is confronted with a bunch of exception and stack trace
outputs instead of a comprehensible error messages.

### Naive Solution

As the next iteration to the problem the shell function will return a
boolean signaling sucess/failure and the main code will check every
single invocation.

::: {.code-include lexer="python"}
code/post55/naive.py
:::

Notice how repetitive this feels and looks. What's even worse: It's hard
to see what is the program actually trying to do! Sometimes you also see
something like this:

::: {.code-include lexer="python"}
code/post55/naive2.py
:::

This is slightly more readable, but the constantly growing indentation
depth scales badly and notice that the error message is now baked into
the shell function, which cuts down on reusability.

### Better Solution - Higher Order Functions

Using higher order functions (functions taking functions as arguments)
we can feed our list of functions (note we do not execute them at that
point!) to a driver function. The driver then executes the functions one
after another until either one of them fails or the end is reached.

::: {.code-include lexer="python"}
code/post55/better.py
:::

This solution is a lot more readable and avoids code duplication. The
[shell]{.title-ref} function produces a delayed computation (aka thunk)
of the desired shell invocation using a lambda function. The underlying
[do\_shell]{.title-ref} function is kept general, simple and can be
easily reused.

There is a problem though. We are just printing the contents of standard
out. For simple scripts that's often all we need. But what if we want to
write the contents to a file? Or filter the error messages? We can't.

Or maybe we don't want to always print out everything. In this case we
have some sort of verbosity level and we only use the print statement if
the verbosity is high enough. This will further clutter our code with
if/else statements.

### Final Solution - Collecting stdout

As the final solution we will collect the stdout values and return a
list of them. This gives maximum control over the contents and allows
things like filtering or verbosity to be handled in the main function
instead of the low level code.

::: {.code-include lexer="python"}
code/post55/better2.py
:::

Most people don't realize that this final solution is basically a
hand-written monadic computation. We will see more of this in the coming
sections.

Monadic Solution
----------------

Before I dive into the final section I want to spend a few words to
recapitulate the involved monads and how they work. Our final solution
will be an EitherT transformer stacked on top of a WriterT transformer
stacked on top of the IO monad.

### The Either Monad

An invocation of our program can either succeed or fail. This *either
or* behavior is implemented by the either monad. In Haskell the either
monad, or the either type class, has to constructors, called *Left* and
*Right*. By convention *Left* signals an error and *Right* signals
success.

``` {.sourceCode .haskell}
data  Either a b  =  Left a | Right b

-- example for a left value
let leftEx = Left "Error"
-- example for a right value
let rightEx = Right 42
```

The interesting part is the monad instance of Either:

``` {.sourceCode .haskell}
instance Monad (Either e) where
    return = Right
    Left  l >>= _ = Left l
    Right r >>= k = k r
```

In short: Whenever we see a *Left* value in the chain we ignore
everything following that and instead return the *Left* value as the
result of the whole computation:

::: {.code-include lexer="haskell"}
code/post55/either\_demo.hs
:::

``` {.sourceCode .bash}
$ ./either_demo.hs
Right 4
Left "Something bad happened"
```

### The Writer Monad

We want to capture the content of standard out and standard error for
every invocation of the external program. This means we want to persist
(write) the content somewhere instead of loosing it. In Haskell the
monad responsible for that is the writer monad.

``` {.sourceCode .haskell}
newtype Writer w m a = Writer { runWriter :: (a, w) }

-- example for a writer value
let writerEx = Writer (1,"String with information")

instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
```

In short: The first element of the tuple is used to drive the
computation as the input to the rest of the computation while the second
element accumulates the information value.

::: {.code-include lexer="haskell"}
code/post55/writer\_demo.hs
:::

``` {.sourceCode .bash}
$ ./writer_demo.hs
(3,"Hello World!!!")
```

### The IO Monad

As the name says the IO monad is responsible for Input and Output, i.e.
the bi-directional communication with *"the outside world"*. In Haskell
the definition of the IO monad is opaque, it is not possible to unwrap
IO values except from within the IO monad. This ensures the
encapsulation and the partition between pure and impure code that
Haskell is famous for.

::: {.code-include lexer="haskell"}
code/post55/io\_demo.hs
:::

``` {.sourceCode .bash}
$ ./io_demo.hs
hi
HI!
```

### Rebuilding Monads in Python

**Note:** There already exists a Python library that provides
implementations for Functor, Applicative and Monad called
[OSlash](https://github.com/dbrattli/OSlash). I was not fully convinced
by their implementation. It felt a tiny bit to *OO* for my taste and and
I had the impression that it is very hard to implement monad
transformers on top of IO with their code. What follows below is my
proto-typical code to implement monadic stacks in Python, but my
implementation is strongly influenced by *OSlash*, e.g. the operator
overloading.

#### Either in Python

::: {.code-include lexer="python"}
code/post55/either\_demo.py
:::

``` {.sourceCode .bash}
$ python either_demo.py
Right 4
Left 'Something bad happened'
Right 2
Left 'nok'
```

This example uses operator overloading for classes to support a nicer to
read infix operator syntax very similar to Haskell. So basically the
only reason that we are using classes here is for nicer syntax (operator
overloading) and to store information if we have a *Left* or a *Right*
value.

#### IO in Python

An IO action in Python is just a deferred call to a function that does
IO. Like in Haskell we don't want IO actions to do anything until they
are actually required (run). This mimics the behavior of lazy
evaluation.

::: {.code-include lexer="python"}
code/post55/io\_demo.py
:::

``` {.sourceCode .bash}
$ python io_demo.py
Doubling 1 -> 2
```

#### Monad Transformers in Python

As I mentioned above we want the functionality of *Either* and *Writer*.
Thus we have to stack these atop our *IO* monad. Before I go into
details, here is the full code:

::: {.code-include lexer="python"}
code/post55/transformers.py
:::

``` {.sourceCode .bash}
$ python3.6 transformers.py
Final result: Right 'Even better\n'
== INFO ==
Command run: echo 'OK'; exit 0
OK

Command run: echo 'Also OK'; exit 0
Also OK

Command run: echo 'Even better'; exit 0
Even better

Final result: Left 'Command failed\n'
== INFO ==
Command run: echo 'OK'; exit 0
OK

Command run: echo 'Command failed' >&2; exit 1
Command failed
```

As can be seen from the examples the command sequence short-circuits in
case one of the commands fails. The implementations for *EitherT* and
*WriterT* are as close as possible to that of Haskell's
[WriterT](https://hackage.haskell.org/package/transformers-0.5.5.0/docs/src/Control.Monad.Trans.Writer.Lazy.html#line-194)
nad
[EitherT](https://hackage.haskell.org/package/either-4.4.1.1/docs/src/Control.Monad.Trans.Either.html#line-226).
The implementation of [\_\_or\_\_]{.title-ref} ([\|]{.title-ref})
corresponds to [(\>\>=)]{.title-ref} in Haskell and that of
[\_\_rhift\_\_]{.title-ref} ([\>\>]{.title-ref}) corresponds to
Haskell's [(\>\>)]{.title-ref}. As in Haskell we have to unwrap the
wrapped [IO]{.title-ref} action before we can run it. The helper
function [runAction]{.title-ref} does the job for us.

There exist a few technical details that are necessary to make this code
run under Python:

> -   Because Python is not typed, we have to use one of the wrapped
>     inner monadic values to retrieve the return function (constructor)
>     of the inner monad of the transformer, e.g.
>     [self.unwrap().get\_return()]{.title-ref}
> -   When using the bind operation we have to enclose the following
>     lambda expression in parenthesis so the code can be correctly
>     lexed and parsed
> -   Python does not have do-notation, so no syntactic sugar
> -   In Haskell unwrapping the value of monads is done with descriptive
>     names, e.g. [runWriterT]{.title-ref} or [runEitherT]{.title-ref},
>     I use the generic [unwrap]{.title-ref} function for this. In that
>     way I don't have to deal with type information, because I assume
>     that every monad class has that method

Comparison to the hand-written Code
-----------------------------------

If we carfully reconstruct the logic added with every monad transformer
level we can see that the monadic solution is actually equivalent to the
hand-written code at the beginning of the post. At least in the way we
used it in the examples.

In the hand-written code we used boolean values to signal success or
failure. The *Either* type is more powerful than that, because it can
also store content, but we haven't used it here. We simply returned
[None]{.title-ref}, which then carries the equally much information as
the boolean values ([Left]{.title-ref} is [False]{.title-ref} and
[Right]{.title-ref} is [True]{.title-ref}).

The monadic code saves the continuation within a closure, the
hand-written code uses an explicit for loop.

The information from stdout is kept in a global variable within function
scope in case of the hand-written solution, whereas the monadic solution
explicitly hands over the updated value to the next function call.

So why bother with all this extra code when the hand-written solution is
so similar in functionality? There are a couple of reasons.

### Reusability

The *IO*, *Either*, *Writer*, *EitherT* and *WriterT* classes are
self-contained and thus fully reusable. It is possible to build other
monadic stacks with them. This is not true for the hand-written
solution.

### Composability

Composability is an interesting aspect and shouldn't be neglected! The
sample code I presented is very basic, but a full featured continuous
integration script for a complex project might grow to considerable
size. It is very likely that the overall command sequences are made of
smaller blocks which are frequently repeating. In this case it would be
nice to be able to compose the higher level command blocks of the
smaller 'unit' command blocks.

#### Example

We want to retrospectively tag a bunch of commits in a git repository
and also print information about the commit:

::: {.code-include lexer="python"}
code/post55/git\_tagging\_example.py
:::

``` {.sourceCode .bash}
$ cd example_git_repo
$ python ../git_tagging_example.py
Final result: Left None
== INFO ==
Command run: git show df40cfe
commit df40cfe4876c9bf57254572762fa9fa669ce0ef3
Author: TobiasPleyer <tobi.pleyer@gmail.com>
Date:   Sun Dec 30 10:57:58 2018 +0100

    First commit

diff --git a/README b/README
new file mode 100644
index 0000000..3609f20
--- /dev/null
+++ b/README
@@ -0,0 +1 @@
+One

Command run: git tag -m 'First release' v1.0 df40cfe

Command run: git show 851a93b
commit 851a93b45397a12358ba697108e05d88928e6584
Author: TobiasPleyer <tobi.pleyer@gmail.com>
Date:   Sun Dec 30 10:58:35 2018 +0100

    Second commit

diff --git a/README b/README
index 3609f20..3b0086f 100644
--- a/README
+++ b/README
@@ -1 +1 @@
-One
+Two

Command run: git tag -m 'Second release' v2.0 851a93b

Command run: git show 961090f
commit 961090f6a727b9c368a1913a8136245aad1d55ee
Author: TobiasPleyer <tobi.pleyer@gmail.com>
Date:   Sun Dec 30 10:59:18 2018 +0100

    Third commit

diff --git a/README b/README
index 3b0086f..b2cde18 100644
--- a/README
+++ b/README
@@ -1 +1 @@
-Two
+Three

Command run: git tag -m 'Third release' v1.0 961090f
fatal: tag 'v1.0' already exists
```

The [git show]{.title-ref}/[git tag]{.title-ref} block forms the unit
and we chain several of these units together to form a bigger block.
This will just result in a bigger monadic computation that retains the
same behavior as if defined as one big block. That's the beauty of
composability.

But this can also be achieved in the hand-written solution. If we define
an action as the list of shell command lambdas then composition can be
done via list addition:

::: {.code-include lexer="python"}
code/post55/git\_tagging\_example2.py
:::

But once again: This shouldn't surprise us because as I mentioned above
the logic in [run\_in\_sequence]{.title-ref} is basically identical to
our monadic code in case we are simple sequencing computations.

### Branching

Branching means doing different things, i.e. taking different code
execution paths depending on conditionals. Typically the execution of an
action influences the behavior or execution of the following actions.

This is where the monadic computations really start to shine. So far we
have only looked at blindly sequencing commands after each other with
the [(\>\>)]{.title-ref} operator. This is because the commands were
independent of each other. But what if we want to do different things
depending on the output of previous commands? In Haskell we would use
the [(\>\>=)]{.title-ref} function, in our Python code we use the
[(\|)]{.title-ref} operator for that.

#### Example

We want to execute different commands depending on the branch we are
currently checked out to.

::: {.code-include lexer="python"}
code/post55/branching\_example.py
:::

``` {.sourceCode .bash}
$ cd example_git_repo
$ python ../branching_example.py
Final result: Right 3
== INFO ==
Command run: git branch
* master

Command run: echo 'action for master'
action for master

Command run: echo 'command independent of previous commands'
command independent of previous commands

Command run: echo master
master

Command run: git tag | wc -l
3

Command run: /bin/zsh -c 'for i in {1..3}; do; echo 'Branch!'; done'
Branch!
Branch!
Branch!

$ git tag newTag
$ git checkout -b other_branch
Switched to a new branch 'other_branch'
$ python ../branching_example.py
Final result: Right 4
== INFO ==
Command run: git branch
  master
* other_branch

Command run: echo 'Other action'
Other action

Command run: echo 'command independent of previous commands'
command independent of previous commands

Command run: echo other_branch
other_branch

Command run: git tag | wc -l
4

Command run: /bin/zsh -c 'for i in {1..4}; do; echo 'Branch!'; done'
Branch!
Branch!
Branch!
Branch!

$ cd /home
$ python branching_example.py
Final result: Left ''
== INFO ==
Command run: git branch
fatal: Not a git repository (or any parent up to mount point /home)
Stopping at filesystem boundary (GIT_DISCOVERY_ACROSS_FILESYSTEM not set).
```

As the example shows the monad's bind syntax can be used to change the
code execution path easily and readable plus we still retain the nice
and controlled behavior in case of an error. The variables bound by
lambdas will remain visible to all follow up actions! If more
sophisticated decision making is required that can't be done with the
limited lambda syntax in Python we can simply define a helper function
to do the job.

**Challenge:** Try to reproduce that kind of behavior with a
hand-written solution!

Some people would argue the above code samples look ugly and that the
used lambda function syntax is not very understandable and readable. I
can't deny that, but keep in mind that this is due to the lack of proper
syntactic sugar support in Python! Here is the same example written in
Haskell:

::: {.code-include lexer="haskell"}
code/post55/branchingHs/app/Main.hs
:::

Summary
-------

Even if you're not convinced at that point that monadic computations can
help you in everyday programming I think it is absolutely worth it to at
least know the bigger picture behind the above shown mechanics. The
concept of a monad is an extremely versatile and powerful one which
allows for arbitrarily complex computations. Haskell's IO system is the
proof that anything can be done with monads.
