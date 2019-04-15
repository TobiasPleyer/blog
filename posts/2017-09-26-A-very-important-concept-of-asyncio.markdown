---
title:  "A very important concept of asyncio"
date: 2017-09-26
tags: python, asyncio
category: Programming
authors: Tobias Pleyer
summary: "An insight into the the inner workings of Python's asyncio library"
---

A very important concept of asyncio
===================================

**Disclaimer**: Everything I mention in this post refers to the version
of Python that is tagged **v3.6.0rc1** in [Python\'s Git
repository](https://github.com/python/cpython.git).

A simple example
----------------

::: {.code-include lexer="python"}
code/asyncio\_hello\_world\_naive.py
:::

And run it:

    $> python3.6 asyncio_hello_world_naive.py
    Return from coro.send: <Future pending>
    Traceback (most recent call last):
      File "content/code/asyncio_hello_world_naive.py", line 13, in <module>
        coro.send(None)
      File "content/code/asyncio_hello_world_naive.py", line 5, in slow_operation
        await asyncio.sleep(1)
      File ".../lib/python3.6/asyncio/tasks.py", line 476, in sleep
        return (yield from future)
    AssertionError: yield from wasn't used with future

Oops... What happened here? Didn't I (a)wait 1 second and then set the
future's result? Nope! We missed an important piece of the puzzle. The
line *coro.send(None)* is driving the coroutine, but this driver is not
smart enough. It is not obvious to see the problem right away, because
many new features of Python apply here and because the vocabulary is
deceptive, especially if you are new to the language.

Let's first have a look at code that has the desrired effect and then
dive deeper into the implementation. This code snippet is almost
identical to an example that can be found in asyncio's documentation

::: {.code-include lexer="python"}
code/asyncio\_hello\_world.py
:::

What is happening here? What does *asyncio.sleep* actually do? Let's
have a look at the source code:

::: {.code-include lexer="python"}
code/asyncio\_sleep.py
:::

Ok. So a [future
object](https://docs.python.org/3/library/asyncio-task.html#future) is
created and the function that is going to set the future's result is
scheduled for the desired delay of the *sleep* function. Sounds
reasonable so far, but then we yield the future. What does that do?
Again let's have a look at the source code:

::: {.code-include lexer="python"}
code/asyncio\_future.py
:::

So in the begining, when the future is usually not done yet, the future
will yield itself. One line below the yield you can see the assert that
came back on us in the naive example. It is caused because when we used
*coro.send(None)* the second time, we reach the assert but the future is
not done at that point. We could cheat and set the result ourselves

::: {.code-include lexer="python"}
code/asyncio\_hello\_world\_naive2.py
:::

And run it:

    $> python3.6 asyncio_hello_world_naive2.py
    Return from coro.send: <Future pending>
    Hello World!

This runs to completion, but there is no perceivable delay and the code
is less than intuitive...

### Coroutines - a quick detour

When we write **await coro** this is sematically (almost) equivalent to
**yield from coro**. A *yield from* chain eventually must end in a bare
yield. Said simple we are diving deeper down a yield from chain until we
find a yield. In case of *asyncio.sleep* the next diving level is
already inside the asyncio core. This means whatever is controlling the
original coroutine *slow operation* must be able to handle objects that
are yielded from within the asyncio core. That's why I called the above
example naive. Simply running the coroutine by sending *None* to it is
not enough, we have to be able to deal with the yielded value, the
future object, as in the second naive example but with much more
sophistication.

### The Task class

As shown in the working example the key to success is to use functions
provided by the asyncio framework. That is why the asyncio framework
feels alot like all-in. It has the same touch as [Haskell\'s IO
monad](https://hackage.haskell.org/package/base-4.10.0.0/docs/System-IO.html):
once you enter this realm you can't leave it anymore.

And that is the reason why we need the above calls to **ensure\_future**
and **run\_until\_complete**. From that point on the asyncio system is
taking control again and installs a coroutine driver that knows what to
do:
[asyncio.Task](https://docs.python.org/3/library/asyncio-task.html#task).
Directly quoting from the documentation:

> *A task is responsible for executing a coroutine object in an event
> loop. If the wrapped coroutine yields from a future, the task suspends
> the execution of the wrapped coroutine and waits for the completion of
> the future. When the future is done, the execution of the wrapped
> coroutine restarts with the result or the exception of the future.*

This describes the situation of our example, where the future that we
are yielding from results from the call to **asyncio.sleep**. Inside the
task class is the point where the event loop is taking and relinquishing
control, the event loops interface point to the external world so to
speak.

Without further ado here come the most interesting bits of the *Task*
class

::: {.code-include lexer="python"}
code/asyncio\_task.py
:::

The key function is the *\_step* function. As can be seen in the
definition, it also performs:

    result = coro.send(None)

But unlike in our naive example this entails extensive evaluation of the
return value, plausibility checks and bookkeeping. Depending on the
return value of the coroutine, the *\_step* function either returns the
final result, raises an exception or puts itself on sleep to be woken up
again at some later time (*\_wakeup*). Step is a very well suited name
because every invocation advances the original coroutine to the next
await statement, thus step by step reaching the end.