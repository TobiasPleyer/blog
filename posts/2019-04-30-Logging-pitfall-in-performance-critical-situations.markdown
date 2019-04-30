---
title: Logging pitfall in performance critical situations
date: 2019-04-30
tags: 
category: Programming
authors: Tobias Pleyer
summary: "Often people think logging does not impact performance when the logging level is high, but this is a misconception!"
---

# Logging pitfall in performance critical situations

Logging is a well known and crucial means to trace and understand program
execution. But often people think of logging in a on/off manner: either the
given log statement is logged or not depending on the momentary loglevel.

While this conception is semantically true regarding the actual output to
stdout or the file system, this is not necessarily true for the performance
impact!

The reason being is that most programming language have what is called **strict
evaluation**. If I may quote wikipedia:

    In strict evaluation, the arguments to a function are always evaluated
    completely before the function is applied.

This means you still pay the cost of the argument evaluation if you are logging
via function calls, as is the case with Python's
[logging](https://docs.python.org/3/library/logging.html) library for example.

## Some simple benchmarks

Take this simple function with an inner loop that does not apply any logging:

```python
def f(n):
    s = 0
    for i in range(n):
        s += i*i
    return s
```

We can check how long it will take us with `n = 1_000_000`:

```python
import time
start = time.perf_counter()
f(1_000_000)
stop = time.perf_counter()
print(stop-start)
```

On my machine this gives:

```bash
$ python3.6 perf.py
0.05806570600179839
```

Now let's add logging. First we set the log level low enough so that our log
messages will be logged:

::: {.code-include lexer="python" file="code/python_logging/perf_debug.py"}
:::

which gives

```bash
$ python3.6 perf.py
9.789353045998723
```

That's ~170 times slower than without logging! Next we still use the same
logging code, but we set the log level so high that nothing will be logged.

```python
logger.setLevel(logging.ERROR)
```

Are we back to the original performance? Let's see:

```bash
$ python3.6 perf.py
0.8399945159981144
```

That's still ~14 times slower than the log free code! So logging doesn't come
for free, even if you set the log level very high! In many situations this
won't affect your application, but if you are dealing with tight inner loops
you should at least have an eye on that.

## Can we do better?

Yes we can. The key point is that we don't want to evaluate a string that will
never be used! Only if we really need it we evaluate it, i.e. we want to be
lazy!

In languages like C++ this can be dealt with via macros. Instead of making a
call to a function you call a macro that transforms the code appropriately at
compile time. Unfortunately we don't have this option in Python, so we have to
write it by hand. All we need to do is to write an explicit if-statement:

::: {.code-include lexer="python" file="code/python_logging/perf_error_if.py"}
:::

Are we back to the original performance? Let's see:

```bash
$ python3.6 perf.py
0.12178002899963758
```

Now we are just twice as slow if we don't log any message and almost at the
same speed when we do log messages (log level = DEBUG).

## Summary

Guarding every call to the logger with an explicit if-statement does definitely
not make your code more pretty, but if you find youself in a situation that
your code has to guarantee a certain performace and yet you need some debugging
information, the presented solution can help you.

**Little aside:** I was of course not the first person to discover this fact.
Indeed this is mentioned in Python's
[official documentation](https://docs.python.org/3/howto/logging.html#optimization),
but literally at the end. The documentation mentions the function
`isEnabledFor` which, in spirit, is equivalent to the simple comparison with
the logger's log level, but ~4 times slower than the simple if-clause I
presented! I don't know why that is, but for sure this function does smarter
things than just an int comparison...

As a last suggestion: There are also other knobs you can try to twist a little
in order to squeeze out more performance. For example to turn off propagation
of messages if you have a hierarchy of loggers. This is really a question of a
proper logging architecture and definitely deserves a deeper thought.
