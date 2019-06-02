---
title: More fine grained logging control in Python applications
date: 2019-06-02
tags: python, logging
category: Programming
authors: Tobias Pleyer
summary: "This post describes a pattern for better control over where log messages are produced."
---

# More fine grained logging control in Python applications

**Note:** This post is pretty Python specific, but most of the ideas should be
applicable in any other programming language as well.

[In a previous post](./2019-04-30-Logging-pitfall-in-performance-critical-situations.html)
I wrote about the implications of overly using logs in performance critical
blocks, even when the log level is set high. The bottom line of that post was:
**even little things can add up and logs always cost something due to strict
evaluation**.

The above mentioned previous post was concerned about making logs in
performance critical situations cheaper. In this blog post I want to write
about a very similar problem: **reducing the amount of logs**.

I want to state the problem with a fictitous scenario: Our project consists of
20,000 lines of code distributed over 100 different files. Because we are good
programmers all the source code in every file is well documented and a lot of
logging is available if we need it. Now a bug is filed. We can pin the problem
down to one of the 100 files. So to see wht's going wrong we set the log level
to DEBUG and run the program. Sadly our application is highly concurrent and
**when we look at the log it is polluted with unwanted messages**. Before we
can find the faulty code we have to apply a lot of filtering and searching.
Unfortunately this is the case for every bug and every time the filtering and
searching is different to the last time. We waste a lot of time just getting
the right information.

What's the problem? The problem is that the log level is valid for the whole
project. Most often logging is set up in the standard way: 1 log level with 5
possible criticalities (debug, info, warning, error, critical) and all loggers
listen to this one log level. Thus when we set the level to DEBUG logs will be
produced everywhere in every file, even though we only want the logs from one
file. No problem you say? The filtering of the log is easy you say? True! But
the point is: **these logs have been made!** If you are dealing with
performance critical code it is possible that this poses a serious problem.
**It's better to never log unnecessary stuff in the first place**.

The problem exists because one central log level is too coarse for a big and
mature project. What we want is more fine grained control on a per file level.

## Per file logging

The solution I want to present is a combination of two things: a logging
configuration and a setup function for the logger.

I provided a very simple Python project for the following discussion. Here is
the [full zip](../sources/sample_project.zip).

Let's start with the logging configuration:

::: {.code-include lexer="python" file="code/python_logging/sample_project/log_config.py"}
:::

The idea is that this file should contain every information about our logging
environment. Here are the explanations:

    (a) we import Python's standard logging module.
    (b) we set the global log level of our application
    (c) we write it to the basic configuration of the logging module
    (d) we define our log handler (a real project would not use the NullHandler)
    (e) the key idea: a dictionary that maps file names (modules) to a log level

I think the idea is already visible. We want to give every file it's own
private log level that we can adjust to our liking. In order for this to work
**every file in our project needs an entry in this dictionary**. If we add or
rename a file we have to adjust the dictionary accordingly. This however does
not pose a big constraint, because tasks like this are easily scripted, e.g. as
part of a *make* call.

Before I show the setup function I want to show how it is used in a module:

::: {.code-include lexer="python" file="code/python_logging/sample_project/moduleA.py"}
:::

As you can see in (a) we give the `setup_logger` function the name of the
current module via the special `__name__` variable and the function returns a
logger and the logger's log level. The reason we want the log level explicitely
is to be able to use lazy logging as described
[in my other post](./2019-04-30-Logging-pitfall-in-performance-critical-situations.html).
In the above example this happens in line (b).

Now to the `setup_logger` function:

::: {.code-include lexer="python" file="code/python_logging/sample_project/util.py"}
:::

Here's what's happening:

    (a) We use the standard logging module to give us a new logger for `name`,
        which the value of `__name__`, i.e. the module's name from which
        `setup_logger` is called
    (b) We set the propagate flag of the logger to *False*. This is important,
        because loggers are typically structured in a hierarchy and we don't
        want this logger to send its output to the handlers of the parent. This
        allows us to have per file logging output control
    (c) We set the default log level (of the base logger) in case the file does
        not have an entry in the dictionary
    (d) Now we lookup the file's private log level and set it for the logger

What follows is just sample code of setting up a handler and adding it to the
logger. Adjust this code to your needs.

Let's now come back to the original motivating scenario of the intro and apply
it to the [simple project](../sources/sample_project.zip). If we assume that we
know the problem is in *moduleA*, then all we have to do is set the log level
of this module to *DEBUG*, like so:

```python
PER_MODULE_LOG_LEVELS = {
    'moduleA': logging.DEBUG,
    'moduleB': logging.ERROR,
}
```

As a result we will now see the logging output of *moduleA*, and only of
*moduleA*! That this can make a considerable performance difference shows the
slightly pathological main function of our example project:

```bash
$ python3.6 Main.py
```

On my machine I get the following results:

Log Level moduleA  Log Level moduleB  Average time in s for 10 runs
-----------------  -----------------  -----------------------------
ERROR              ERROR              0.39561099929997
DEBUG              ERROR              12.451260923500376
DEBUG              DEBUG              24.318808462699963

And in the example we used just in memory logging! If we print to stdout or
file handles this gets even worse!

**Small extra task:** How do the times change when we are not using the lazy
logging trick, i.e. not put an if-statement before our logging calls?
