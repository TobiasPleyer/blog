Sleeping for a certain amount of iterations
###########################################

:date: 2017-10-02
:tags: python, asyncio, patch
:category: Programming
:authors: Tobias Pleyer
:summary: A patch for asyncio's sleep function to sleep for a given amount of iterations

**Disclaimer**: The patch given in this article is verified and made for the
Python tag **v3.6.0rc1** in `Python's Git repository`_.

.. _Python's Git repository: https://github.com/python/cpython.git

An alternative for waiting
==========================

The standard implementation of Python's *asyncio.sleep*, which can be found in
*asyncio/tasks.py*, only allows to specifiy a seconds delay argument. But in
certain situations, maybe, one wants to wait for a given amount of event loop
iterations, instead of a given time delay.

This can be achieved easily by changing the implementation of *sleep*. Here is
the `patch <{filename}/static/improved_asyncio_sleep.patch>`_.

Implementation details
======================

Asyncio's loop function which called in every iteration is called **_run_once**.
Every callback deemed *ready* (in the *_ready* deque object) will be run there.
A small implementation detail is that the length of the current deque is frozen
when processing *_ready*, which has the effect that callbacks scheduled while
the callbacks are processed won't be processed in that run, but in the next.
With this knowledge the patch should be easy to understand:

    * We schedule the helper function initially
    * In each call the function decreases its *iterations* argument
    * As long as the argument is bigger than 0, the function will schedule
      itself (which will happen in the next iteration)
    * Eventually the future's result is set and the function exits, asyncio
      does the rest

Here is a small sample script to test the new functionality:

.. code-include:: code/asyncio_iteration_sleep_test.py
    :lexer: python
