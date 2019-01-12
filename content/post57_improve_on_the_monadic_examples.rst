Monadic computations in Python revisited
########################################

:date: 2019-01-12
:tags: python, monads, functional programming
:category: Programming
:authors: Tobias Pleyer
:summary: Extending the monadic computation examples from a previous post


Recap
=====

In a `previous post <{filename}/post55_monadic_computations_in_python.rst>`_ I
introduced monadic computations for Python. Make sure you read that post first
before continuing with this one.

In this post I want to show how easy it is to extend the functionality towards
logging based on a verbosity level for the application.

Verbosity based logging
=======================

The verbosity level is normally given via the command line. Writing a CLI is
not part of this post, so to make it simple the application will take just one
argument: the verbosity level given as a number.

**Goal:** *Based on the provided verbosity level the application will provide
out of a given degree of detail. The higher the verbosity level the higher the
degree of detail.*

Verbosity as part of the log
----------------------------

Our idea is very simple: Instead of just providing a list of strings we now
provide a list of tuples, each tuple consisting of an importance level and the
actual string. The lower the importance level, the higher the importance, i.e.
it is inverse propertional to the verbosity level.

The only changes we have to make to our library code are in the `do_shell`
function:

.. code-include:: code/post57/new_shell.py
    :lexer: python

The main code can remain almost unchanged. All we have to do is read the
verbosity level from our command line and filter the log based on the log
level:

.. code-include:: code/post57/app.py
    :lexer: python

Here is the full code for the example.

.. code-include:: code/post57/full_example.py
    :lexer: python

I also provided a definition for the very useful helper function `compose` and
`sequence`. They are not strictly necessary for the example, but it is very
neat to be able to write chained function calls like that. Compose lets you
create a new function that will thread its arguments through the given chain of
functions. Sequence is compose, but takes the initial argument directly and
then returns the final result directly instead of a function.

We can verify now that different verbosity levels lead to different output:

.. code::

    $ python full_example.py 1
    Final result: Right 'Even better\n'
    == INFO ==
    some warning

    Final result: Left 'Command failed\n'
    == INFO ==
    Command failed


    $ python full_example.py 2
    Final result: Right 'Even better\n'
    == INFO ==
    OK

    Also OK

    some warning

    Even better

    Final result: Left 'Command failed\n'
    == INFO ==
    OK

    Command failed


    $ python full_example.py 3
    Final result: Right 'Even better\n'
    == INFO ==
    Command run: echo 'OK'; exit 0
    OK

    Command run: echo 'some warning' >&2; echo 'Also OK'; exit 0
    Also OK

    some warning

    Command run: echo 'Even better'; exit 0
    Even better

    Final result: Left 'Command failed\n'
    == INFO ==
    Command run: echo 'OK'; exit 0
    OK

    Command run: echo 'Command failed' >&2; exit 1
    Command failed



However the example is too simple to guard against misuse:

.. code::

    $ python full_example.py 0
    Final result: Right 'Even better\n'
    == INFO ==
    Traceback (most recent call last):
      File "full_example.py", line 220, in <module>
        main(verbosity_level)
      File "full_example.py", line 208, in main
        print(filter_log(verbosity_level, info))
      File "full_example.py", line 196, in filter_log
        partial(reduce, lambda x, y: x + '\n' + y)
      File "full_example.py", line 188, in sequence
        return compose(*flist[1:])(f0)
      File "full_example.py", line 174, in helper
        f0 = fn(f0)
    TypeError: reduce() of empty sequence with no initial value

Nothing that couldn't be solved, but this is not the purpose here. I want to
stress that the actual changes necessary to achieve the new functionality
required less than 10 lines of code!
