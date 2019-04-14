Function Overloading in Python
##############################

:date: 2019-01-01
:tags: python
:category: Programming
:authors: Tobias Pleyer
:summary: A proof-of-concept implementation for function overloading in Python


Motivation
==========

An overloaded function exhibits a different behavior depending on the arguments
it is called with. This can be very useful if you want to maintain a consistent
interface, even though the function may have different arity or argument types.
For example see the following example in pseudo code:

.. code:: python

    # We store customer data in two formats:
    #   1. A hash table for name lookup
    #   2. An array for really fast index based lookup
    #
    # But we want to use the same function `lookup`

    customer1 = lookup("John Doe") # This will use the hash table
    customer2 = lookup(123456)     # This will use the array
    # ...

Since Python is a dynamic language it is quite possible that the arguments have
different types at runtime. The above example is slightly nicer to read than
the more explicit version:

.. code:: python

    customer1 = lookupByString("John Doe") # This will use the hash table
    customer2 = lookupByIndex(123456)      # This will use the array
    # ...

Python does not support function overloading out of the box and there are
certainly reasons to not use it at all. I won't discuss the pros and cons of
function overloading, the goal of this blog post is simply to show that there
is a way to implement function overloading in an elegant, maintainable and
readable way.

Implementation
==============

Hand-written
------------

Obviously at some point we have to decide which version of a function we need
to invoke, so the simplest possible implementation is a hand-written
if-statement.

.. code-include:: code/post56/hand_written.py
    :lexer: python

.. code::

    $ python hand_written.py
    {'first': 'John', 'last': 'Doe', 'age': 42}
    {'first': 'John', 'last': 'Doe', 'age': 42}

But this implementation is very fragile! For every possible combination of
arguments we have to make sure to modify the if-statement.

If the actual variants of the function are scattered throughout the source code
it is very easy to miss one. Or what happens if another developer mindlessly
renames one of the functions?

.. code-include:: code/post56/hand_written_error.py
    :lexer: python

.. code::

    $ python hand_written_error.py
    Traceback (most recent call last):
      File "hand_written_error.py", line 19, in <module>
        print(lookup("John Doe"))
      File "hand_written_error.py", line 13, in lookup
        return lookupByString(key)
    NameError: name 'lookupByString' is not defined

Type Annotations and Decorators
-------------------------------

Since Python3.5 it is possible to annotate functions with optional type
declarations. These declarations have no semantic meaning for Python's
interpreter but can be used by third party tools for their analysis. If a
function is annotated with types these are part of the function object:

.. code-include:: code/post56/annotate.py
    :lexer: python

.. code::

    $ python annotate.py
    hihihi
    {'x': <class 'int'>, 'y': <class 'str'>, 'return': <class 'str'>}

Since the annotations are part of the function object they are also available
to function decorators, which do receive the function object for modification.
With this knowledge it is possible to hack function overloads via function
decorators. Once a function has been decorated no manual maintenance is
required anymore. Below follows my proof of concept implementation for simple
function overloading.

.. code-include:: code/post56/overload.py
    :lexer: python

.. code::

    $ python overload.py
    44
    1.0
    Hi!!!
    Hi!!!!!
    {'first': 'John', 'last': 'Doe', 'age': 42}
    {'first': 'John', 'last': 'Doe', 'age': 42}
