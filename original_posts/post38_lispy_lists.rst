Lispy lists in Python
#####################

:date: 2018-02-05
:tags: python
:category: Programming
:authors: Tobias Pleyer
:summary: How functional list processing can lead to easier code


Ok I admit that the title is a bit catchy. Want I want to talk about is list
processing in Python. One language that was (and still is) famous for its
abilities to handle lists is LISP, the LISt Processor language. Lisp is also
famous for its extensive use of paranthesis. That's because source code in Lisp
is in fact data, namely lists, and thus the paranthesis.

The first thing is that the appearance of Lisp code can be faked in Python. At
this point I should admit that I do not know a lot about Lisp and I have never
written a single program in it. But that's not the point. I won't write real
Lisp, it's still Python, it will just look very un-pythonic.

But Lisp, as a functional language, inspired the other and more important part
of this post: tackling data processing with a functional approach. Depending
on which language you refer to, the term *functional* can mean different
things. For this post I will restrict myself to

.. code:: python

    functional == no hidden/implicit state & immutable data

The task
--------

Let's assume we have `the following input <{filename}/code/post38_example.csv>`_.

This file represents the measurement of two different data lines with the given
timestamps of the measurement. The valid field is mostly 1, but if the field
has value 0 it means the data has to be ignored, because it is invalid.

Both data columns represent streams of byte packages. The only thing we know to
extract the packages is the following:

    * Between packages exists an inhibit (minimum waiting time) of `1e-6`
      seconds, i.e. when the time difference is bigger a new package has
      started
    * The packages of data1 and data2 arrive alternating, starting with data1.
      The data of the other channel can then be considered garbage
    * Don't forget to ignore those rows with `valid == 0`

How do we tackle this problem?

Even though the underlying data is quite primitive, the circumstances demand a
lot of conditions to be taken care of.

Solution
--------

**Note:** The solution presented here is not the most efficient possible,
neither time nor memory wise. But the individual steps are easy to understand
and it is easy to approach the solution step by step. Those kind of
*"intermediate"* task are every day business for most programmers and usually
the data sets are small measured with the standard of a modern computer and the
time is negligible. Thus our focus is on getting the program right, not to
optimise the hell out of it.

That said we will make heavy use of lists and generators, generating new lists
out of existing ones in the process.

.. code-include:: code/post38_version1.py
    :lexer: python

Each step reduces the list of available indices. The most critical point is the
`enumerate` in the calculation in *time_diffs*. This enables us to keep track
of the indices of the *valid_rows* list. Notice how the valid_rows list remains
unchanged (immutable). After applying all logic, we can use the saved indices
to slice out the correct packages out of that list.

List comprehensions
-------------------

Above solution represents a nice stepwise approach, but looks very weird. In
fact I only used this kind of syntax for amusement. Using the list manipulation
functions **map** and **filter** directly is considered not *pythonic*.

Instead Python offers list comprehension which come with a bunch of advantages

    * They are implemented very efficient and usually faster
    * They do not need the explicit list contructor
    * They do not need the explicit lambda
    * They can do filering with an easy to read **if notation**

Below is the same program as above, but with list comprehensions instead

.. code-include:: code/post38_version2.py
    :lexer: python
