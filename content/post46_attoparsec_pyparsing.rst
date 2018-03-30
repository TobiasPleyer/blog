Comparing Haskell's Attoparsec with Python's pyparsing
======================================================

:date: 2018-03-30
:tags: haskell, python, attoparsec, pyparsing, parsing
:category: Programming
:authors: Tobias Pleyer
:summary: In this post I write two parsers for the same data, one in Haskell the
          other in Python, and compare them.


Writing parsers for custom data is a fairly common task in a programmer's
every day life. Consequently it is nice to know alternatives. In this post I
want to compare 2 different parser libraries, written in different languages.

On the one side: Haskell with attoparsec. On the other side: Python with
pyparsing. Both languages have more than one parser library in their package
repositories, but I think these two are nice to compare, because they are
similar in spirit.

Input
-----

The input is a simple log file format of an imaginary process logging data. One
line contains exactly one log entry and all entries follow the same pattern.
I took this example from a nice post on `schoolofhaskell`_.

.. code-include:: code/post46/yesterday.log
    :lexer: text

Haskell
-------

The Haskell implementation is also taken from `schoolofhaskell`_, but I modified
the code slightly:

    * I made the file a `stack script`_
    * Data.Attoparsec.Char8 is deprecated, I replaced it with
      Data.Attoparsec.ByteString.Char8
    * I removed all comments
    * The main method has been altered a bit. The parser result is now printed
      more readable, with one parsed log per line

.. _stack script: https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter

.. code-include:: code/post46/log_parser.hs
    :lexer: haskell

.. code::

    $ ./log_parser.hs
    LogEntry {entryTime = 2013-06-29 11:16:23, entryIP = IP 124 67 34 60, entryProduct = Keyboard}
    LogEntry {entryTime = 2013-06-29 11:32:12, entryIP = IP 212 141 23 67, entryProduct = Mouse}
    LogEntry {entryTime = 2013-06-29 11:33:08, entryIP = IP 212 141 23 67, entryProduct = Monitor}
    LogEntry {entryTime = 2013-06-29 12:12:34, entryIP = IP 125 80 32 31, entryProduct = Speakers}
    LogEntry {entryTime = 2013-06-29 12:51:50, entryIP = IP 101 40 50 62, entryProduct = Keyboard}
    LogEntry {entryTime = 2013-06-29 13:10:45, entryIP = IP 103 29 60 13, entryProduct = Mouse}


Python
------

.. _schoolofhaskell: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec
