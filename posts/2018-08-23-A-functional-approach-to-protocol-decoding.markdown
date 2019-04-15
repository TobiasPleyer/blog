---
title:  "A functional approach to protocol decoding"
date: 2018-08-23
tags: python, haskell, functional programming
category: Programming
authors: Tobias Pleyer
summary: "A recipe for a light weight, flexible and functional style of protocol decoding"
---

A functional approach to protocol decoding
==========================================

Intro
-----

A few months ago I was confronted with the task to decode the
communication of several devices on a
[CANopen](https://www.can-cia.org/canopen) bus. The dump was produced by
the *candump* program, which is part of the GNU can-utils library, whose
output looks similar to this:

``` {.sourceCode .text}
can0  70F   [1]  7F
can0  721   [1]  7F
can0  0A1   [8]  02 FF 01 00 03 00 00 00
can0  000   [2]  82 00
```

My wish was to make this human readable, i.e. plain text. Without any
deeper thought or reason I chose a (in my opinion) very functional
approach for this task which is of so general nature that I wanted to
devote it a blog post. Since CANopen is a rather complicated protocol
and its details have nothing to do with the presented solution I will
use a much simpler dummy protocol to prove my point. This does not
affect generality however.

My original code was written in Python so I will start presenting my
original solution in Python, followed by an implementation in Haskell
with stronger type safety.

Protocol
--------

Every protocol message consists of exactly 2 bytes. The first byte
identifies the sender and the second byte the status of the sender, thus
this is a read-only protocol where participants just broadcast their
status.

The question of message send arbitration, etc. are not of interest here.
We just assume we have some utility program which in the end will give
us a file with the following contents

``` {.sourceCode .text}
(1,2)
(3,0)
(5,1)
...
```

That means: One tuple per line with sender ID first followed by the
status.

The network has the following known participants:

  -----------------------------------------------------------
  Name        ID   Status Codes
  ----------- ---- ------------------------------------------
  Motor\_A    21   0: Not moving, 1: Moving, 2: Hardware
                   defect

  Motor\_B    22   0: Not moving, 1: Moving, 2: Hardware
                   defect

  Motor\_C    23   0: Not moving, 1: Moving, 2: Hardware
                   defect

  Sensor\_A   41   0: Ok, 1: Above threshold, 2: Below
                   threshold

  Sensor\_B   42   0: Ok, 1: Above threshold, 2: Below
                   threshold

  GPIO\_A     51   0: Off, 1: On

  GPIO\_B     52   0: Off, 1: On

  Device\_A   2    0: Idle, 1: Processing, 2: Sending, 3:
                   Error

  Device\_B   4    0: Idle, 1: Calculating
  -----------------------------------------------------------

The details are really not important, the key point is: **for a
different device ID the same status code means a different thing**, so
we have to distinguish the devices in order to understand the meaning of
the message. Note however that we have some very crude grouping in the
sense that all motors start at ID 20, all sensors at ID 40, and so on.

Implementation
--------------

The concept is very simple: We are using a list of functions, so called
interpreters, to handle the input. We only provide an interpreter for
the things we are interested in. An interpreter is nothing more than a
function which either returns *None* or a string. An interpreter
receives a protocol message as its input and decides if it knows how to
interpret (handle) this message. If it doesn't it returns *None*. If it
does it extracts the information and returns a nicely formatted string
of the message's content.

Now comes the core idea of the implementation. The main decoding
function does nothing more than looping through all available
interpreters. The return value of the first interpreter not returning
*None* will be used as the decoding result. If no interpreter matches
some default behavior is invoked.

Here is a possible implementation:

::: {.code-include lexer="python"}
code/post52/fp\_decode.py
:::

**Note:** In order to execute this code Python3.6 or higher is required
because I use [literal string
interpolation](https://www.python.org/dev/peps/pep-0498/).

Given the following input

::: {.code-include lexer="text"}
code/post52/input.txt
:::

this yields the following output

``` {.sourceCode .text}
$ python3.6 fp_decode.py input.txt
Motor #1: Not moving
Motor #2: Moving
Device_A: Idle
Unknown ID 37 with status 5
Motor #3: Not moving
Device_A: Processing
Device_B: Idle
Sensor #1: Ok
Unknown ID 6 with status 2
Motor #1: Hardware defect
Sensor #2: Above threshold
GPIO #1: Off
GPIO #2: On
```

As the above code shows we are using interpreter creator functions to
create multiple interpreters for every group (motors, sensors and
gpios). This allows for maximum code reuse. The interpreter creators
make use of closures to provide the status map and device name to the
generated interpreter function.

Discussion
----------

With regards to the primitive dummy protocol the above solution may
appear over engineered, but its real power starts to shine in more
complicated scenarios. It is worth noting that the implementation is
extremely flexible. Everything can be customized down to the case of
unhandled messages.

Let's assume that one day the status specification and ID of *Device\_A*
change. Instead of digging deep down some big switch case or if/else
tree we know that the *device\_a\_interpreter* function is the only
place we have to look at.

In our example the *ID* field is just a simple digit, but in a realistic
scenario it will be way more complex, e.g. an HTTP header. If this is
the case every interpreter can have custom code to decide if it is able
to handle the attached payload. Same applies for the *status* field.

Likewise if one day we decide we are no longer interested in sensor
data, then we just remove the sensor interpreter from the interpreter
list. That is a one line code change. Simple as that. The main
application logic remains untouched.

What are the disadvantages of this implementation? First of all it is
not type safe, as usual for Python. We are only using a convention,
nothing enforced by a type system. If an interpreter breaks this
convention and for example returns an integer instead of *None* or
string this would break our code. In the above implementation we are
also not guarding against unknown status codes. If a status code is
unknown this is treated as "not interpreted".

``` {.sourceCode .text}
$ python3.6 fp_decode.py bad_input.txt
Motor #1: Not moving
Motor #2: Moving
Device_A: Idle
Unknown ID 37 with status 5
Motor #3: Not moving
Device_A: Processing
Device_B: Idle
Unknown ID 2 with status 4
Sensor #1: Ok
Unknown ID 6 with status 2
Motor #1: Hardware defect
Sensor #2: Above threshold
GPIO #1: Off
GPIO #2: On
Unknown ID 41 with status 6
```

With the following "bad input":

::: {.code-include lexer="text"}
code/post52/bad\_input.txt
:::

Of course this can be fixed, but is not of importance for this post.

We also have to see that the generic loop over all available
interpreters will very likely not be able to meet the performance of a
hand written solution, but that shouldn't be too surprising. We are
trading performance for generality here and in a typical use case this
shouldn't hurt too much.

Haskell
-------

In the intro I promised to provide a Haskell solution as well. We can
more or less copy/paste the Python version, all we need is a bunch of
type definitions.

::: {.code-include lexer="haskell"}
code/post52/fp\_decode.hs
:::

The Haskell version provides the same output as the Python version.
Additionally it also handles the case of status lookup failure:

``` {.sourceCode .text}
$ stack fp_decode.hs input.txt 
Motor #1: Not moving
Motor #2: Moving
Device_A: Idle
Motor #3: Not moving
Device_A: Processing
Device_B: Idle
Sensor #1: Ok
Motor #1: Hardware defect
Sensor #2: Above threshold
GPIO #1: Off
GPIO #2: On
$
$ stack fp_decode.hs bad_input.txt
Motor #1: Not moving
Motor #2: Moving
Device_A: Idle
Motor #3: Not moving
Device_A: Processing
Device_B: Idle
Error! Unknown status 4 for Device_A
Sensor #1: Ok
Motor #1: Hardware defect
Sensor #2: Above threshold
GPIO #1: Off
GPIO #2: On
Error! Unknown status 6 for Sensor #1
```

**Note:** The code in the [decodeWithInterpreters]{.title-ref} function
uses foldr to achieve the same "loop until the first hit" as in the
Python version. If all interpreters have been tried without success then
the "default value" *NotInterpreted* is returned.

Using folds to look through a choice of available handlers is a very
common pattern in Haskell. As a real world example the [Scotty
Webframework](https://hackage.haskell.org/package/scotty) uses such a
pattern [for
routing](https://github.com/scotty-web/scotty/blob/0.11.2/Web/Scotty/Trans.hs#L106).
[But this deserves a post on its
own](./2018-08-29-How-routing-works-in-Scotty.html).
