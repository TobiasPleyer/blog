---
title:  "A closer look at Python's import mechanism - Part 2"
date: 2017-04-13
tags: python, import
category: Programming
authors: Tobias Pleyer
summary: "Part 2 of \"A closer look at Python's import mechanism\""
---

A closer look at Python's import mechanism - Part 2
===================================================

Outline of the Series
---------------------

1.  [What is a module?](./2017-04-02-A-closer-look-at-Pythons-import-mechanism.html)
2.  [The low level import interface](./2017-04-13-A-closer-look-at-Pythons-import-mechanism---Part-2.html)
3.  [Bootstrapping and the importlib module](./2017-04-16-A-closer-look-at-Pythons-import-mechanism---Part-3.html)
4.  How to customize Python's import behaviour

### The low level import interface

This is the second part in a series of articles about Python's import
mechanism. I am planning to give a detailed look on the entire call
stack, from the very low level interface to the very high level
interface and its manipulation facilities. You can start at the
beginning [here](./2017-04-02-A-closer-look-at-Pythons-import-mechanism.html).

**Remark:** All my findings, references and source code depictions are
based on the [CPython GitHub
repository](https://github.com/python/cpython) checked out at tag
*v3.6.1rc1*.

Since 2009 Python exposes its import mechanism via the
[importlib]{.title-ref} module. As the short introductory text of the
module documentation explains, the purpose of this module is to provide
an implementation of import in pure Python and to expose the functions
behind import. This allows for easier customization. The importlib
module and its boostrapping will be the content of the third part of
this series. Customization of the import behaviour of Python will be
comprised in the fourth part.

Even though Python exposes big part of its import machinery as pure
Python code, it cannot expose everything. Python is written in C and it
is unavoidable that the interal bookkeeping and definition of objects
has to be implemented in C. This low-level code (located in *import.c*)
will be part of this series.

#### The callstack

![call stack of Python\'s import statement](../images/pyimport/call_stack.png)

The above image shows the callstack involved with the import statement.
The [dis](https://docs.python.org/3/library/dis.html) module helps to
verify this:

``` {.sourceCode .python}
# Simple script to disassemble the import statement
import dis

def func():
    import simple
    from parent import sub
    return 0

print(dis.dis(func))
```

Which gives the following output

``` {.sourceCode .python}
4           0 LOAD_CONST               1 (0)
            3 LOAD_CONST               0 (None)
            6 IMPORT_NAME              0 (simple)
            9 STORE_FAST               0 (simple)

5          12 LOAD_CONST               1 (0)
           15 LOAD_CONST               2 (('sub',))
           18 IMPORT_NAME              1 (parent)
           21 IMPORT_FROM              2 (sub)
           24 STORE_FAST               1 (sub)
           27 POP_TOP

6          28 LOAD_CONST               1 (0)
           31 RETURN_VALUE
```

> None

This disassembly shows us what happens with the import statement. As the
disassembly shows, the statement

> import simple

becomes the byte code **IMPORT\_NAME** (108). In order to trace the call
further, we have to know how the byte code *IMPORT\_NAME* is translated.
A look in *ceval.c*, where the byte code is evaluated, shows us that
*IMPORT\_NAME* is mapped to the function *import\_name*, which in turn
tries to call the builtin import function, known as *\_\_import\_\_*, in
the builtin namespace. The default implementation of this function can
be found in *bltinmodule.c* and ultimately maps onto
*PyImport\_ImportModuleLevelObject*, which is defined in *import.c*.
This is the roadmap leading to the innards of Python's import machinery.

As stated above the implementation of the import statement is provided
in the module importlib. Is the code in import.c the module definition
for importlib? Well - yes and no. Yes because these are closely related
and interlocked. No because import.c forms a module by its own - \_imp -
and importlib is pure Python code. As the Python standard hints the
underscore means privacy, i.e. it is intended for internal use. Part 3
of the series will shine more light on this. In the following I want to
exemplify on the tasks of import.c.

#### Import hooks

Not very surprisingly Python's import mechanism has undergone some
change with the evolvement of Python. One of these changes was the
introduction of a hook mechanism to facilitate customization.
[PEP302](https://www.python.org/dev/peps/pep-0302/) is a very
recommendable source for this matter. *PEP302* mentions the problems
with the old mechanism and how the Python core developer team tried to
tackle the problem. I won't detail this, because the document is really
well written. One of the key aspects, as explained in the paper, is the
[hook](https://en.wikipedia.org/wiki/Hooking) based nature of import.
The PEP mentions three special objects involved: **meta\_path**,
**path\_hooks** and **path\_importer\_cache**. The initialization of
these is amongst the duties of import.c and done in the function
*\_PyImportHooks\_Init*. The lookup in *path\_importer\_cache* and the
traversal of *path\_hooks* is also done in this module.

#### Thread safety

The import of a module should be thread safe. Loading the same module
from different threads shouldn't lead to broken or partially loaded
modules. The serialization routines, aka locks, are provided by this
module: *\_PyImport\_AcquireLock*, *\_PyImport\_ReleaseLock* and
*\_PyImport\_ReInitLock*

#### Management of sys.modules

Querying, adding removing and manipulating the internal dictionary
sys.modules is also done here. But this is not solely left to import.c.
Since sys.modules is exposed at the Python level, Python code can also
manipulate this dictionary at will.

#### Loading extension modules

The loading of extension modules, which are dynamically compiled object
files (e.g. .dll or .so), is also managed by this module. Since this
involves dynamic loading of symbols from a compiled source, this needs
the lower level C interface of *dl.h*. What this means was explained in
the [first part](./2017-04-02-A-closer-look-at-Pythons-import-mechanism.html)
of the series. The module will lookup a function *PyInit\_XXX* and execute it.
This will fill the module which will then be stored in sys.modules.

#### Importing frozen modules

Frozen modules play a crucial role in the boostrapping process of
importlib. A frozen module is the compiled object of a pure Python
module. It is "frozen" because the compilation process "freezes" the
current state of the module, like a snapshot.
