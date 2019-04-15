---
title:  "A closer look at Python's import mechanism"
date: 2017-04-02
tags: python, import
category: Programming
authors: Tobias Pleyer
summary: "Part 1 of \"A closer look at Python's import mechanism\""
---

A closer look at Python's import mechanism
==========================================

This is the first in a series of articles about Python's import
mechanism. I am planning to give a detailed look on the entire call
stack, from the very low level interface to the very high level
interface and its manipulation facilities.

Outline of the Series
---------------------

1.  [What is a module?](./2017-04-02-A-closer-look-at-Pythons-import-mechanism.html)
2.  [The low level import interface](./2017-04-13-A-closer-look-at-Pythons-import-mechanism---Part-2.html)
3.  Bootstrapping and the importlib module
4.  How to customize Python's import behaviour

### What is a module to Python?

Before we can speak of the import process itself, we first have to know
what the end result is: a Python module. The module will be the
accessible interface on the Python level to what has been imported.

**Remark:** All my findings, references and source code depictions are
based on the [CPython GitHub
repository](https://github.com/python/cpython) checked out at tag
*v3.6.1rc1*.

The direct answer to this question is: Like everything in Python, a
module is an object. An instance of *PyModuleObject* (defined in
*Objects/moduleobject.c*) to be more precise. Said a little bit over
simplified, a module is nothing more than a little bit book keeping
around a dictionary (also known as hash map).

One of the nice things about Python is that it is not trying to hide its
internals. Surely some of the low level *C* interface gets lost on the
way, but where possible things are made available almost unchanged. The
internal *md\_dict* member of the *PyModuleObject* object, which is the
dictionary underneath a module holding its content, is exposed as the
*\_\_dict\_\_* attribute of the module:

``` {.sourceCode .cc}
static PyMemberDef module_members[] = {
    {"__dict__", T_OBJECT, offsetof(PyModuleObject, md_dict), READONLY},
    {0}
};
```

And can be accessed as soon as the module has been imported:

``` {.sourceCode .python}
>>> import itertools
>>> itertools.__dict__
{'__loader__': <class '_frozen_importlib.BuiltinImporter'>, 'combinations_with_replacement': <class 'itertools.combinations_with_replacement'>, '__spec__': ModuleSpec(name='itertools', loader=<class '_frozen_importlib.BuiltinImporter'>, origin='built-in'), 'groupby': <class 'itertools.groupby'>, 'islice': <class 'itertools.islice'>, 'combinations': <class 'itertools.combinations'>, '__name__': 'itertools', 'cycle': <class 'itertools.cycle'>, 'filterfalse': <class 'itertools.filterfalse'>, '_grouper': <class 'itertools._grouper'>, '__package__': '', 'tee': <built-in function tee>, '__doc__': 'Functional tools for creating and using iterators.\n\nInfinite iterators:\ncount(start=0, step=1) --> start, start+step, start+2*step, ...\ncycle(p) --> p0, p1, ... plast, p0, p1, ...\nrepeat(elem [,n]) --> elem, elem, elem, ... endlessly or up to n times\n\nIterators terminating on the shortest input sequence:\naccumulate(p[, func]) --> p0, p0+p1, p0+p1+p2\nchain(p, q, ...) --> p0, p1, ... plast, q0, q1, ... \nchain.from_iterable([p, q, ...]) --> p0, p1, ... plast, q0, q1, ... \ncompress(data, selectors) --> (d[0] if s[0]), (d[1] if s[1]), ...\ndropwhile(pred, seq) --> seq[n], seq[n+1], starting when pred fails\ngroupby(iterable[, keyfunc]) --> sub-iterators grouped by value of keyfunc(v)\nfilterfalse(pred, seq) --> elements of seq where pred(elem) is False\nislice(seq, [start,] stop [, step]) --> elements from\n       seq[start:stop:step]\nstarmap(fun, seq) --> fun(*seq[0]), fun(*seq[1]), ...\ntee(it, n=2) --> (it1, it2 , ... itn) splits one iterator into n\ntakewhile(pred, seq) --> seq[0], seq[1], until pred fails\nzip_longest(p, q, ...) --> (p[0], q[0]), (p[1], q[1]), ... \n\nCombinatoric generators:\nproduct(p, q, ... [repeat=1]) --> cartesian product\npermutations(p[, r])\ncombinations(p, r)\ncombinations_with_replacement(p, r)\n', 'takewhile': <class 'itertools.takewhile'>, 'permutations': <class 'itertools.permutations'>, 'product': <class 'itertools.product'>, 'zip_longest': <class 'itertools.zip_longest'>, 'chain': <class 'itertools.chain'>, 'count': <class 'itertools.count'>, 'compress': <class 'itertools.compress'>, 'starmap': <class 'itertools.starmap'>, '_tee_dataobject': <class 'itertools._tee_dataobject'>, 'accumulate': <class 'itertools.accumulate'>, 'repeat': <class 'itertools.repeat'>, 'dropwhile': <class 'itertools.dropwhile'>, '_tee': <class 'itertools._tee'>}
>>>
```

That's it about modules. As far as Python is concerned they are just
objects of a certain type. When dealing with modules on the Python
level, i.e. in .py files, they are nothing but objects holding
attributes and one builtin function, *\_\_dir\_\_*. The contents of the
module - functions, variables and sub-modules, are stored in
*\_\_dict\_\_* in the process of importing the module. The details of
that will be the focus of the next part of the series.

### How to write a module?

Basically a module can have one of two forms:

1.  Any pure Python file (.py extension)
2.  C/C++ extension code (.so extension)

In the first case Python will open the file, parse its content and
thereby "learn" what the module is about and what its content is.
Important to know is that Python will actually execute the code
contained in the module! That is the reason why files that are meant to
be used as scripts and as modules try to split these two
functionalities. The common idiom used for that is this:

``` {.sourceCode .python}
# ---
# Module part goes here
# ---
def func(x):
    print("func(x)")

# ---
# Script part goes here
# ---
if __name__ == '__main__':
    func(1)
    func(2)
    # etc.
```

The construct *\_\_name\_\_ == '\_\_main\_\_'* is a guard. When Python
executes a script, this script is more or less the main driving script (
*\_\_main\_\_* ). Python will set the *\_\_name\_\_* variable
accordingly. If, on the other hand, the file is just imported and the
portion of the code below the if construct will never be reached, thus
never be executed.

The second case is different. The content of the module will be C/C++
code, not native Python syntax. This means Python cannot tell how to use
that code as is. What Python needs is a "recipe" that help it integrate
the code in the Python eco system. Most importantly it needs to know how
to initialize it. This is done via an import hook. Every module written
in C/C++ needs to export one function, **PyInit\_modulename**, where
*modulename* is the name used in the import:

``` {.sourceCode .python}
import mymodule  # will end up in a call to PyInit_mymodule    
```

This function is the only means of communication between the import
mechanism and the module to be imported. Python expects this function to
return a pointer to a *PyObject*, which will be casted to the module in
the process. In order for this function to return a valid module it
needs to make use of a few helping constructs. The best is to have a
look at an example. Directly taken from the collections module
(*Modules/\_collectionsmodule.c*):

``` {.sourceCode .c}
/* module level code ********************************************************/

PyDoc_STRVAR(module_doc,
"High performance data structures.\n\
- deque:        ordered collection accessible from endpoints only\n\
- defaultdict:  dict subclass with a default value factory\n\
");

static struct PyMethodDef module_functions[] = {
    {"_count_elements", _count_elements,    METH_VARARGS,   _count_elements_doc},
    {NULL,       NULL}          /* sentinel */
};

static struct PyModuleDef _collectionsmodule = {
    PyModuleDef_HEAD_INIT,  /* m_base */
    "_collections",         /* m_name */
    module_doc,             /* m_doc */
    -1,                     /* m_size */
    module_functions,       /* m_methods */
    NULL,                   /* m_slots */
    NULL,                   /* m_traverse */
    NULL,                   /* m_clear */
    NULL                    /* m_free */
};

PyMODINIT_FUNC
PyInit__collections(void)
{
    PyObject *m;

    m = PyModule_Create(&_collectionsmodule);
    if (m == NULL)
        return NULL;

    if (PyType_Ready(&deque_type) < 0)
        return NULL;
    Py_INCREF(&deque_type);
    PyModule_AddObject(m, "deque", (PyObject *)&deque_type);

    defdict_type.tp_base = &PyDict_Type;
    if (PyType_Ready(&defdict_type) < 0)
        return NULL;
    Py_INCREF(&defdict_type);
    PyModule_AddObject(m, "defaultdict", (PyObject *)&defdict_type);

    Py_INCREF(&PyODict_Type);
    PyModule_AddObject(m, "OrderedDict", (PyObject *)&PyODict_Type);

    if (PyType_Ready(&dequeiter_type) < 0)
        return NULL;
    Py_INCREF(&dequeiter_type);
    PyModule_AddObject(m, "_deque_iterator", (PyObject *)&dequeiter_type);

    if (PyType_Ready(&dequereviter_type) < 0)
        return NULL;
    Py_INCREF(&dequereviter_type);
    PyModule_AddObject(m, "_deque_reverse_iterator", (PyObject *)&dequereviter_type);

    return m;
}
```

As the comment string in the code example already says, this is the
module level code. As can be seen above, every module needs to have a
definition of its methods (*PyMethodDef*) and itself (*PyModuleDef*).
The *PyMethodDef* structure instructs which functions should be
available in the module. The *PyModuleDef* structure holds important
information about the module, like its name, the documentation string
its functions and so on. I added comments to the *PyModuleDef* structure
above to show the names of the structure fields. A module only needs to
define those that are needed. The attributes of the module, e.g. the
class *OrderedDict*, are added in the *PyInit\_* function via the helper
function *PyModule\_AddObject*, which results in a new entry in the
internal hash table (dictionary) of the module (see above).

We now can conclude this first part of the series and concentrate on the
import procedure itself in the following parts.
