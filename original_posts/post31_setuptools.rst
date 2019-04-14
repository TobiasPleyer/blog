Python setup.py entry points
############################

:date: 2017-12-13
:tags: python, setuptools, entry_points
:category: Programming
:authors: Tobias Pleyer
:summary: What exactly are entry points in setup.py script?


Introduction
============

The standard way to distribute a Python package is to ship the sources with a
*setup.py* file at the top level of the package. This file holds all the
information necessary to build and install the shipped package. The content of
this file can grow quite a lot for fairly big and well documented projects, but
all of them follow the very same simple pattern

.. code-block:: python

    from setuptool import setup

    setup (
        ...
    )

The *setup* function has many options, the one I want to talk about is the
*entry_points* keyword. The most typical example of an entry point is the so
called *console_scripts* entry point. Here an example from `pytest's setup.py`_

.. code-block:: python

    setup(
        ...
        entry_points={'console_scripts': [
            'pytest=pytest:main', 'py.test=pytest:main']},
        ...
    )

.. _pytest's setup.py: https://github.com/pytest-dev/pytest/blob/master/setup.py

When we install the pytest package this will cause the following two executables
to be present on the system's path: **pytest**, **py.test**.

What are they?
==============

Concise answer: Entry points are a **global** map of maps of Python importable
objects.

Lending Haskell's type annotation syntax:

.. code-block:: haskell

    entry_points :: Map String (Map String PythonImportable)

The elements of the outer map are called groups. A group bundles common
interest or functionality. It is important to note that **there exist no
enforced standards or conventions how to choose the lookup string**. If a
string is chosen too general name clashes are very likely.

The inner map maps strings (freely choosable) on Python browsable/importable
objects which are written in Python's dotted import syntax

.. code-block:: python

    """
    name: The lookup key for the browsable Python object
    package.subpackage.module could correspond to a file
    package/subpackage/module.py on Python's path
    The colon ':' separates the object from path specification.
    """
    'name=package.subpackage.module:objectname'

    # This is equivalent to the following code
    from package.subpackage.module import objectname as name

Because entry points are global, as mentioned above, every Python module can
gain access to every entry point. How this is done I'll show below.

I don't know the exact details of the implementation of entry points. From what
I got the information is written to files following a standard format and
standard locations.

What are they good for?
=======================

The two biggest use cases are

#. Providing commands in the terminal
#. Supporting plugins

Console Scripts
---------------

I already talked a bit about the first point, console scripts. Let's come back
to the example above.

.. code-block:: python

    entry_points = {'console_scripts': ['pytest=pytest:main']}

Console scripts is a builtin feature of setuptools and will create a script
on the system's path with the name of the left hand side of the equal sign and
make it a wrapper around the specified Python object (which should be
callable).

Here's how it looks on my computer

.. code-block:: python

    #!/usr/bin/python3.5
    # This file is located under /usr/local/bin/pytest

    # -*- coding: utf-8 -*-
    import re
    import sys

    from pytest import main

    if __name__ == '__main__':
        sys.argv[0] = re.sub(r'(-script\.pyw?|\.exe)?$', '', sys.argv[0])
        sys.exit(main())

Plugins
-------

Again I choose `pytest`_ as an example. Pytest is a pretty large testing
framework which allows plugins to extend its functionality. This automatically
brings up the question how this can be done. From a maintainers point of view
it is a pain to take responsibility of maintaining that much third party code
and keep it in the main repository. In addition if a user has a very project
specific need, that user needs to have a chance to integrate this functionality
quickly in his test suite, without hacking the source code or waiting forever
for the plugin to maybe go mainline.

.. _pytest: https://docs.pytest.org/en/latest/

Instead pytest offers a very general registration system. Plugins must inherit
from certain classes provided by pytest and then register themselves under a
name in the entry point group **pytest11** (see the `docs`_). When pytest is
run, it will parse and import every entry point registered for its group and
feed it with data via a hook mechanism based on certain events happening during
program execution.

.. _docs: https://docs.pytest.org/en/latest/writing_plugins.html#making-your-plugin-installable-by-others

.. code-block:: python

    # The example taken from pytest's documentation
    # sample ./setup.py file
    from setuptools import setup

    setup(
        name="myproject",
        packages = ['myproject']

        # the following makes a plugin available to pytest
        entry_points = {
            'pytest11': [
                'name_of_plugin = myproject.pluginmodule',
            ]
        },

        # custom PyPI classifier for pytest plugins
        classifiers=[
            "Framework :: Pytest",
        ],
    )

Example
=======

The last example concerns those that write a library and want to provide their
own entry point group. How do you access the registered entry points? The
following code is an example I got from a `PyCon talk of Phoenix Zerin`_,
starting from minute 24.

.. _PyCon talk of Phoenix Zerin: https://www.youtube.com/watch?v=0W0k6zP_Lto&t=24m

.. code-block:: python

    from pkg_resources import iter_entry_points
    skynet_commands = list(iter_entry_points('skynet.commands'))
    # load brings the object into the local namespace
    first = skynet_commands[0].load()
    # now we can call whatever object was registered
    first()

