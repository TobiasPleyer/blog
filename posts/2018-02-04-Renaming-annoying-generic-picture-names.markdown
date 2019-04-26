---
title:  "Renaming annoying generic picture names"
date: 2018-02-04
tags: python
category: Programming
authors: Tobias Pleyer
summary: "How to write a quick and dirty script to rename pictures."
---

Renaming annoying generic picture names
=======================================

A really quick and dirty script to rename pictures with a generic
filename to have more meaningful names. One especially annoying thing is
when the pictures have the camera's picture count in them, e.g.
[CAM\_2345.jpg]{.title-ref}.

The script will search for pictures meeting a glob pattern, e.g.
[CAM\_\*]{.title-ref}, and rename all of these files given the new
basename and a meaningful counter, starting at 1 and padded with zeros,
i.e. "001" instead of just *1*.

The script isn't very user friendly or error proof etc., but it does the
job, uses Python's new [pathlib
library](https://docs.python.org/3/library/pathlib.html) and was hacked
in under 10 minutes.

::: {.code-include lexer="python" file="code/pic_rename.py"}
:::
``` {.sourceCode .bash}
# Example usage
$ python3 path/to/picture/folder 'CAM_*' 'my_better_name'
```