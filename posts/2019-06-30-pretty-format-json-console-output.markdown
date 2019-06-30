---
title: Pretty format JSON console output
date: 2019-06-30
tags: json, python, couchdb, console, shell
category: Programming
authors: Tobias Pleyer
summary: "A neat little trick to gain effortless pretty printing of JSON output in your shell"
---

# Pretty format JSON console output

Recently I started reading a little bit about
[CouchDB](http://docs.couchdb.org/en/stable/index.html). So after I installed
*CouchDB* on my machine I started reading the
[Getting Started](http://docs.couchdb.org/en/stable/intro/tour.html)
section of the documentation. There they explain how to interact with *CouchDB*
using [curl](https://curl.haxx.se/). For every *curl* command they also
provide the corresponding answer of the *CouchDB* server, which is pretty nice
if you're just reading without typing the commands yourself or reassuring if
you do type the commands while following along.

**But:** In my own shell the JSON responses are not as nicely formatted as in
the docs.

This is of course not surprising, because that is not *curl*'s task to do. So I
asked: "Isn't there a simply way of doing that?". Probably not so surprisingly:
there is!

## The Python JSON tools module

There is a Python package called **json\_tools** on
[PyPI](https://pypi.org/project/json_tools/).

The nice thing: If you run the library module as a script (see the `-m` option
in `python --help`) it will read JSON data input from stdin and pretty print it
to stdout.

So instead of this:

```bash
$ curl -X GET http://127.0.0.1:5984/example                         
{"db_name":"example","update_seq":"0-g1AAAAFTeJzLYWBg4MhgTmEQTM4vTc5ISXIwNDLXMwBCwxygFFMiQ5L8____sxIZ8ChKUgCSSfaE1TmA1MUTVpcAUldPUF0eC5BkaABSQKXziVG7AKJ2PzFqD0DU3idG7QOIWpB7swBegl6O","sizes":{"file":1552,"external":0,"active":0},"purge_seq":0,"other":{"data_size":0},"doc_del_count":0,"doc_count":0,"disk_size":1552,"disk_format_version":6,"data_size":0,"compact_running":false,"cluster":{"q":8,"n":1,"w":1,"r":1},"instance_start_time":"0"}
```

you can do this:

```bash
$ curl -X GET http://127.0.0.1:5984/example | python -m json.tool 
{
    "db_name": "example",
    "update_seq": "0-g1AAAAFTeJzLYWBg4MhgTmEQTM4vTc5ISXIwNDLXMwBCwxygFFMiQ5L8____sxIZ8ChKUgCSSfaE1TmA1MUTVpcAUldPUF0eC5BkaABSQKXziVG7AKJ2PzFqD0DU3idG7QOIWpB7swBegl6O",
    "sizes": {
        "file": 1552,
        "external": 0,
        "active": 0
    },
    "purge_seq": 0,
    "other": {
        "data_size": 0
    },
    "doc_del_count": 0,
    "doc_count": 0,
    "disk_size": 1552,
    "disk_format_version": 6,
    "data_size": 0,
    "compact_running": false,
    "cluster": {
        "q": 8,
        "n": 1,
        "w": 1,
        "r": 1
    },
    "instance_start_time": "0"
}

```

This is a lot nicer to read and all you need for it is to have Python and the
json_tools module installed.
