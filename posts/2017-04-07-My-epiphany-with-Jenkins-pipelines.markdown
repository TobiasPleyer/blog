---
title:  "My epiphany with Jenkins pipelines"
date: 2017-04-07
tags: jenkins, groovy, pipeline
category: Programming
authors: Tobias Pleyer
summary: "I finally found a satisfying way to handle pipeline scripts"
---

My epiphany with Jenkins pipelines
==================================

Background
----------

At work we use [Jenkins](https://jenkins.io/index.html) for our
automation needs. Since a month now I am involved with the maintenance
of our Jenkins server, as well as the development of new automation
jobs. Nowadays Jenkins already exists in major version 2 (Jenkins 2.0)
and since this release a new build system, the [pipeline
subsystem](https://jenkins.io/solutions/pipeline/) has become available.
Since we started to use Jenkins before that, many of our jobs are still
written in the old (multi)project style. Since these jobs are running
fine I didn't see a reason to alter them, but for all new jobs I decided
to pick up on the new pipeline subsystem. First it took me some time to
understand the concept behind pipeline. Also, since I am not a Java guy,
it took me a while to get used to the pipeline *domain specific language
(DSL)* that is a superset of [groovy](http://www.groovy-lang.org/). But
now I am really into the new way of writing Jenkins jobs as pipelines.
There were just a few things I didn't like so much, which I will mention
below after a short introduction to Jenkins pipeline.

Pipeline
--------

**Disclaimer:** The content of this and the following sections is based
on what I think I have understood about Jenkins pipeline. The things I'm
writing might be wrong. Most of my knowledge was collected via trial and
error, not via an extensive research.

Pipelines (*PL* from now on) are written in files, called *Jenkinsfile*,
which are more or less groovy scripts. In its essence PL is a *DSL*. It
is made up of a couple of plugins which add certain DSL directives in
addition to the usual groovy namespace. Examples of such *DSL* methods
are
[mail](https://jenkins.io/doc/pipeline/steps/workflow-basic-steps/#code-mail-code-mail)
and [git](https://jenkins.io/doc/pipeline/steps/git/#git-git)

``` {.sourceCode .groovy}
node('node') {
    stage('send email') {
        mail subject: 'My first pipeline', body: "Some text with a $global_variable", to: 'example@gmail.com'        
    }
}
```

The thing I really like about PL is the fact that you now have a full
blown scripting language at your disposal: groovy. Before it was kind of
annoying to either write everything as a script in the OS-native
scripting language (batch under Windows - duh!) or to constantly wrap
other programs in shell calls. PL nicely integrates with the rest of
groovy and groovy helps you out if you need something like *if...else*,
*loops* or *custom functions*.

My problems
-----------

What I don't like to much is the current concept of the *Jenkinsfile*.
You can have a maximum of one per branch per repository, and it is
executed every time some action happens via version control system (svn,
git, etc...). This doesn't quite fit into our current usecase. We work a
lot with [gerrit](https://www.gerritcodereview.com/) and Jenkins has to
fulfill quite a few jobs there too. Where is my Jenkinsfile to go now?
In addition also per branch there exists more than one job, which means
more than one Jenkins file needed (or I don't get the concept). I ended
up writing all my pipeline code in the tiny, none resizeable, editor
window on the pipeline's configuration page. That was unsatisfying for a
couple of reasons:

-   The editor window is sooo small, it is annoying to scroll that much!
    **It has syntax highlighting and tab help though!**
-   No real version control possible! Manually saving the code somewhere
    is doomed to fail and the Jenkins job history functionality is not
    even close to the comfort git offers
-   Sharing common stuff is only possible via external code (e.g. Python
    scripts)

My solution
-----------

First, as I mentioned before, a *Jenkinsfile* is almost a groovy script.
In addition the PL DSL offers a nice directive:
[load](https://jenkins.io/doc/pipeline/steps/workflow-cps/#load-evaluate-a-groovy-source-file-into-the-pipeline-script).
As the documentation says:

> load: Evaluate a Groovy source file into the Pipeline script

The keyword here is **into**. If my interpretation is correct it means:
"Make groovy understand Jenkinsfile". So what the load directive does,
among probably a lot of other things, is prepending a bunch of import
statements to the script to make the PL symbols available. The result:
you can write a .groovy file with PL DSL directives mixed inbetween.
Loading such a file is easy and follows this pattern:

``` {.sourceCode .groovy}
// In the Jenkins web UI
def pipeline
node('slave') {
    pipeline = load 'pipeline.groovy'
    pipeline.functionA()
}
pipeline.functionB()
```

``` {.sourceCode .groovy}
// pipeline.groovy
def functionA() {
    //code
}
def functionB() {
    //code
}
return this
```

**Pitfall:** load is a bit weird with its search behaviour. Every path
you give it, even absolute paths, are interpreted as starting within the
current Jenkins workspace. In order to cope with that I had to add an
extra line to the above code sample:

``` {.sourceCode .groovy}
// In the Jenkins web UI
def pipeline
node('slave') {
    bat 'copy my/path/to/git/pipeline.groovy'
    pipeline = load 'pipeline.groovy'
    pipeline.functionA()
}
pipeline.functionB()
```

Where *my/path/to/git* is the git repository where I keep all my scripts
under version control. The *copy* command just brings a local copy of
the script into the current workspace, where *load* can find it.

With this new style of writing PL scripts my "Jenkinsfile" degenerates
to a couple of *stage* definitions calling into the loaded groovy
**script object** (that's why we need the *return this* statement). Now
the big bulk of my jobs can be maintained in a version control system,
be edited in whatever editor I please and modifications are quick and
easily distributed to all Jenkins workers, e.g. via a scheduled *"git
pull"*.

Summary
-------

Maybe this is still not the ideal way. Absolutely possible that I am
raping Jenkins in way that every experienced Jenkins enthusiast is
screaming right now. The important point for me is: my workflow is more
**structured**, **maintainable** and **distributed**.