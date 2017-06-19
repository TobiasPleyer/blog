Jenkinsfiles
############

:date: 2017-06-05
:tags: jenkins
:category: Programming
:authors: Tobias Pleyer
:summary: What I don't like about Jenkins' pipeline documentation

Jenkinsfile confusion
=====================

In general the documentation of Jenkins still leaves much to desire, but one thing that really annoyed me is the documentation for the so called **Jenkinsfile**. In my opinion this documentation is unnecessarily cryptic and confusing.

I personally misinterpreted the following things when I first read about it:
    * Naming convention
    * Usage
    * Source code management

Let me just write about these topics really quick

Naming convention
-----------------

Very short: There is none. The documentation is never getting tired to almost religiously speak of the **Jenkinsfile**. But Jenkinsfile just means "a file that holds valid Groovy/Pipeline code". That's all there is to it. You can name it *test*, *file1*, *lib1.groovy*, etc. Jenkins doesn't care. All that Jenkins cares about is that the content of that file can be parsed to valid pipeline code.

Usage
-----

This goes along with the previous point. Convinced that the name got to be *Jenkinsfile*, I thought there can only be one such file per repository, or at least per branch. That is not true. Firstly, if you hack your code directly into the web UI of Jenkins' job configuration the name doesn't matter. A different job can have a different code snippet. Secondly, if you supply the pipeline script from a source code management system you are also free to choose which file to take. Which brings me to the next point...

Source code management
----------------------

It is stated nowhere that the SCM holding your code, can be completely de-coupled from the SCM holding your Jenkins stuff. One could be under

    *https://github.com/customername/project.git*

while the other runs under

    *https://github.com/continuous_integration/jenkins.git*

I find this point very important because it really opens doors to options like reusability (aka *DRY*) and independent development and how you think about your setup.

I'll probably write a bit more about Jenkinsfiles in a later post.
