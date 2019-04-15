---
title:  "More quirks with Jenkins"
date: 2017-07-14
tags: jenkins
category: Programming
authors: Tobias Pleyer
summary: "I tripped over some Jenkins quirks again lately"
---

More quirks with Jenkins
========================

Jenkins is still work in progress
---------------------------------

Work related I spend quite a bit of time with Jenkins. Recently I again
stumbled upon some issues with Jenkins that irritated me for a moment.

### Pipelines with Java Web Start

I must admit I haven't investigated a lot, so maybe I'm wrong about
this, but here is what happened. Jenkins offers you a couple of methods
to start a slave. One of them is using Java Web Start for it. You click
a button in your browser and magically the rest is done behind the
scenes (you download a .jnlp file which gets executed).

I did this and my computer was online as a slave. But when I executed my
pipeline job Jenkins would fail on me with the usual incomprehensible
error message. Even more confusing: When I ran the exact same job with a
slave registered as a service on my machine the job succeeded.

I don't know what's behind this. I can only guess it has something to do
with user accounts and permissions, but I find it very irritating that a
supposedly easy (and recommended!) method like this seems to cause
problems with the (not so anymore) new pipeline DSL.

### Jenkins and Git submodules

Git submodules prove to be a very annoying problem for me in general.
Not only Jenkins shows problems with them. I found them to cause a lot
of problems at other places, e.g. GitLab too.

The problem I'm facing is the following: if the repository of my
interest includes submodules, how do I treat them? More specifically:
How do I handle the credentials? Jenkins has a [Git
plugin](https://plugins.jenkins.io/git) which is included by default I
think. This plugin let's you choose credentials which will be used to
authenticate with the server. This works without problems, both for
freestyle jobs and for pipeline jobs. But I can't do that for
submodules! Even if the submodules had the same credentials, it still
doesn't work!

If you write pipeline jobs you are anyway on your own. The pipeline DSL
[git step](https://jenkins.io/doc/pipeline/steps/git/#git-git) only
supports the bare minimum usage. When you run freestyle jobs you have
more luck. Under *Advanced Checkout Behaviour* an option about
submodules can be found. There you can choose to check these out
recursively. "Nice!", I thought, "then this job I'll write in freestyle"
(I usually write pipelines). Job configured, build step written, build
project. *And...*

``` {.sourceCode .groovy}
FATAL: not implemented yet    
...
```

Yep, apparently this is not yet implemented. I found a
[ticket](https://issues.jenkins-ci.org/browse/JENKINS-26026) for this
from 3 years ago, still open. The error apparently lies in
[JGit](https://git-scm.com/book/be/v2/Embedding-Git-in-your-Applications-JGit).

Conclusion
----------

Especially since I am more involved in the topics continuous integration
and continuous deployment, I can see the need for tools like Jenkins. I
also don't want to grunt at the Jenkins developers. Jenkins is an open
source project and many hack on it in their free time. Free time is
precious, so this remain unsolved. It is just unfortunate that
submodules in a repository are not such an unusual case and the fact
that this is so unconvenient to work with is unsatisfactory.

Why not write the necessary commands in the job's build recipe? Because
their is no way to handle credentials via command line.

``` {.sourceCode .bash}
# You can do the following
git clone https://username:password@git.mypage/repo/name.git
# But you cannot do something like this
git submodule update --init --user username --pw password
```

Luckily their exists a workaround for most cases: **caching** or **SSH
key pairs**. I prefer caching via

``` {.sourceCode .bash}
git config --global credential.helper store
# You have to run these commands once and enter your credentials
git clone https://git.mypage/repo/name.git
git submodule update --init
```

Now the credentials for every repository are stored. **Note**: The
passwords can only be retrieved from the computer this command was run
from and only as the user that run these commands. **Make sure you run
above commands as the user that Jenkins runs under**.

This solution feels kinda instable and wrong, but it works smoothly if
credentials don't change very often for the repository and credentials
are not regularily cleared.