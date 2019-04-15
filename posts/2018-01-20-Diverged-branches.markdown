---
title:  "Diverged branches"
date: 2018-01-20
tags: git
category: Programming
authors: Tobias Pleyer
summary: "How I usually handle the case when my local branch has diverged with upstream."
---

Diverged branches
=================

The whole idea of git is to allow people to cooperate. People work on
their local copies of a branch on the server(s) and regularly push their
work upstream. A quite common scenario is the case when someone pushes
changes to a branch that pose conflicts with the changes of somebody
else. In such a case it is "first wins" and the second guy has to take
care to bring the all present version into a congruent state. One way is

``` {.sourceCode .bash}
$> git fetch origin branchname            # take the newest vesion from the server
$> git checkout conflicting_local_branch  # switch to the branch with conflicts
$> git merge origin/branchname            # merge the two branches
```

And solving the merge conflicts that will result from these commands.

I personally prefer to pretend that all of my commits happened perfectly
after the commits of the guy that was faster, so I choose to rebase my
branch

``` {.sourceCode .bash}
$> git fetch origin branchname            # take the newest vesion from the server
$> git checkout conflicting_local_branch  # switch to the branch with conflicts
$> git branch tmp                         # make another reference to the current branch (backup)
$> git reset --hard origin/branchname     # make your local copy the same as origin's
$> git checkout tmp                       # swtich back to the stored refernce of the local branch
$> git rebase conflicting_local_branch    # rebase onto the branch and resolve the merge conflicts
$> git push origin HEAD:branchname        # push the new resolved commits to origin
```

The net results should be identical but the history graph will look much
nicer.