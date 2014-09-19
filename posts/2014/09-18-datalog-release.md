---
title: Releasing datalog-0.2.0.2
tags: haskell
date: 2014-09-18
---

I just released version 0.2.0.2 of my datalog library to
[hackage](http://hackage.haskell.org/package/datalog-0.2.0.2).
Datalog is a declarative query language similar in spirit to Prolog,
though less expressive.  If you have a problem that can be expressed
as
[dynamic transitive closure](http://en.wikipedia.org/wiki/Transitive_closure),
Datalog is one way to express your problem.  I believe Datalog is
equivalent to SQL with recursive queries.  My implementation is
currently fairly basic; I hope to add some more sophistication in the
near future.  The primary feature of my implementation is that it is
usable as a library: you can add facts to a database and issue queries
through just a few simple library calls.  The facts in the database
can be any Haskell type that is hashable and comparable for equality
-- basically anything you could reasonably want to query.

This latest release has one new feature and an important bug fix.  The
feature is a new REPL implementation using
[haskeline](https://hackage.haskell.org/package/haskeline), a very
nice implementation of readline.  The REPL is not ideal as a
high-performance interface to the solver, but it is useful for
exploratory query design.

The bug fixed in this release caused the solver to return some
incorrect answers.  Specifically, it would return the empty set when
there were really some answers.  The root cause of the problem was a
bug in how queries with negation were handled.  In Datalog programs
with no negations in any rules, a very simple evaluation strategy
works well.  When a Datalog program includes negated clauses,
evaluation order begins to matter a great deal.  The simplest way to
deal with negation is to
[stratify](http://www.cs.uni-paderborn.de/fileadmin/Informatik/AG-Boettcher/Lehre/WS_07_08/dbis1/stanford/dbis1k4-ddb-JU-datalog_stratified-negation.pdf)
the rules in the Datalog program.  This stratifies, or partitions, the
Datalog program into strongly-connected components (based on
dependencies between relations) and evaluates the strata (partitions)
bottom-up.  Embarrassingly enough, I was evaluating the strata
top-down before this bug fix release.

