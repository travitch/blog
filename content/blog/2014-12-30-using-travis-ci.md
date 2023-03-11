+++
title = "Using travis-ci"
date = 2014-12-30
[taxonomies]
tags = ["programming"]
+++

For quite some time, I have been meaning to set up [travis-ci](https://travis-ci.org/) for some of my github projects.  To get the ball rolling, I chose my projects with the simplest build systems and dependencies.  As promised, it was very much a painless experience.  I added my trivial `.travis.yml` files specifying Haskell as my language and asking for a few versions of ghc; everything else was automatic.  I had to make a few tweaks here and there to update some old test suites that I had not run in a while.  That alone was worth the time.

One project was (is) more interesting.  I have a [highly experimental graph library](https://github.com/travitch/haggle) that I have been mulling over for a while.  It currently has very limited tests and is more in the design stage.  I set it up to build on travis and, oddly enough, it built fine on ghc 7.6, but failed on 7.4 and 7.8.  This was surprising.  I had developed the library on ghc 7.6, but I did not use any particularly advanced compiler features.  It is a bit embarrassing, but I think it only compiled when I wrote it due to a bug in the ghc 7.6 type checker (possibly related to partially-applied type synonyms).  I will have to shuffle things around a bit to make it work properly.  This easy multiple compiler version testing is probably the killer feature of travis-ci for me – I usually only develop on a single ghc version and did not catch this problem at all.

Annoyingly, I have a test suite that runs fine locally but fails on the travis nodes.  I am sure I will work it out eventually.  I also have to sit down and make some more sophisticated `.travis.yml` files for my projects with non-trivial dependencies.  Of course, I also need to add test suites to a few projects.
