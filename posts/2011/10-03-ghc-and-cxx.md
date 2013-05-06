---
title: GHC and C++
tags: haskell, programming, c++
date: 2011-10-03
---

Recently I was getting error messages from GHC while building Haskell
programs that complain about unknown symbols referring to C++ standard
template library symbols.  It turns out that these are weak symbols
(since only one definition is required) and telling ghc to pass the
extra (forbidden, deprecated, and evil) `-fno-weak` flag to gcc helps.
I guess I am in trouble when this flag is finally removed.

After changing this, the compiler complained that it could not find
libstdc++.so.  A
[ghc bug](http://hackage.haskell.org/trac/ghc/ticket/5289) describes
this problem.  GHC was apparently not looking at decorated names and
did not find `libstdc++.so.6`.  There is a patch in ghc-HEAD (what
will become ghc 7.4).

Since these problems only seemed to arise while the compiler was
expanding Template Haskell code.  I worked around the problem by
moving the modules that link against C++ into a separate library that
is only linked *after* the TH code is already expanded.  This way the
special GHC compile-time/GHCi linker doesn't need to worry about these
difficult cases.
