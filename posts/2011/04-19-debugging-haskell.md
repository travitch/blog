---
title: Debugging Haskell
tags: haskell, programming
date: 2011-04-19
---

This is mostly just a note to myself.  Every time I get to the point
in a project where I need to debug some Haskell code, I forget some of
the more useful methods.  I always seem to remember the
printf-debugging method:

~~~~~~~~~~{.haskell}

import Debug.Trace

debug = flip trace

f x = y `debug` (show y)

~~~~~~~~~~

This one just prints out the String returned by `show y` when `y` is
evaluated.  This is useful for some bugs, but rarely for the kind that
I seem to inflict upon myself.

The profiling infrastructure provides another axis for attacking problems:

 * [RTS Debugging Options](http://www.haskell.org/ghc/docs/7.0.3/html/users_guide/runtime-control.html#rts-options-debugging "RTS Debugging Options")
 * [Enabling RTS Options (GHC7)](http://www.haskell.org/ghc/docs/7.0.3/html/users_guide/flag-reference.html#id490773 "Enabling RTS Options")
 * [Enabling Profiling](http://www.haskell.org/ghc/docs/7.0.3/html/users_guide/profiling.html "Enabling Profiling")
 * [Cabal Options](http://www.haskell.org/cabal/release/cabal-latest/doc/users-guide/authors.html#buildinfo "Cabal Options")

These require compiling the program (and all of its dependencies) with
profiling enabled.  It is also useful to specify `ghc-prof-options:
-auto-all` in your .cabal file to automatically annotate most values
with names.  These can help identify space leaks.  Debugging stack
overflows and `<<loop>>` errors is still difficult, since the `-xc`
RTS flag doesn't give much of a stack trace.
