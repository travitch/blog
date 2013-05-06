---
title: Haskell in Emacs
tags: haskell, programming, emacs
date: 2011-07-15
---

Like many people, I use Emacs to edit my Haskell code.  The standard
[haskell-mode](http://projects.haskell.org/haskellmode-emacs/ "haskell
mode") works fine, but I always felt like I could use a bit more help
from my editor.  I recently ran across
[scion](https://github.com/hvr/scion "scion"), which is something like
a Haskell IDE library that provides deep information about programs
to editors.  It also happens to include Emacs integration.

A few notable features:

 * In-buffer error and warning highlighting
 * Expression typechecking
 * Completion of `LANGUAGE` pragmas
 * Go-to-definition of symbols

It does all of this by using the Cabal library to determine your
project settings and then invoking the GHC API to compile everything.
The information is very accurate and compilations after the initial
setup are too fast to notice.  It re-checks your code every time you
save, so feedback is also basically immediate.

I really like scion so far.  I had one project that was fairly complex
with a fancy custom build system involving a configure script -- this
proved to be too complicated for scion to handle correctly.  It ended
up having some linking issues with some FFI calls I was using, so I
split those out into a separate package so I could use scion with my
main project.
