---
title: Recent Bug Fixes
tags: programming, linux
date: 2011-09-08
---

First, I finally went back and added support for clang in my
[whole-program-llvm wrapper](https://github.com/travitch/whole-program-llvm).
Usage is easy: just set `LLVM_COMPILER=clang` and it should Just Work.
In the process of implementing this clang support I ended up feeling
pretty bad about some of the old code.  I ended up refactoring out
quite a bit and it is definitely cleaner now.  I'm still not sold on
this whole "object-oriented programming" thing, but it worked
tolerably for a small chunk of Python code.  The resulting objects
are, of course, completely arbitrary bundles of functionality.

I also fixed a bug in [taffybar](https://github.com/travitch/taffybar)
that prevented non-ASCII window titles in xmonad from rendering
properly in the XMonadLog widget.  It turns out that xmonad lies about
the types of strings it gets from X.  While the type signature claims
that they are `String`s (lists of `Char`), they are actually lists of
bytes stuffed into `Char`.  That is, the underlying utf-8 string was
not decoded before being converted into a `String`.  A simple call to
`decodeString` from
[the utf8-string library](http://hackage.haskell.org/package/utf8-string-0.3.5
"utf8-string library") fixed the issue.
