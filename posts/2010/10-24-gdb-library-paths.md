---
title: GDB and Non-standard Library Paths
tags: linux
date: 2010-10-24
---

This came up for the second time in five months yesterday, so I
thought I would record it so that I can remember what the deal is next
time it happens.  If you have a binary that loads just fine and seems
to be normal according to tools like ldd, but GDB refuses to load it
stating that some shared library is missing, this post may be of
interest to you.

In certain environments (particularly those based on Red-Hat
Enterprise Linux), system libraries can be pretty out-of-date.  If you
have more recent versions somewhere non-standard on your filesystem,
you are probably used to passing library search path flags to the
linker (our friend -L).  This works fine at runtime, assuming you keep
your LD\_LIBRARY\_PATH environment variable set properly.
Unfortunately, GDB does not respect LD\_LIBRARY\_PATH while loading
binaries to debug (possibly only if there is a system library with the
same name).

The easy workaround is to re-link the binary with an rpath to your
non-standard libraries (-Wl,-rpath,/a/path).
