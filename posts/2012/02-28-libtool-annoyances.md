---
title: Libtool Annoyances
tags: linux
date: 2012-02-28
---

I have a set of scripts
([available on github](https://github.com/travitch/whole-program-llvm))
that I use to generate whole-program LLVM bitcode files.  They are
used as drop-in replacements for gcc that compile code twice: once to
a normal object file and once to a bitcode file.  Ideally, these
scripts can be substituted in via environment variables in the typical
fashion:

~~~~~~~~~~
CC=wllvm CFLAGS="-O0 -g" ./configure
make
make install
~~~~~~~~~~

Until now, this worked for many libraries, but often ran into trouble
with some libraries that used libtool in their build processes.  The
libtool script would die with a syntax error some time into the build.
After tracing the configure script, it was apparent that a configure
check was failing and then continuing after setting its variable to
something invalid.  The check was determining how to parse the output
of `nm`; when it failed, part of the parsing pipeline was left empty
and became a syntax error.

Annoying.

I eventually figured out that my scripts were improperly trying to
attach an LLVM bitcode file to an executable during the configure
process, causing this critical check to fail.  Preventing the scripts
from attaching bitcode directly to binaries avoids this problem.  No
significant functionality is lost, since multi-file executables are
nearly always linked together separately from the compilation of
individual files.
