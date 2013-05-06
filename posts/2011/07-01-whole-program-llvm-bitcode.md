---
title: Whole Program LLVM Bitcode
tags: programming
date: 2011-07-01
---

Sometimes it is useful to be able to analyze an entire program at
once, rather than analyzing individual compilation units.  LLVM has
some infrastructure in this: the `llvm-link` program (and associated
library methods) combines multiple bitcode files into one as a linker
might.  Unfortunately, getting all of the bitcode to pass to
`llvm-link` in the first place can be challenging in the face of
strange and arcane build systems (e.g., autotools, libtool, and other
impolite tools).

Official Aside
==============

There is actually an official LLVM way to build whole-program LLVM
bitcode using the GCC link time optimization (LTO) framework and a
plugin for the gold linker.  Some of the relevant details are
[documented](http://llvm.org/docs/GoldPlugin.html "Gold Plugin
Documentation"), but that is not the whole story.  Long story short,
after you:

 * build binutils > 2.20.1 with LTO and plugin support (`--enable-lto` and
   `--enable-plugins`),
 * build LLVM with the `--with-binutils-include` flag to tell it where to find
   the plugin-api header,
 * build gcc 4.5.2 with LTO and plugin support (`--enable-lto` and `--enable-plugin`)
   _with_ the patch from dragonegg that did not make it into the 4.5 mainline (also,
   gcc dependencies mpc, mpfr, ppl, and cloog), and
 * build the dragonegg plugin

you can finally use the link time optimization plugin LLVMgold.so to
generate whole-program bitcode files.  One way to do this is to just
use gcc with dragonegg as your compiler for a project with the extra
flags `-flto -fplugin=dragonegg.so -S`.  This tells the dragonegg
plugin to generate LLVM IR for each file; the `-S` is necessary to
prevent gcc from attempting to run the system assembler over LLVM
assembly, which it will not understand.  During the link stage, you
can add the flag `-Wl,-plugin-opt=also-emit-llvm` and the linker will
spit out a bitcode file along with a binary.

Note that the `-S` flag is not entirely necessary.  You can convince
the compiler to use `llvm-as` to assemble the output of the dragonegg
plugin into an actual bitcode file if you pass the `-Bdir` flag, where
`dir` is the path to a directory containing a single symlink that
presents `llvm-as` as just `as`.  I also had to provide a link for my
plugin-enabled `ld`, as the system version was far too old.

This process does work for simple builds, but fails down if your build
process generates intermediate static libraries.  Since LLVM assembly
and bitcode files are not true object files, the resulting static
library archives are not valid linker inputs and the compilation will
error out when it tries to use them.

A Hackish Solution
==================

Instead of doing things the right way, I decided to do them my way.  I
wrote a simple wrapper script that pretends to be gcc and compiles
every file in a project twice.  The first compilation produces an
authentic object file.  The second produces an LLVM bitcode file and
writes the full path to the bitcode file into an ELF section in the
object file.  The objects can be moved around and linked arbitrarily
and these ELF sections are merged appropriately.  After the
compilation finishes, there is a script to read this ELF section and
link together all of the named bitcode files.

I posted the scripts on
[github](https://github.com/travitch/whole-program-llvm "github").  They
have worked for me so far but there are probably bugs.  If you happen
to find some, just let me know and I'll see about fixing them.

TODO
====

Currently, the scripts only support dragonegg.  I need to make it work
with clang, too.
