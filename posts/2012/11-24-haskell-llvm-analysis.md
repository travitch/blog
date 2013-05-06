---
title: Program Analysis with LLVM in Haskell
tags: haskell, llvm
date: 2012-11-24
---

# Introduction

I have had the code on github for quite some time, so it seems like I
should say something about my LLVM program analysis tools.  The
primary repository is
[llvm-analysis](https://github.com/travitch/llvm-analysis), which
provides a Haskell interface for analyzing the LLVM IR.  The LLVM IR
is a high-level assembly language for a virtual machine with infinite
registers.  This is a virtual machine as in a piece of hardware that
does not exist rather than a JVM-style virtual machine that programs
run on.  LLVM IR is converted directly to machine code once a target
architecture is chosen.  The LLVM IR is a convenient analysis target
when the details of a programming language's AST are not required.
The IR has a relatively modest ~50 instructions and is fairly similar
to a RISC architecture (only three our four instructions actually
touch memory), so reasoning about it is simple.  Once you compile your
source file to the LLVM IR (persisted in a _bitcode_ file), you can
load it and analyze the instruction stream.

LLVM itself is a C++ library, and writing a program analysis in C++ is
unpleasant to say the least.  I wanted to be able to use a rich
functional language and I chose Haskell.  LLVM has an official C
interface, llvm-c, which provides access to many LLVM facilities to
other languages.  Bryan O'Sullivan created some excellent LLVM
[bindings](https://github.com/bos/llvm) to this C library.
Unfortunately, the C interface is really only suitable for
*generating* code.  Analyzing existing bitcode does not really seem to
be possible through the C interface (and thus through Bryan's
library).

# My solution

If the C interface does not expose enough information to inspect
existing bitcode, what can we do?  I chose to write a layer to
translate the IR from its C++ representation directly to a Haskell
data type.  The resulting data type is pure and can be analyzed with
normal Haskell pattern matching facilities.  This was slightly tricky
because the object graph of the LLVM IR can contain cycles (due to phi
nodes and a few other constructs).  Translating this into a Haskell
data type without something like IORefs to break the cycles required
[tying the knot](http://www.haskell.org/haskellwiki/Tying_the_Knot).
For the interested, the code to perform this translation lives in my
[llvm-data-interop](https://github.com/travitch/llvm-data-interop)
library; it relies on a bit of C++ code to convert the C++ LLVM IR
into a FFI-friendly format (using plain C structs instead of C++
classes).  The Haskell portion of the library ties the knot while
translating the C structures into their Haskell equivalent.  This
library simply exposes the LLVM IR as a simple Haskell data type.

The main library is
[llvm-analysis](https://github.com/travitch/llvm-analysis), which
provides some higher level program analysis infrastructure:

 * An implementation of Andersen's points-to analysis
 * Control flow graphs
 * Call graphs
 * Control dependence graphs
 * Dominator and postdominator trees
 * A dataflow analysis framework
 * A simple framework for analyzing call graphs in bottom-up
   strongly-connected component order
 * A class hierarchy analysis for C++

# Open Issues

I use all of this daily - hopefully someone else might find it useful
one day, too.  I have not put this code on hackage yet because the API
is still unstable and there are a few big issues that I would still
like to address.

## On-demand Metadata Parsing

Currently, all metadata is translated and loaded into memory eagerly.
This is fine for most programs that you might want to analyze, but it
makes analyzing some larger programs impossible due to memory bloat.
Notably, eagerly loading all of the metadata for something the size of
Firefox is completely infeasible - it takes more than 32GB of RAM with
the Haskell representation of the IR (which is much less compact than
the C++ equivalent).

The solution will be to just load metadata on-demand.  This will
require a bit of unsafePerformIO magic.  It will result in redundant
translation work, but most uses of metadata only require a tiny
fraction of it, so it should not be a problem.  Perhaps on-demand
versus eager loading could be turned into a parse-time option.

## Unstable APIs

I am still modifying the API as I use it more and discover things that
are missing or that are just inconvenient.  As my Haskell-fu improves,
I sometimes also discover more efficient or safer ways to handle
things that sometimes require minor API changes.  This sort of change
has been becoming rarer, however.

There is also some instability inherent to following LLVM versions -
the LLVM IR itself changes and a high-level library like this can only
mask so much of that.  That said, this library can mask many of the
very annoying changes that land in LLVM (like the various string types
that seem to come and go from the LLVM codebase).

## Graph Scalability

Another ongoing issue has been my struggle to build a scalable graph
library.  I started off using fgl, but ran into issues with large
graphs.  Some of these issues have been fixed by just making smaller
graphs, but occasionally it cannot be avoided.  Traversing a large fgl
graph can become a bottleneck if the graph is highly-connected (which
can be common if you are taking the transitive closure of a graph).

I have been playing with alternative implementations of purely
functional graphs, but the improvements I have seen are not quite what
I was hoping for (though some have been significant).  Some of my
experiments can be seen in my
[hbgl](https://github.com/travitch/hbgl-experimental) library.
