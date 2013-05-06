---
title: A Handy LLVM Tool (ViewIRGraph)
tags: haskell, llvm, programming
date: 2012-12-03
---

I realized that I forgot to mention another repository related to my
last post: [llvm-tools](https://github.com/travitch/llvm-tools).  As
the name suggests, this repository contains some useful tools based on
my [llvm-analysis](https://github.com/travitch/llvm-analysis) library.
The most interesting tool for people who aren't me is
[ViewIRGraph](https://github.com/travitch/llvm-tools/blob/master/tools/ViewIRGraph.hs),
which makes it easy to visualize several interesting program graphs
(anything supported by llvm-analysis).  The help output gives a reasonable
breakdown:

```
  ViewIRGraph - View different graphs for LLVM IR modules in a variety of formats

  Usage: ViewIRGraph [-o|--output FILE/DIR] (-t|--type TYPE) [-f|--format FORMAT] FILE
    Generate the specified graph TYPE for FILE

  Available options:
    -h,--help                Show this help text
    -o,--output FILE/DIR     The destination of a file output
    -t,--type TYPE           The graph requested.  One of Cfg, Cdg, Cg, Domtree, Postdomtree
    -f,--format FORMAT       The type of output to produce: Gtk, Xlib, Html, Canon, XDot, Eps, Jpeg, Pdf, Png, Ps, Ps2, Svg.  Default: Gtk
```

The input to the tool is any bitcode file or C/C++ source file (source
files will be compiled with clang or clang++).  You can specify the
type of program graph you want to see with the `-t` option.  I've
found the most useful to be the CFG, CDG, and Postdominator tree.  The
call graph (Cg) is also often useful to get an idea of how a program
is structured.  For all of the graph types except for the call graph,
the tool creates one graph per function.

The format option, `-f`, determines how the graph will be displayed.
The `Gtk` and `Xlib` options create X11 windows to show each generated
graph.  These are suitable for small inputs with just a few functions.
The `Html` option is probably the most useful.  It requires that the
`-o` option be set and creates one `.html` file for each function in
the input.  Each file embeds the graph for the function as an SVG
using the [OpenLayers](http://openlayers.org/) library to provide
panning and zooming support.  The other output formats generate images
in the format indicated by their names.  These are a bit less useful
because most of those formats do not have viewers capable of handling
large graphs in a useful way.

I should note that this tool uses graphviz (through the aptly-named
Haskell [graphviz](http://hackage.haskell.org/package/graphviz)
library).  This is wonderful code re-use, but be aware that huge
graphs can end up being laid out in less-than-useful ways.  The tool
does use reasonable clustering specifications where possible, which
does tend to help.

As a side note, I've been using
[optparse-applicative](http://hackage.haskell.org/package/optparse-applicative)
for command line argument parsing lately, including for `ViewIRGraph`.
I'll write up a post about it sometime, but I'll just say for now that
it is the nicest argument parsing library I have used.  It also
automatically generated the delightful `--help` output above.
