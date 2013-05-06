---
title: TeX crossref Package
tags: tex
date: 2010-05-25
---

A few days ago I was looking for a more robust type-aware
cross-reference helper macro for LaTeX.  This type of macro lets you
specify something like:

~~~~~~~ {.latex}
  \autoref{fig:tuna}
~~~~~~~

Assuming _fig:tuna_ is the fifth figure, this macro will produce
"figure 5".  This is very convenient if the type of a reference might
change.  Previously I had used
[hyperref](http://tug.ctan.org/tex-archive/macros/latex/contrib/hyperref/)
but it was a bit of a pain to customize and it was not obvious how to
get good list output.

I was pointed to
[cleveref](http://www.ctan.org/tex-archive/macros/latex/contrib/cleveref/),
which is easily customizable and seems to handle just about
everything.  It provides two macros: _cref_ and _crefrange_.  The
first allows you to specify a list of labels and it does the right
thing.  If you list three or more consecutive labels to entities of
the same type, it will collapse them into a range when printed;
something like

~~~~~~~ {.latex}
  \cref{fig:1,fig:2,fig:3}
~~~~~~~

becomes "figures 1 to 3" in the document.  You can also mix references
to different types of entities in one _cref_ and the sensible thing
happens.  _crefrange_ takes two arguments and, as the name implies,
produces a reference to a range of entities.
