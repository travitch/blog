---
title: Down with Type 3 Fonts
tags: tex
date: 2014-11-25
---

I recently found myself needing to excise all of the type 3 fonts from
a TeX document.  I believe that type 3 fonts are actually just
embedded postscript, which is less than ideal for a nice PDF.
Obviously, I would never intentionally include a type 3 font, so some
other tool in my toolchain must have been adding them.  Eventually, I
figured out that they were sneaking in through some matplotlib graphs.
I found a nice [description](http://www.phyletica.com/?p=308) of the
problem.  The solution I chose was to tell matplotlib to use
TeX-compatible output:

```{.python}
import matplotlib
matplotlib.rcParams['text.usetex'] = True
```

This fixed my output: all of my graphs now have proper type 1 fonts.
This particular solution had one interesting side effect: all of the
labels in the graphs were interpreted by TeX.  This meant that some
characters had to be escaped and that I was able to use some nice TeX
amenities in labels.
