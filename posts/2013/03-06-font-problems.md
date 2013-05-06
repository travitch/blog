---
title: Font Problems
tags: linux, fonts
date: 2013-03-06
---

I've been using [Adobe Source Code Pro](https://github.com/adobe/source-code-pro)
as my primary font for a while.  The other day I tried to update from 1.013 to
1.017 for no particular reason; this caused some Problems.  As soon as I
switched, the font looked awful in my terminals (no matter how many times I
rebuilt the font cache).  It still looked fine in GUI programs (e.g., gvim).  I
am not a font expert, but it seems like a new weight was added in this release:
medium.  Somehow this became the default weight, while the regular weight was
much thinner.  The default was more bold than I was used to.

I ended up having to revert.  We will see what happens with the next released
version.  This font is still one of my favorites - it has a distinctive yet
clean look.  My only complaint is that the zero is dotted and not slashed.  I
also like Consolas, but the license of Source Code Pro is much nicer.

Speaking of fonts, I also seem to have trouble getting bold fonts in my
terminal vim.  I will probably have to spend an afternoon some day debugging
everything font related.
