---
title: Emacs Fonts
tags: linux, emacs
date: 2011-04-16
---

I keep forgetting to update this blog.  I feel bad about that; I
thought that I would have more free time after I finished taking
classes.  I am going to try to force myself to update more frequently.
I won't force myself to make the content insightful, though.

Anyway, I have been writing code in Haskell lately, and the
[haskell-mode](http://www.haskell.org/haskellwiki/Haskell_mode_for_Emacs
"haskell-mode") for emacs is very good.  This mode has support for
replacing common multi-character operators and identifiers with their
Unicode equivalents.  For example, -> becomes →.  The replacement is
done only at the level of the text renderer; the underlying file still
has the original multi-character ASCII sequences.  While this feature
is far from necessary, it makes scanning code more pleasant.  There is
only one problem: my programming font
([Anonymous Pro](http://www.ms-studio.com/FontSales/anonymouspro.html
"Anonymous Pro")) does not have full glyph coverage for the Unicode
symbols that haskell-mode uses.

Emacs notices this, of course, and uses another font to render these
symbols.  Unfortunately, whatever font it picks is 1) ugly and 2) the
wrong size.  I eventually figured out that emacs uses
[fontsets](http://www.gnu.org/software/libtool/manual/emacs/Modifying-Fontsets.html
"fontsets") to determine the search order for glyphs; I modified my
default fontset to prioritize a better looking and more
appropriately-sized font for Unicode symbols.  The hardest part was
figuring out the second argument to this function: TARGET.  I still
haven't found a definitive list of the possible values of this argument,
but I found enough examples to at least let me fix my problem:

~~~~~~~~~~ {.lisp}
  (set-fontset-font "fontset-default" 'ucs "dejavu sans mono-11")
~~~~~~~~~~

I also tried [ghc-mod](http://www.mew.org/~kazu/proj/ghc-mod/en/
"ghc-mod"), which extends haskell-mode with some more interactive
features using ghc (or hlint).  It is very impressive and can offer
syntax checking, useful symbol completion, type signature inference,
and documentation lookup.  Unfortunately it automatically saves the
buffer you are working on whenever emacs is idle so that it can redo
the syntax check.  I found this to be incredibly annoying and had to
disable the mode.  Perhaps future versions will allow the auto-save to
be disabled.  If the saving doesn't bother you, though, it is an
impressive aid to Haskell development.
