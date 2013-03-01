---
title: hasksyn: Haskell Syntax for Vim
tags: vim,haskell
date: 2013-03-01
---

During my (ongoing) vim experiment, I found myself missing some of the
amenities of haskell-mode for emacs.  Namely great syntax highlighting and
pretty good indentation.  [vim2hs](https://github.com/dag/vim2hs) looked
reasonably nice, but did not handle indentation.  It also made scrolling very
slow, perhaps because it just does too many fancy things.
[haskellmode-vim](http://projects.haskell.org/haskellmode-vim/) had slightly
worse syntax highlighting and was also a bit slow; it also did not have much of
an indentation story.

In response, I wrote [hasksyn](https://github.com/travitch/hasksyn), which has
both good syntax highlighting and pretty good indentation.  Indentation is
tricky in vim, I can appreciate that now, but I think I did a reasonable job.

# Features

 * Align pipes, commas, and let bindings
 * Indent after trailing operators, `where`, and `case ... of`
 * Snap `in` to match its corresponding `let`
 * De-indent after catchall cases and returns (optional)

Most importantly, comments and strings that happen to contain Haskell keywords
will not confuse the indentation.  I threw in a new cabal highlighting mode,
too, because I got a bit annoyed that "keywords" were being highlighted in my
package descriptions and module names.

