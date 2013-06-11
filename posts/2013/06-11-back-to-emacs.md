---
title: Back to Emacs
date: 2013-06-11
tags: linux
---

## Notes on vim

It seems that my extended vim experiment has come to an end.  I did
not mind modal editing and I appreciated the rich language of "text
objects" that vim editing is based on.  On the other hand, I really
disliked most of the editing modes available.  Syntax highlighting in
vim is fast and, for the most part, reasonable.  However, none of the
indent scripts impressed me.  It is easy enough to write a basic
indentation script, but any script like that can be easily fooled.  I
find occasional bad indentation almost worse than no indentation at
all.  Fundamentally, vim does not really have support for building
parse trees, so accurate indentation is probably not feasible.

## Enter yi

I briefly tried to use [yi](https://github.com/yi-editor/yi), an
emacs-like editor charmingly written in Haskell.  To define a syntax
mode in yi, you provide a lexer and a parser.  This lets yi construct
an accurate parse tree (as accurate as the parser you provide), which
enables excellent syntax highlighting and indentation.  The idea
behind yi is exciting, but the current implementation is a bit hard to
work with.  Most significantly, the incremental parsing framework is
not well documented.  I wrote a
[syntax mode for HTML](https://github.com/travitch/yi/tree/html-mode),
but I seem to have missed some subtle details required to get
acceptable performance from the incremental parser.  Documents with
more than a few tags quickly peg a CPU and develop enormous space
leaks.  As far as I can see, my parser and AST look quite a bit like
the LaTeX mode included with yi, which performs well on reasonably
large documents.  Oh well.

## Return to emacs

So I am back to emacs.  The upshot of the whole vim experiment was
that I was motivated to rewrite my emacs configuration to clean it up
and improve startup times.  It now starts up about as fast as vim:
less than half a second.  I also fixed a few things in various modes I
used that really annoyed me before, but that I was never motivated to
deal with properly.

### Startup time

My old emacs configuration could best be described as "cobbled
together".  I had snippets taken from the emacs wiki and all sorts of
packages just dumped in `~/.emacs.d`.  I imagine not a few were
`load`ed or `require`d in unfortunate places.  Now that all of my
machines are running emacs 24, I switched to using `package.el` to
manage my emacs packages.  I just have a list of packages I use and a
check on startup that installs any that are not currently installed.
This seems to do all of the correct autoloading and has no observable
impact on startup time.  The only package I `require` now at the start
of my `init.el` is `package.el` to manage my required packages.  The
few other functions I directly reference from elisp code are
autoloaded on demand.

As a side note, I am using [MELPA](http://melpa.milkbox.net/) instead
of [marmalade](http://marmalade-repo.org/), largely because the former
loaded faster and was more responsive when I was looking through the
package lists.

### Keybindings

The biggest annoyance with my emacs configuration before my vim
experiment was that, occasionally, I would fat finger `C-xC-s` (save
buffer) and hit some key combination that I never could figure out.
Unfortunately, that key combination consistently and helpfully locked
up my emacs frame.  I always had trouble reviving it after that and
usually just killed it and restarted emacs.  It was infuriating.  I
finally tracked it down: I was hitting `C-xC-z`, which is
`suspend-frame`.  First, I have never wanted to do that.  Second, the
behavior of `suspend-frame` is somewhat unusual with a tiling window
manager like xmonad.  Normally, `suspend-frame` minimizes the frame.
xmonad does not really have a concept of "minimize", but the frame
stops accepting input (since it expects that it is minimized).  Under
xmonad, the frame is still visible and it just appears to have locked
up.  Needless to say, this was a bit tough to google.  It also would
not have been a problem under a normal window manager, since the effect
of the key binding would have been obvious.  The solution was simple:
disable the offending keybinding:

```commonlisp
(global-unset-key "\C-x\C-z")
```

### auctex changes

[auctex](http://www.gnu.org/software/auctex/) is an excellent LaTeX
editing environment.  There has always been just one feature that
annoyed me: section and subsection headings are displayed in a
proportional (and large) font face by default.  Minor, but annoying.
I used to use `M-x customize` to change all of the fonts manually, but
that was obnoxious.  The correct incantation is:

```commonlisp
(setq font-latex-fontify-script nil)
(setq font-latex-fontify-sectioning 'color)
(add-hook 'LaTeX-mode-hook
          #'(lambda ()
              (custom-set-faces '(font-latex-slide-title-face ((t (:inherit font-lock-type-face)))))
              (font-latex-update-sectioning-faces)))
```

I think I found that somewhere on stackexchange.

### Misc changes

I finally read up on the difference between `setq` and `setq-default`,
so I modified my configuration to use the correct calls where
necessary.  For the record, `setq-default` sets a global default for
buffer-local variables that can be overridden locally in each buffer.
`setq` sets global variables.

