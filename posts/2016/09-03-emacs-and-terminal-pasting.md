---
title: Emacs and Terminal Pasting
tags: emacs linux
date: 2016-09-03
---

Over the last few weeks, I've found myself having to copy and paste
large chunks of text into emacs running in a terminal.  This leads to
some annoyance, as each character pasted triggers a keystroke.  In
particular, every newline triggers indentation via
`newline-and-indent`.  This is very annoying, as the indentation
usually gets a bit messy and turns into an ugly staircase of text.
Vim has a solution to this problem via a command `:set paste`, which
turns off indentation while pasting.  More generally, most terminals
support what is known as
[bracketed paste](https://cirw.in/blog/bracketed-paste) to enter a
large string of text as one virtual keystroke.  I recently discovered
the emacs equivalent: bracketed-paste mode on
[melpa](http://melpa.org/#/bracketed-paste).  Now a long standing
annoyance is fixed with a simple:

```
(require 'bracketed-paste)
(bracketed-paste-enable)
```
