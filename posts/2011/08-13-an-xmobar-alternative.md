---
title: Taffybar: An Xmobar Alternative
tags: linux, haskell
date: 2011-08-13
---

I've been an XMonad user for a while now.  The window manager doesn't
include any status bar-type functionality, instead relying on external
programs.  The two most common bars in use seem to be dzen and xmobar.
I didn't like the methods for feeding data into dzen, so I went with
xmobar.  It was tolerable. I added a freedesktop.org notification
widget a few months ago and it got a little better.  Unfortunately,
the text-only interface bothered me a little bit, and there was no
easy way to get a system tray working well with xmobar.

This itch bothered me enough that I scratched it last week and wrote
an alternative status bar for xmonad:
[taffybar](http://hackage.haskell.org/package/taffybar).  The bar is
actually very simple and based on gtk2hs.  If you find that offensive,
taffybar probably isn't for you.  Visually, it is very similar to
xmobar (I don't really know how much room there is for visual
variation in the system bar space).  A few feature highlights:

 * System tray
 * XMonad log widget that works over DBus (so you can restart xmonad
   and taffybar separately and not worry about pipes filling up)
 * Freedesktop notification widget
 * Battery widget using UPower
 * Time-based graph widgets (similar to those in Awesome)

There is still a lot of work that could be done and a few widgets I
want to add, but I wanted to have an early release so people could try
it and give me feedback.  The documentation is in the System.Taffybar
module haddock; unfortunately, hackage doesn't like my package and the
build failed, so the documentation is temporarily
[elsewhere](http://pages.cs.wisc.edu/~travitch/taffybar) until I figure out
how to make hackage happy.  The installation goes fine on the machines
I've tried it on.

### Update (2012-02-17)

It looks like the
[documentation finally builds on hackage](http://hackage.haskell.org/package/taffybar).
