---
title: Taffybar 0.4.3
tags: haskell, taffybar
date: 2014-09-12
---

Recently, I cut a long overdue release of Taffybar, my desktop bar for xmonad (or any window manager, really).  The changes are in the [changelog](https://github.com/travitch/taffybar/blob/v0.4.3/CHANGELOG.md).  A few highlights are:

 * Better support for resizing screens
 * A few new widgets
 * A completely new pager built on EWMH
 * Vastly improved power usage
 * Fixed an annoying bug where time zone changes are not picked up

More recent fixes deal with breakage from the network/network-uri split.  Now on a Hackage near you.

I have no specific plans for interesting new development at this point, but I am happy to take suggestions (or, better yet, patches).  I have been mulling over some ideas for a better way to present freedesktop notifications for a while, but I do not have a concrete plan yet.  The biggest outstanding bug I know of relates to handling of xrandr events.  Right now, removing a monitor with a taffybar on it makes you lose your bar, which is fairly annoying.  I need to figure out what my options are on that front.  I am sure that there are some xrandr events that one can listen for.
