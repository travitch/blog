---
title: A Change of OS
tags: linux
date: 2014-09-11
---

I have been running Debian unstable on my machines for years reasonably happy.  Living on the bleeding edge is slightly exciting.  Having maximally up-to-date versions of programs is also useful.  The cost of running Debian unstable is implied by the name: sometimes things break.  I have been fine with that over the years - I knew what I was getting in to.  So the occasional update messes up a symlink for glibc.  No big deal.  At least, that is how I used to feel.

I feel like I might be getting old, since I seem to be losing patience with that sort of thing.  Most recently, seemingly during the transition to systemd, a few things suddenly broke.  There has been enough churn in recent years that there are a number of aspects of modern Linux distributions that I just do not understand everything anymore and have no idea how to fix these issues.  A few things that popped up off the top of my head:

 * I could not mount USB drives with my normal user account anymore
 * NetworkManager simply laughed at me and failed silently when I attempted to connect to VPNs
 * gdm would start, but would not show a login screen

The last one was slightly annoying.  I suspect that I was supposed to be doing something special when I logged in to get some magical new session permission system working, perhaps courtesy of systemd.  I have no idea, and searching for answers to these problems is difficult.  I should say, searching for these issues is very easy, but all of the results are for the era around 2006-2008 when these issues were a daily occurrence.  At this point, I decided that I did not really have the patience to figure out what was wrong.  I took the easy way out.

Ubuntu seems to have a lot of momentum these days, and it does have a reputation for just working.  That is, Just Working.  I figured that I would give it a shot.  My system could not get more broken, right?  I grabbed a Kubuntu image and let the installer do its thing.  The first thing I noticed was that the installer was very pleasant to use compared to the curses-based Debian installer.  In the grand scheme of things, that is not important at all.  While you are installing, though, it is a nice touch that you notice (at least if you are coming from a less well-polished distribution).  The installation went off without a hitch and I logged in.  I have to say - everything does Just Work, as promised.  I set up a custom X session and used my normal `.xsession` file to start xmonad.  Deep down in the system, some magic seems to happen with some basic KDE infrastructure tied into the PAM login modules.  I am not complaining - this is exactly the stuff I never want to think about again.

Updates will be less frequent, but that is for the best.  A normal distribution release cycle will protect me from myself.  Part of my dislike of normal release cycles stems from too much time spent with certain enterprise Linux distributions that only provide zombified versions of important programs (e.g., compiler toolchains).  Compiling your own version of GTK+ so that you can run Chrome is several kinds of not very much fun.  Ubuntu has much more reasonable versions of everything I need.  For the most part, libraries seem to be very stable and applications are new enough.  The shorter release cycle, as compared to an enterprise distribution, will also help.  It also seems that a new Ubuntu release is imminent, so we will see shortly how the upgrade process goes.  I have not tried a distribution upgrade since the Mandrake days - I have to assume it is much more pleasant now.  In any case, it probably has to be better than accidentally losing glibc to sid.
