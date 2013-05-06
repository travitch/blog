---
title: Freedesktop.org Notifications for Xmobar
tags: haskell, linux
date: 2010-10-25
---

I am an XMonad user.  There, I said it.  I don't use any components of
the typical desktop environments except for one: notification-daemon.
Notifications from various applications are useful, even in a minimal
environment.

The Problem
-----------

Unfortunately, the normal notification daemon is rather unpleasant to
look at.  The popups are intrusive and tend to appear wherever I am
looking.  I've tried to configure them to appear in every corner of
both monitors at various times, and it seems that I just like to look
at corners of things for some reason.  They are also not particularly
visually appealing.  The notifications can be themed, to an extent,
but the engines are a pain to work with when you don't have root
access to install new ones.

Where Can I Put These Things
----------------------------

It occurred to me that I had a large blank space in my xmobar.  xmobar
is a status bar mainly for XMonad users.  It is simple and can display
the current workspace status of XMonad and various system monitors,
all in plain text.  It can read and display the output of arbitrary
external commands.  Using
[statnot](http://www.k2h.se/code/statnot.html), a text-based
notification-daemon replacement, you can construct a file with the
current notification in it.  Combine this with the Command reader from
xmobar and you have a working text-based notification system.

There are a few problems with this approach:

 * statnot is written in Ruby
 * Ruby is not Haskell
 * Spawning external processes all the time to check for new notifications feels dirty
 * I have too much free time

What Now Then
-------------

Instead of leaving well enough alone, I did what anyone else in my
situation would have done.  I wrote a proper xmobar plugin
implementing the freedesktop notification specification.  What I
actually implemented is apparently some
[draft version](http://library.gnome.org/devel/notification-spec/) of
the specification, but it seems to be approximately what
notification-daemon itself was using.  There are surely some
deviations from the specification, but it works well.

This plugin uses the
[dbus-client](http://hackage.haskell.org/package/dbus-client) package
to handle all of the heavy DBus-related lifting.  It spawns one thread
to wait for notifications and another to display and expire them as
necessary.  It does not spawn any external processes.  It has a silly
gimmick: if a notification is longer than some (configurable)
threshold, it will scroll around in the style of Winamp 2.x

The source of [the plugin](https://gist.github.com/3707046) is released here
under whatever license is required to use it with xmobar and
dbus-client.  I think dbus-client is GPL3.  I might put this into a
repository somewhere at some point.

Usage
-----

The plugin can be instantiated in ~/.xmobarrc with an entry like

~~~~~~~{.haskell}
  Run NotificationDaemon 75 100 20000 5000
~~~~~~~

The plugin aliases to "NotificationDaemon" in the format string.  The
parameters are, in order:

  * the maximum number of characters displayed at a time from a given notification.
  * the animation speed (milliseconds between frames)
  * the maximum notification duration (in milliseconds)
  * the default notification duration (in milliseconds)


I had to tweak the cabal file for xmobar a bit to make things work.
Besides adding the dependencies on the DBus-related packages, I had to
remove the EWMH plugin from xmobar.  Unfortunately, the EWMH plugin
uses the mtl library while dbus-core and dbus-client use the
transformers library.

Issues
------

There are improvements to be made, of course.

 * I want to make the message template configurable.  Right now it is hard-coded as "subject: body" or something similar
 * The colorization should be configurable, too

The xmobar configuration system doesn't quite allow arbitrary Haskell
code to appear, unfortunately, so I probably can't just use
user-provided hooks.  The animation is also not particularly smooth,
but I don't think I can do much better using string-only output.  I'd
love to add some support for writing to OpenGL contexts from xmobar
plugins, but that would take more time than I have.
