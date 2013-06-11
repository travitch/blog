---
title: Linux Media Players
tags: linux
date: 2010-06-09
---

Background
----------

I bounce around between media players fairly often -- maybe two or
three times a year.  Lately I have been using Banshee, which works
reasonably well.  Unfortunately, the Last.fm support has been broken
for a while.  Worse, judging by
[bug 612929](https://bugzilla.gnome.org/show_bug.cgi?id=612929), there
don't seem to be any plans on fixing it (or even any ideas on what
might be wrong).  For the curious, the problem is that Banshee will
start to play a stream perfectly well, but stop after the second song.
This is slightly functional but requires too much babysitting.

This is an unfortunate state of affairs because, other than that, I do
like Banshee.  The more software written in a high-level language, the
better.  C# is better than C or C++.  Oh well, I also like to listen
to Last.fm.

There were some other minor annoyances, I suppose.  The notification
mechanism leaves something to be desired; notifications are tied to
the system tray plugin and do not work if you do not have a system
tray.  I guess this isn't a problem for most people, but I don't
really use a desktop environment (just XMonad) and lack a system tray.
Also, song changes have been pretty messy since the introduction of
gapless playback.  Gapless itself doesn't seem to work consistently,
and disabling it actually makes things worse and the first few seconds
of some songs play twice.  This is fairly annoying.

Alternatives
------------

There are lots of obvious alternative media players on Linux:

 * [Amarok](http://amarok.kde.org/)
 * [Audacious](http://audacious-media-player.org/)
 * [MPD](http://mpd.wikia.com/wiki/Music_Player_Daemon_Wiki)
 * [xmms2](http://xmms2.org/)

### Amarok

Lets get the most famous out of the way first.  In principle, I like
Amarok.  My primary complaints revolve around the audio backend:
Phonon.  There isn't much configurability there.  One of my machines
has a slightly fancy sound card and choosing the correct output for
Phonon seems to be either impossible or much more difficult than it
should be.

The build process is also slightly off-putting.  I work in an
environment where I don't have root access to install packages at the
system level, so I end up having to compile a lot myself.  I don't
actually have enough space for KDE4; this is slightly problematic.
Worse, building the embedded MySQL library is incredibly painful,
especially on 64-bit platforms.

On the topic of MySQL, one of the reasons from the switch from SQLite
to MySQL Embedded was supposed to be for speed.  I suppose I don't
have much music, but SQLite never gave me any trouble in the speed
department.  Amarok 2 with the embedded MySQL has yet to impress me
with the speed of collection management; on the contrary, it felt
fairly slow as recently as 2.2 (the last version I tried).

Anyway, Amarok 2 is eliminated from the race by technical constraints
at the very least.

### Audacious

I like Audacious a lot -- it is small and light on resources.  I
certainly use it for playing one-off files frequently.  That said, I
really like having some form of music library management available
(especially for easy searching), and Audacious can't (and probably
shouldn't) deliver on that front.

Also, the Last.fm plugin has been broken for quite some time.

### MPD

As the name implies, MPD is a headless daemon that plays music.  It is
very light on resources and is also very fast.  It plays more formats
than I have ever seen.  Until recently, I hated all of the GUI
frontends that were available.  Last time I used it,
[gmpc](http://gmpc.wikia.com/) was actually pretty good.  This is
still essentially the case.

I actually gave it another fair shot today and the Last.fm support
seems to be broken in the development version.  Oh well, maybe next
time.

### xmms2

The successor to the venerable xmms.  The architecture is completely
different from the original xmms and they have taken a music daemon
approach, similar in spirit to MPD.  This is an interesting project,
but still quite rough around the edges and a constantly moving target.
This, unfortunately, seems to have impacted UI development.  There
are many UIs that worked at one point or another, but have bitrotted
and are no longer compatible.  Compatibility aside, none have reached
any positively notable level of polish, completeness, or usability.

I've fought with xmms2 in the past and didn't feel like trying too
hard this time.  The notion of collections hasn't been sufficiently
explained for me to understand what it means or how they help me, much
less how to use them.  The frequent segfaults don't help much, either.
I couldn't tell whether or not there was Last.fm support, but I didn't
look too hard to try to find out.

### Others

There are many others, of course, that I didn't seriously consider.
Mozilla Sunbird seems to be dead or dying (and I wouldn't use a
XUL-based application anyway).  Decibel doesn't seem to have Last.fm
support and the resource leakage has impressed me in all the wrong
ways.  The story is similar for Exaile and Quod Libet.


Victor -- Clementine
--------------------

I eventually settled on
[clementine-player](http://code.google.com/p/clementine-player/).
This project seems to be a backlash against the UI changes of Amarok
2; it is an attempt to port the UI and ideas of Amarok 1.4 to Qt4.
Personally, I have nothing against the UI of Amarok 2 (I rather like
it, actually), but 1.4 was also perfectly acceptable to me.

It is written in C++, but I can forgive that.  It has been stable so
far and it has a surprisingly modest memory footprint (compared to
Banshee).  The Last.fm support works, and that basically decided
matters.  The song change notification situation is also rather
happier than under Banshee; it is separate from the system tray and
uses DBus and the freedesktop.org notification standard.

Building it by hand was a bit of a pain, but bearable.  I'm not sure
how the developers compile it, but I found myself needing to much with
the CMake build file quite a bit to add extra include directories and
libraries.  It depends on liblastfm, which is reasonable.
Unfortunately, two projects go by that name and they are not at all
API-compatible.  The actual version that is required is the
[Qt and C++-based one](http://github.com/mxcl/liblastfm).

The recommended audio backend is currently gstreamer, which seems
fairly reasonable to me.  They claim that it works on the three main
platforms (Windows, OS X, and Linux), though only Linux support really
matters to me.  It is not yet in the main Debian repository, but they
provide Debian (along with Fedora) packages on their project page.

Things I Want
-------------

I can't think of too many features that I might want which clementine
does not implement.  One thing that I do miss from Amarok 1.4 was
Musicbrainz-based tagging.  I won't hold my breath.
