---
title: Linux Messaging Clients
tags: linux
date: 2015-11-09
---

I recently found myself looking at Linux instant messaging clients
again.  Sometimes I feel like some kind of Luddite for still using a
desktop instant messaging client, but I just cannot bring myself to
use a web application.  I have used pidgin for as long as I can
remember, but I have been looking for an alternative with better
support for primarily keyboard interaction.  My biggest complaints
about pidgin center around odd focus issues that sometimes seem to
require a mouse to fix.

Unfortunately, none of the GUI clients seem to offer decent
keybindings.  That leaves console clients.  I have tried finch, which
isthe console frontend to libpurple (which is itself the library that
provides all of the interesting functionality in pidgin).  finch comes
with an interesting console window manager built on top of ncurses
called gnt.  I found the default UI and keybindings to be maddening,
even more than most normal GUI applications.  That defeated the
purpose of switching.

gnt has a surprising feature: it supports alternative window manager
layouts through plugins.  I tried to use the one that mimics an
irssi-style client.  This interface superficially looked like irssi,
but the interaction model was still annoying and required awkward
focus changing commands.  Moreover, it interacted badly with terminal
resize events.  Whenever I would move the terminal to another monitor
with xmonad, the subsequent resize event would jumble the window
positions.  This caused awkward overlaps, which ruined the irssi-like
illusion that the window manager plugin was trying to present.  It
turns out that the plugin really just carefully arranges windows to
mimic the irssi interface, but only on application startup.  It would
not reflow the layout in response to changes.

For now, I have settled on [profanity](http://profanity.im).
Profanity is a console client that also mimics the irssi interface.
It is significantly more successful at doing so than finch.  The only
downside to profanity is that it only supports a single account.  It
is not a huge burden to use tmux to manage a few instances for
different accounts, though I would not object to multi-account
support.  It is worth noting that profanity only supports XMPP, which
is fine for my purposes.  It seems to support all of the XMPP
extensions that I need (and then some), including multi-user chats.  I
had to spend a bit of time configuring things properly, but I
eventually managed to get it working well with tmux and my xmonad
configuration.  Most of this configuration was related to
notifications.  I use X window urgent hints to show activity
notifications in my xmonad bar
([taffybar](https://github.com/travitch/taffybar)).  Profanity easily
supports this mode of operation with the
[/flash](http://profanity.im/reference.html#flash) configuration
option.  There was a bit of an issue with running it in a tmux
session, though; tmux would set the urgent hint when there was *any*
console activity at all.  Annoyingly, this included _clock_ updates in
the profanity status bar.  I had to disable that with the
[/time](http://profanity.im/reference.html#time) configuration option,
which was unfortunately a new addition as of 0.4.7.  Of course, the
version shipped with Ubuntu 15.10 is 0.4.6.

Overall, I am pleased with profanity so far.  We shall see if I can
stand it in a week.
