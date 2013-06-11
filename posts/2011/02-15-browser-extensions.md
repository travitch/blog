---
title: Browser Extensions
tags: linux
date: 2011-02-15
---

I am not really a huge fan of browser extensions.  Most seem kind of
excessive or silly and can easily make the browser seem slower than it
really is.  That said, I do appreciate the extensions that let me
browse more easily without using a mouse.

The most important feature that I want from a browser is to be able to
easily select links to follow using the keyboard.  The typical user
interface for this feature is to label each link with _hints_ after
some keystroke is hit; you follow a link by typing in the hint
attached to the link you want.  In Firefox,
[vimperator](http://vimperator.org/vimperator "vimperator") does a
good job of providing this functionality.  As the name implies, this
extension brings vim-like behavior to the browser.  Of course, I
switched to Chrome a while ago.  The equivalent in Chrome is somewhat
more lightweight:
[vimium](https://chrome.google.com/extensions/detail/dbepggeogbaibhgnhhndojpepiihcmeb
"vimium")

That extension covers most of my requirements.  Unfortunately,
switching between tabs is still a pain, so I finally got around to
finding a more keyboard-friendly way to switch tabs.  The default
Chrome keybindings allow you to switch tabs with `Alt+<TabNum>`.
Unfortunately, that only works for up to 9 tabs.  I try not to open
that many at once on general principle, but sometimes it happens.
Even if you have fewer than 10 tabs open, you still have to remember
the numbers for each tab (or count each time).  I am bad at both
remembering things and counting, so this does not scale well for me.
Enter another (less slickly named) extension:
[Tab Title Search](https://chrome.google.com/extensions/detail/fbgmfenfjogaoibifpgolehkibnfalgn?hl=en
"Tab Title Search").  A keystroke (`Alt+g` by default) brings up a
dropdown list of your tabs with a search box.  You can type in a
fragment of any page title (or use a regular expression, apparently),
and just hit `enter`.  This is close enough to the buffer list in a
[proper text editor](http://www.gnu.org/software/emacs/ "emacs") that
I am fairly happy, finally.
