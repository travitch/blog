+++
title = "Unusual Emacs keybindings"
date = 2015-02-13
[taxonomies]
tags = ["emacs"]
+++

I recently ran into a strange problem with running emacs.  A few days ago, my delete, page up, and page down keys stopped working, but only when emacs was run in the terminal.  Whenever I hit one of those keys, emacs would complain that the region was empty.  This was baffling for a while.  Searching the internet turned up nothing, so I had to assume that it was something I had done.  One further symptom manifest: when I used my `M-[` keybinding, the bound function would execute and it inserted `3~` into the buffer.  That was unusual.

It turns out that binding `M-[` to anything is a bad idea because Meta and Escape are the same in emacs.  It also turns out that `Escape-[` is the prefix for a few keycodes, including delete, page up, and page down.  My delete key was being intercepted and invoking my keybinding.  When the associated function was successful, the rest of the delete keycode was inserted into the buffer.

Lesson learned: do not bind `M-[`.
