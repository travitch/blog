---
title: TAGS for Haskell
tags: haskell, emacs, linux
date: 2015-02-08
---

I periodically try to use TAGS to navigate round my code in emacs.
When it works, it is very convenient.  I have not been using them
lately, partly because generating the tags and keeping them up-to-date
has always been a bit fiddly.  In the past, I have tried to get emacs
to automatically regenerate my tags when I save changes to a file.  I
have had solutions that work to a greater or lesser extent, but they
are always a bit unsatisfying.  Inevitably, I end up having to write
an extra shell script to generate the tags.  Usually, I need a script
because I often work in projects that span more than one repository or
cabal project.  The simple solutions I have seen for tag management
all assume that all of your code lives under one project root.

This time, given that I will probably require some kind of shell
script anyway, I decided to try a more UNIX-y approach.  My tag
generation script now uses `inotifywait` (from
[inotify-tools](https://github.com/rvoicilas/inotify-tools/wiki)) to
watch for filesystem changes and rebuild my tags when necessary:

```{.bash}
#!/bin/bash

DIRS="*/src"
EVENTS="-e close_write -e move"

inotifywait ${EVENTS} -mr ${DIRS} | while read event ; do
  hasktags -e -o TAGS ${DIRS}
done;
```

This says: whenever a file under `src` in my repository is closed
(after some writes) or moved, run
[hasktags](https://github.com/MarcWeber/hasktags) to rebuild the TAGS
file.  So far, it works remarkably well and emacs does not need to
know anything about the process.  `inotifywait` only works on Linux; I
am still looking for a corresponding utility for OS X.

While this approach produces a nice TAGS file, I still have a small
problem with the `find-tag` function in emacs.  This function lets you
jump to the tag for the identifier under the cursor.  The default
function does not know anything about the qualified module imports
typical of Haskell code, and includes the module qualifier in the
suggested tag name.  This does not work, because the TAGS file does
not record the qualifier (and it cannot do so, since you can choose a
different qualifier each time you import the module).  You can always
type in whatever name you want when using `find-tag`, but having the
default chosen by `find-tag` would be ideal (and faster).  To get
around this, I stole and adapted a bit of code from clojure-mode that
addresses this very same problem:

```{.commonlisp}
(defun find-tag-without-module (next-p)
  (interactive "P")
  (find-tag (first (last (split-string (symbol-name (symbol-at-point)) "\\.")))
            next-p))

(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "M-.") 'find-tag-without-module)))
```

So far, everything is working well.
