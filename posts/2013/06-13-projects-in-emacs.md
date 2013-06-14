---
title: Project Management in Emacs
date: 2013-06-13
tags: linux, emacs
---

Over the years, I have tried many different "project management"
libraries for emacs -- enough that I do not even remember which ones.
I do not really remember finding one that did what I wanted (or could
be coaxed into it within any reasonable amount of time).  Sometime
last year I stitched together something rudimentary that serves my
workflow very well.  With my recent emacs configuration rewrite,
I took the opportunity to refine my solution into something slightly
nicer.  I am documenting it here partly so I do not forget what I
did, but it may be useful to someone else.

## Requirements

First, let me outline my simple requirements for project management:

 * I need to be able to build tags (and have them automatically loaded)
   for a project.

 * I need to be able to easily build a project.

Note that my projects do not necessarily have a one-to-one
correspondence with source code repositories.  I have a few large
projects split over multiple repositories that I would prefer to treat
as a single projects.  In particular, I want a single `TAGS` file that
has tags spanning multiple repositories for easy source code
navigation.

## Solution

I ended up using simple
[directory local variables](http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html).
This emacs feature lets you put a file named `.dir-locals.el` at the
root of a source tree.  When a file is loaded, the first
`.dir-locals.el` above it in the file system is loaded.  Variables
defined in `.dir-locals.el` are defined as buffer locals in the opened
file.  I define two directory local variables for each project:
`tag-builder` and `build-method`.  I use the following elisp to build
my project management functionality:

```commonlisp
;;; Tag support
(defun make-tags-hook ()
  (when (boundp 'tag-builder)
    (let ((project-root (locate-dominating-file buffer-file-name ".dir-locals.el")))
      (let ((cmd (format "cd %s ; %s &" project-root tag-builder)))
        (call-process "bash" nil 0 nil "-c" cmd)))))

(add-hook 'after-save-hook 'make-tags-hook)

(defun set-project-tags ()
  (let ((project-root (locate-dominating-file buffer-file-name ".dir-locals.el")))
    (let ((tags-file (format "%s/TAGS" project-root)))
      (when (file-readable-p tags-file)
        (visit-tags-table tags-file)))))
(add-hook 'find-file-hook 'set-project-tags)

;;; Compile support
(defun set-compile-command ()
  "Set the compile command based on a `.dir-locals.el' build-method"
  (when (boundp 'build-method)
    (let ((project-root (locate-dominating-file buffer-file-name ".dir-locals.el")))
      (setq compile-command (format "cd %s ; %s" project-root build-method)))))
(add-hook 'find-file-hook 'set-compile-command)
```

The `make-tags-hook` function rebuilds the tags for an entire project
and is run after each file is saved.  In practice this has been fast
enough for me.  I have only tried it on ~20,000 lines of Haskell code,
though.  Simply, it finds the closest `.dir-locals.el` file and
assumes that is the project root.  It changes to that directory and
runs the tag building command from the directory local variable file.

The next function, `set-project-tags` selects the appropriate `TAGS`
file every time a new file is opened (using `visit-tags-table`).  This
is not exactly ideal.  Only one `TAGS` file can be loaded at a time,
normally, so working with multiple projects in one emacs session
would not work perfectly.  Luckily, I do not do that very often.
This shortcoming could be fixed with something like
[etags-table](http://www.emacswiki.org/emacs/EtagsTable).  I might
try to do that soon.

The last function deals with compilation support.  It uses the
`build-method` directory local to define an appropriate
`compile-command` for the project.  The key functionality here is that
the computed `compile-command` changes directory to the root of the
project before running the build command.  Again, the project root is
determined based on the location of `.dir-locals.el`.  Since this just
modifies `compile-command`, the normal build infrastructure (like `M-x
compile`) Just Works.

## Notes

I used unconventional names for my directory local variables.  I
wanted to use `build-command` and `tag-builder-command`, but there is
an interesting quirk in directory local variables.  For security
purposes, only variables explicitly marked "safe" in emacs are loaded
without user interaction.  The user is interactively prompted before
so-called "risky" variables are loaded.  These "risky" variables can
be trusted (and recorded in `custom.el`) easily.  Variables that are
fundamentally unsafe, or ending in "-command" (or a few other
suffixes) can be loaded explicitly by the user, but cannot be marked
as safe in `custom.el`.  This was very annoying, so I just chose some
unconventional names to work around it.  This is in some sense
"unsafe", but you are still prompted to accept new values of these
variables the first time they are encountered.  This is a fine
tradeoff for simplicity and convenience, for me.

