+++
title = "Avy Interactive"
date = 2020-12-20
[taxonomies]
tags = ["emacs"]
+++

The excellent [avy](https://github.com/abo-abo/avy) package for emacs provides an efficient mechanism for positioning the cursor in the current window.  The avy workflow is:

1. Active avy via one of several possible functions
2. Type a few characters corresponding to the location on screen you want to navigate to
3. Avy will highlight each occurrence of those characters with navigation hints
4. Type the one or two letters corresponding to the desired destination

Avy typically enables navigation to any position on screen in four or five keystrokes.  There are variants to highlight candidates based on a single character (`avy-goto-char`), two characters (`avy-goto-char-2`), a character at the beginning of a word (`avy-goto-word-1`), or an arbitrary number of characters entered within a short time limit (`avy-goto-char-timer`).

I find that the one and two character variants produce too many matches for me to easily visually identify my target. As a result, I have been using `avy-goto-char-timer`, which allows you to enter any number of characters; after a timeout following the final character, avy activates and highlights matches for selection.

The `avy-goto-char-timer` function, unfortunately, does not provide a means for correcting typos.  This minor annoyance has kept me from using avy as much as I would like for navigation within a buffer.  I finally decided to fix it by introducing my own variant:

```lisp
(defun tr/avy-prompt (s)
  "Prompt for an avy target and activate avy with it."
  (interactive "MAvy: ")
  (avy-process (avy--regex-candidates s)))
```

This function (which I bind to `C-]`) simply uses the minibuffer to prompt for a string and activates avy on that string. Since the string is entered in the minibuffer, all normal editing commands work on it.  A future variant could potentially use `completing-read` to provide a filtered list of candidates (e.g., via [selectrum](https://github.com/raxod502/selectrum).
