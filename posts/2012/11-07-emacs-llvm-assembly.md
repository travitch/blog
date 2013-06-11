---
title: An Emacs Function for LLVM Assembly
tags: emacs
date: 2012-11-07
---

I finally decided to use the power of emacs to automate a task that I
perform at least a dozen times per day: converting a snippet of C
source code into LLVM IR assembly.  The command to do this is simple:

~~~~~
clang -emit-llvm -c -o - file.c | opt -S
~~~~~

The only annoying part about using the command was finding a spare
terminal and finding the input file on the file system (and then
remembering where I put that terminal when I wanted to refer to it
later).  Enter emacs.  It took a while to navigate the emacs
documentation to figure out how to write what I wanted and learn the
relevant terminology.  The function I came up with pipes the current
buffer (or region, if it is active) to clang and opt, puts the output
in a new buffer, activates the
[llvm major mode](https://llvm.org/viewvc/llvm-project/llvm/trunk/utils/emacs/llvm-mode.el),
and switches to the new buffer.

~~~~~{.commonlisp}
(defun llvmize (&optional start end)
  "Convert the current buffer or region (containing C code) to LLVM assembly via clang and opt"
  (interactive)
  (let ((start (if mark-active (region-beginning) (point-min)))
        (end (if mark-active (region-end) (point-max)))
        (default-major-mode 'llvm-mode)
        (buf (generate-new-buffer "*llvm-asm*")))
    (set-buffer-major-mode buf)
    (shell-command-on-region start end "clang -emit-llvm -x c -c -o - - | opt -S -mem2reg -basicaa -gvn" buf)
    (set-buffer buf)
    (setq buffer-read-only t)
    (switch-to-buffer-other-window buf)))
~~~~~

This has a few nice benefits over just throwing LLVM IR assembly dumps
into random terminals:

 * Nice syntax highlighting
 * Standard emacs buffer management applies
 * Cuts down on terminal proliferation (a serious problem)

I feel like this style of function is generally useful and I should
probably make a few more to automate my life a bit.  This function
still needs a few improvements:

 * I want to look at the file extension (if there is one) of the
   active buffer so that I can use `-x c++` instead of always just
   using `-x c` for clang.
 * I also want to find the best clang and opt binaries on the system;
   debian installs opt as `opt-$version`, so this function doesn't
   work as-is.
 * I want to add some keymaps to the freshly-created buffer to easily
   quit and close the window when I am done with it.

Back to reading the emacs documentation.
