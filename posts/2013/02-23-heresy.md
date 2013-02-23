---
tags: linux
title: Heresy
date: 2013-02-23
---

Don't tell anyone, but I decided to try to use vim as my primary editor for a
week.  I have actually used both vim and emacs for years, but I usually only
used vim for viewing files and emacs for editing them.  This week is vim full
time.  I like emacs, but it has been doing something annoying that I haven't
quite been able to track down.  There is some keystroke that is very much like
"save file" that I keep hitting that seems to just lock up my emacs window.
To recover, I inevitably have to kill my entire emacs session (since I use
the emacs server).  Very minor, but annoying.

I already have vim set up to do everything I want, really.  Its tag support
seems to be a bit better, and I typically use tags a lot.  The startup time of
vim is much shorter, which is nice.  The one emacs extension that I use the
most is [magit](http://philjackson.github.com/magit/) (a git repository manager
in emacs), and I worried that I wouldn't be able to find a vim equivalent.  How
naive.  [vim-fugitive](https://github.com/tpope/vim-fugitive) fills the same
role in vim.  It is not quite as polished, especially the interface for adding
only part of a change (equivalent of `git add -p`).  However, it gets the job
done admirably.  Of course, I was missing the function I wrote for emacs to
turn a buffer of C code into LLVM assembly.  Here is a vim version:


```
" This is a convenience function to turn a C function into LLVM bitcode,
" displaying the result in a split window.
command! LLVMIZE call LLVMToNewWindow()

function! LLVMToNewWindow()
  let TempFile = tempname()
  let SaveModified = &modified
  exe 'w ' . TempFile
  let &modified = SaveModified
  exe 'split ' . TempFile
  if executable('opt')
    let opt = 'opt'
  elseif executable('opt-3.2')
    let opt = 'opt-3.2'
  elseif executable('opt-3.1')
    let opt = 'opt-3.1'
  elseif executable('opt-3.0')
    let opt = 'opt-3.0'
  endif

  exe 'silent %! clang -x c -emit-llvm -c -o - - | ' . opt . ' -S -mem2reg -basicaa -gvn'

  setlocal nomodifiable
  setlocal buftype=nofile nowrap
  setlocal syntax=llvm
endfunction
```

I would say this is a very unpleasant language to work in, but it is about
as usable as elisp at the end of the day.
