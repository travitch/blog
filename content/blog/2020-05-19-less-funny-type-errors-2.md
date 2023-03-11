+++
title = "Less Funny Type Errors Issue 2"
date = 2020-05-19
[taxonomies]
tags = ["haskell"]
+++

This is part two of the presumably unending type errors series.  The previous entry involved an amusing type error.  This time the error is a bit less amusing:

```
ghc: panic! (the 'impossible' happened)
  (GHC version 8.8.3 for x86_64-unknown-linux):
        exprToType
  Call stack:
      CallStack (from HasCallStack):
        callStackDoc, called at compiler/utils/Outputable.hs:1159:37 in ghc:Outputable
        pprPanic, called at compiler/coreSyn/CoreSyn.hs:1997:28 in ghc:CoreSyn

Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug
```

Unfortunately, there was no additional context to help narrow down the problem.  The file in question has a few hundred lines of code.  My starting set of observations was:
- The code compiles and works on GHC 8.6.5 (so can't be too bad)
- It used to compile and work under GHC 8.8.3 (so there were no compile errors)
- There were some warnings (but they didn't seem particularly relevant)

I had vague memories of seeing this error before in another context and moving code around until the error went away.  I don't remember any details besides that approach to debugging being unsatisfying.  Ultimately, I took a guess that:
1. GHC was attempting to print a warning related to inferred types
2. GHC has recently started warning about type family applications appearing as kinds

There was one function in the file in question that had an explicitly quantified kind variable meeting those conditions.  Adding an explicit (dependent) kind annotation to that variable caused the crash to go away.
