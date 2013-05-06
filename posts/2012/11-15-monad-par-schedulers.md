---
title: Divide and Conquer with Monad Par
tags: haskell, programming
date: 2012-11-15
---

Recently I decided to parallelize part of my set constraint solver
[ifscs](https://github.com/travitch/ifscs), which I plan to write more
about eventually.  At one point, the constraint solver has a large set
to process where each element is really independent: a prefect
situation for simple coarse-grained parallelism.  I had a good experience
using [monad-par](http://hackage.haskell.org/package/monad-par) at another
place in my code, so I decided to try my luck again.  After a bit of fooling
around, I came up with the following:

~~~~~{.haskell}
import Control.DeepSeq
import Control.Monad.Par.Scheds.Direct
import Data.Foldable ( Foldable )
import qualified Data.Foldable as F
import qualified Data.Sequence as S

mapReduceThresh :: (Foldable f, NFData b)
                   => Int -- ^ Threshold to start serial computation
                   -> f a -- ^ A foldable sequence of elements to map over
                   -> (a -> b) -- ^ A map operation to apply
                   -> (b -> b -> b) -- ^ Combine results
                   -> b -- ^ Seed value
                   -> b
mapReduceThresh thresh elts fn red seed =
  runPar $ go s0
  where
    s0 = S.fromList (F.toList elts)
    mapred !a !b = fn b `red` a
    go s
      | S.length s <= thresh =
        return $ F.foldl' mapred seed s

      | otherwise = do
          let mid = S.length s `quot` 2
              (p1, p2) = S.splitAt mid s
          rpart <- spawn $ go p2
          l <- go p1
          r <- get rpart
          return $ red l r
~~~~~

This is very similar to the divide-and-conquer example from the
original
[monad-par paper](http://research.microsoft.com/en-us/um/people/simonpj/papers/parallel/monad-par.pdf).
The only real difference is that it takes any `Foldable` as an input
and provides a bit less control over problem splitting.  It splits
into fixed-sized chunks (specified via the threshold parameter).
Internally, it uses `Data.Sequence` for efficient splitting.

Since the code is so simple, I was confident.  So, of course, the
first time I ran it, it crashed with an "`MVar` blocked indefinitely"
error.  I spent about an hour poking at it trying to figure out what I
had done wrong.  This was alarming because the code was very simple
and I have a much more complex use of monad-par elsewhere in my code,
and that has worked fine for months.

After a while, I gave up and switched to the
[parallel](http://hackage.haskell.org/package/parallel) package, which
is based on sparks instead of explicit threads.  I was still curious,
though, so I decided to read through the bug tracker for monad-par.
There, I found
[issue 21](https://github.com/simonmar/monad-par/issues/21), which
described my problem exactly.  Apparently, nested parallelism with the
default scheduler is broken in the current release (0.3) on Hackage.
This made sense based on my observations: simple divide-and-conquer
failed, but my complex case did not use any nested scheduling.  The
workaround was to explicitly use the `Direct` scheduler (as in the
code above).  Hopefully the bug will get fixed soon or the default
scheduler will be switched to one that works a bit more reliably.
