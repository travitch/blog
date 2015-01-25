---
title: Arrays in Haskell
tags: haskell
date: 2015-01-24
---

Relative to many other languages, the humble array is underrepresented
in Haskell.  That may follow from the difficulty of fitting arrays
into pure code; an entire array to update a single element rarely
makes sense.  There are some immutable data structures that
[look like arrays](https://hackage.haskell.org/package/persistent-vector),
but are not actually.  Mutable arrays (in `ST` or `IO`) make more
sense, so I want to write up a few notes about them.

Haskell has two general purpose array libraries:
[array](https://hackage.haskell.org/package/array) and
[vector](https://hackage.haskell.org/package/vector).  There are also
some special-purpose libraries like
[repa](https://hackage.haskell.org/package/repa), which enables
efficient (and implicitly parallel) numeric computations.  The vector
package provides a flexible and efficient array abstraction, with
support for fusing many operations.  In contrast, the array package
has a reputation as archaic and slightly odd; certainly, the API
specified in the Haskell report is strange when compared with the API
provided by vector, which more closely mirrors the APIs provided by
containers and the other data structure libraries in the Haskell
ecosystem.  That is a fair criticism.  However, if you do not need the
features in vector, array is still worth a look.

# Why Array

If you just need contiguous (and possibly _unboxed_) storage of values
with `Int` (or `Int`-like) keys with support for O(1) reads and
writes, array is still a very good choice.  For this use case, array
even has a significant advantage over vector: the index into the array
can be any type implementing the `Ix` typeclass (for “indexable”).
This means that you can define extra types (perhaps as a `newtype`
around `Int`) as array indexes.  I have found this to be immensely
useful, as it prevents me from accidentally indexing an array with the
wrong `Int` that I had lying around.

# Unboxed data

Safe indexing and O(1) updates are undeniably useful.  However, one of
the real strengths of arrays (and vectors) is that they support
_unboxed_ data.  In a boxed array, elements are stored via pointer.
In an unboxed array, the elements are stored directly in the allocated
array.  This has a few nice implications:

* it removes a layer of pointer indirection,
* the elements are all stored contiguously, and
* the garbage collector does not need to scan the elements.

In short, if you can store your data in an unboxed format, it is
usually worth it.  There is one restriction: you cannot store
unevaluated thunks in an unboxed container.  Since that extra layer of
indirection has been removed, all of the elements are always fully
evaluated.  In many cases, this is actually also a good thing.

Both array and vector have a default set of instances allowing them to
store most of the unboxable types in base.  But what if we want to
store our own types in unboxed arrays?  The answer turns out to be a
little complicated.

## Unboxed mutable vectors

The vector package has a few classes that need to
be implemented for your type `T` (in the case of mutable vectors):

* `Data.Vector.Generic.Mutable.MVector Data.Vector.Unboxed.Mutable.MVector T`
* `Unbox T`

It really is not obvious how to implement them by hand.  You need to
root around in the implementation of vector to figure it out.
Luckily, these instances can be derived with
`GeneralizedNewtypeDeriving`.  Or, they could, until ghc 7.8 made
`GeneralizedNewtypeDeriving`
[safe](https://ghc.haskell.org/trac/ghc/ticket/9112).  If you are
willing to use Template Haskell, the
[vector-th-unbox](http://hackage.haskell.org/package/vector-th-unbox)
package can generate the necessary instances.


## Unboxed mutable arrays

The story for array is not much simpler.  I could not find much
documentation on this issue, so hopefully this post will shed some
light on these darker corners of the library ecosystem.  In order to
store a type `T` in an unboxed mutable array, you only need one
instance (at least for the `IO` variant):

* `MArray IOUArray T IO`

This is a bit problematic.  First, it does not really fit the form we
would expect for a (`GeneralizedNewtypeDeriving`) deriving clause.
Normally, we would expect the `T` to come last, but `MArray` is just
not defined that way.  That is okay – we can work around that with
`StandaloneDeriving`:

```{.haskell}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

import GHC.IO ( IO(..) )
import qualified Data.Array.IO as IOA

newtype T = T Int

deriving instance IOA.MArray IOA.IOUArray T IO
```

We have to import the constructor for `IO` these days, or GHC
complains.  It turns out that GHC still complains:

```
    Can't make a derived instance of ‘IOA.MArray IOA.IOUArray T IO’
      (even with cunning newtype deriving):
      cannot eta-reduce the representation type enough
    In the stand-alone deriving instance for
      ‘IOA.MArray IOA.IOUArray T IO’
```

I confess that I do not really know what that means, aside from it
just not working.  Perhaps GHC does not know what parameter it is
supposed to be deriving for.  In any case, that leaves us with one
option: we get to write the instance ourselves.  Doing so is
unpleasant, but at least it is fairly obvious from the existing
instances defined in array.  For our type `T`, which is a `newtype`
around `Int`, the instance looks like the following:

```{.haskell}

import qualified GHC.Base as B
import GHC.ST ( ST(..) )
import Control.Monad.ST ( stToIO )

import qualified Data.Array.IO as IOA
import qualified Data.Array.IO.Internals as IOA
import qualified Data.Array.MArray as MA
import qualified Data.Array.Base as BA

instance MA.MArray (BA.STUArray s) T (ST s) where
  {-# INLINE getBounds #-}
  getBounds (BA.STUArray l u _ _) = return (l, u)
  {-# INLINE getNumElements #-}
  getNumElements (BA.STUArray _ _ n _) = return n
  {-# INLINE unsafeRead #-}
  unsafeRead (BA.STUArray _ _ _ marr#) (B.I# i#) = ST $ \s1# ->
    case B.readIntArray# marr# i# s1# of
      (# s2#, e# #) -> (# s2#, T (B.I# e#) #)
  {-# INLINE unsafeWrite #-}
  unsafeWrite (BA.STUArray _ _ _ marr#) (B.I# i#) (T (B.I# e#)) = ST $ \s1# ->
    case B.writeIntArray# marr# i# e# s1# of
      s2# -> (# s2#, () #)
  unsafeNewArray_ (l, u) = BA.unsafeNewArraySTUArray_ (l, u) BA.wORD_SCALE

instance MA.MArray IOA.IOUArray T IO where
  {-# INLINE getBounds #-}
  getBounds (IOA.IOUArray arr) = stToIO $ BA.getBounds arr
  {-# INLINE getNumElements #-}
  getNumElements (IOA.IOUArray arr) = stToIO $ BA.getNumElements arr
  {-# INLINE unsafeRead #-}
  unsafeRead (IOA.IOUArray arr) i = stToIO $ BA.unsafeRead arr i
  {-# INLINE unsafeWrite #-}
  unsafeWrite (IOA.IOUArray arr) i e = stToIO $ BA.unsafeWrite arr i e
  unsafeNewArray_ bounds = stToIO (IOA.IOUArray <$> BA.unsafeNewArray_ bounds)

```

Entirely unpleasant, but worth the effort.  There are a few important
things to note about these instances:

* You need to replace calls to `readIntArray`/`writeIntArray` and the
  `I#` constructors with variants appropriate for your underlying
  type.  If you have a type that is not a `newtype` around some
  existing type, you will need to write your own variants.
* The `wORD_SCALE` function is defined in Data.Array.Base, along with
  a few others.  You also need to choose the appropriate scale (which
  is basically the stride in the array) for your type.  Again, you may
  need to define your own for custom types.
* While it may look like a function you would never call, you still
  need to implement `unsafeNewArray_`.  The default implementation of
  `newArray`, which you probably do not want to implement, calls it.
  The default implementation of `unsafeNewArray_` fills the array
  elements with a call to error.  This is fine for boxed arrays, but
  immediately bottoms out when it is evaluated to be put into an
  unboxed array.  The call to `unsafeNewArraySTUArray_` just allocates
  some memory and leaves its contents undefined in the traditional C
  sense.

The substantive definition is for `ST`, with a trivial wrapper lifting
it into `IO`.  You probably want to keep all of the `INLINE` pragmas,
as most of this code will get compiled away nicely.

# Conclusion

Unboxed data can be a bit of a pain to work with in Haskell
(especially with the recent changes to `GeneralizedNewtypeDeriving`),
but it is often worth it when performance is important.  I have tried
to document the steps required to unbox custom types with the array
library.  I also argue that array is still a useful library, despite
its quirky API.

