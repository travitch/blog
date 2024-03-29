+++
title = "Funny Type Errors Issue 1"
date = 2020-04-19
[taxonomies]
tags = ["haskell"]
+++

I find that I run into a large number of what I would consider Funny Type Errors while using GHC.  This is the first post in what will presumably be a long series of posts on the topic.

Today I was working on trying to improve the performance of a codebase that uses a large number of the type system extensions provided by modern GHC including `-XGADTs` and `-XTypeInType`.  Seeing strange type errors in this code is expected on a daily basis.  However, the code I was working in today was not doing anything _too_ unusual.  A reduced example of the surprising code is:

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -Wall -Wcompat #-}
import qualified Data.Functor.Const as C
import           Data.Kind ( Type )

data Pair (a :: k -> Type) (b :: k -> Type) where
  Pair :: a tp -> b tp -> Pair a b

data MonoStruct =
  MonoStruct { item :: Pair (C.Const Int) (C.Const Int)
             , otherData :: Int
             }

getFirst :: MonoStruct -> Int
getFirst ms =
  let Pair a _b = item ms
  in C.getConst a

update :: MonoStruct -> Int -> MonoStruct
update ms f = ms { item = Pair (C.Const f) (C.Const f) }
```

Naively, I expected this to compile.  Instead, GHC (8.6.5 and 8.8.3) report the errors:

```
❯ ghci poly.hs
GHCi, version 8.8.3: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( poly.hs, interpreted )

poly.hs:18:19: error:
    • Cannot use record selector ‘item’ as a function due to escaped type variables
      Probable fix: use pattern-matching syntax instead
    • In the expression: item ms
      In a pattern binding: Pair a _b = item ms
      In the expression: let Pair a _b = item ms in C.getConst a
   |
18 |   let Pair a _b = item ms
   |                   ^^^^

poly.hs:22:15: error:
    • Record update for insufficiently polymorphic field:
        item :: Pair (C.Const Int) (C.Const Int)
    • In the expression: ms {item = Pair (C.Const f) (C.Const f)}
      In an equation for ‘update’:
          update ms f = ms {item = Pair (C.Const f) (C.Const f)}
   |
22 | update ms f = ms { item = Pair (C.Const f) (C.Const f) }
   |               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

This is very strange: the ~MonoStruct~ type does not contain any polymorphic fields.  In fact, I called it `MonoStruct` in this example because it looks entirely monomorphic.  To dig a little deeper, we can ask `ghci` for some additional information:

```haskell
Prelude> :set -fprint-explicit-foralls
Prelude> :set -fprint-explicit-kinds
Prelude> :load poly.hs
[1 of 1] Compiling Main             ( poly.hs, interpreted )
Ok, one module loaded.
*Main> :info MonoStruct
data MonoStruct
  = forall {k}.
    MonoStruct {item :: Pair k (C.Const k Int) (C.Const k Int)}
  	-- Defined at poly.hs:11:1
```

This shows us what is really happening.  Note the definition of `Pair`: it takes two data items that are indexed by a poly-kinded type parameter (`k`).  This has an unfortunate interaction with the use of ~Const~ as the data elements, as ~Const~ is also kind polymorphic.  This means that while the type of the `Pair` in `MonoStruct` looks monomorphic, the _kind_ of the pair could change on update, which causes the error we see from `ghci`.  As a concrete example, we could populate our `Pair` with `Const Int Int` or `Const Int '[Natural]`.  Specializing the type put into the `Pair` removes the unwanted polymorphism:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -Wall -Wcompat #-}
import           Data.Kind ( Type )

data Tag = A | B

data ConstInt (k :: Tag) = ConstInt { getConstInt :: Int }

data Pair (a :: k -> Type) (b :: k -> Type) where
  Pair :: a tp -> b tp -> Pair a b

data MonoStruct =
  MonoStruct { item :: Pair ConstInt ConstInt
             }

getFirst :: MonoStruct -> Int
getFirst ms =
  case item ms of
    Pair a _b -> getConstInt a

update :: MonoStruct -> Int -> MonoStruct
update ms f = ms { item = Pair (ConstInt f) (ConstInt f) }
```
