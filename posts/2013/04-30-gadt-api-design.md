---
title: GADTs in API Design
date: 2013-04-30
tags: haskell
---

Right after my [hoopl
experience](http://nochair.net/posts/2013/02-20-a-hoopl-experience.html) in
February, I meant to write more about
[GADTs](http://www.haskell.org/ghc/docs/latest/html/users_guide/data-type-extensions.html#gadt)
and how I have found them to fit into API design.  Obviously, I got a bit
distracted.  Broadly speaking, before I get into too many details, GADTs are
very powerful, but seem most useful _within_ a module and not exposed to
clients of an API.  I will try to detail a few reasons why I came to feel that
way, though obviously I might change my mind one day.

# Introduction to GADTs

Before I start, I will introduce the basic idea of GADTs, as far as I
understand them.  For more details, see the [GHC
manual](http://www.haskell.org/ghc/docs/latest/html/users_guide/data-type-extensions.html#gadt),
which also links to some relevant papers on the underlying type theory.  GADTs
are Generalized Algebraic Data Types -- as the name implies, they are a
generalization of normal Algebraic Data Types (ADTs).  In what way are they
more general?  Consider a normal Haskell ADT:

```haskell
data Example = E1 Int Int
             | E2 Double
             | E3 String
```

This declaration creates three functions (constructors):

```haskell
E1 :: Int -> Int -> Example
E2 :: Double -> Example
E3 :: String -> Example
```

The important thing to note is that all three have the same return type:
`Example`.  The generalization of GADTs allows you to define data types
whose constructors return _different_ types.  For example:

```haskell
{-# LANGUAGE GADTs, DataKinds #-}

data ETag = Numeric | Stringlike

data Example tag where
  E1 :: Int -> Int -> Example Numeric
  E2 :: Double -> Example Numeric
  E3 :: String -> Example StringLike
```

Note that the first two constructors return one type, while the third
constructor returns a different type.  How is this useful?  Consider
a function like this:

```haskell
example1 :: Example Numeric -> Int
example1 (E1 i1 i2) = i1 + i2
example1 (E2 d) = floor d
```

Compiled with `-Wall`, this function will produce no warnings.  The compiler
can prove that it has full case coverage because (1) `Example` is a GADT and
(2) only the `E1` and `E2` constructors have the `Numeric` tag.  Since
`example1` takes only `Example` instances with the `Numeric` tag, it covers all
cases.  Furthermore, adding a case for `E3` to `example1` would be an error.
I believe this works because the act of pattern matching on a GADT constructor
introduces new constraints (derived from the list of constructor return types
in the GADT declaration) to the type checker.  Consider:

```haskell
driver1 :: Example a -> Int
driver1 e =
  case e of
    E1 _ _ -> example1 e
    E2 _ -> example 1 e
    E3 _ -> 0
```

Obviously, this is a contrived example.  But notice that `driver1` is given
only an `Example a` -- the tag is unknown.  In the first two cases, though, the
pattern match introduces a new constraint, `tag == Numeric`, because the GADT
declaration says that the only way to construct an `E1` or `E2` yields an
`Example Numeric`.  At their core, GADTs let you make your code more type safe.
Consider that the alternative to `example1` without GADTs is something like:

```haskell
example2 :: Example -> Int
example2 (E1 i1 i2) = i1 + i2
example2 (E2 d) = floor d
example2 _ = error "Impossible"
```

There are other variants:

 * You could omit the last case and just ignore the warning about an
   unhandled case.

 * You could replace the placeholder with an actual case for `E3`, still
   calling `error`.

The problem with `example2` is that, if you add another constructor `E4` that
should be handled, you get no warning.  The catch-all case takes care of it by
erroring out, whether or not you wanted that.  The first alternative is
annoying during development, but may help catch such errors.  On the other
hand, it may add, in aggregate, enough extra warnings that you simply miss one
more when you add a new constructor.  The second alternative is reasonable and
can help catch forgotten cases when adding a new case.  However, it can be
extremely inconvenient for data types with many constructors (say, twenty or
so).

## Disadvantages

So GADTs let us make code more type safe.  We always like that, so why not
use them everywhere?  There are a few conveniences you give up by turning
your data type into a GADT:

 * You cannot use record selectors on GADT values.  You can declare GADT
   records, but you must extract values through pattern matching.

 * Record fields cannot share names unless they also share a return type
   in the GADT constructor.  This is not really an extra constraint, in a
   sense, because you could not do that with ADTs anyway (where all constructors
   shared a single return type.  It is still annoying, though, and one reason
   I think I will avoid them in public APIs.

 * Pattern matching on a GADT (the only way to extract information) requires
   the `-XGADTs` extension to be enabled at the pattern match site.  This
   means that the implementation detail (that you used GADTs) leaks out to
   all clients of the code.  This is another reason that I am less than keen to
   design a public API around them.

 * GADTs can also complicate type inference and introduce more cases where
   explicit type signatures are required.  I like adding type signatures
   anyway, but code typed into `ghci` could also be affected negatively.

# My GADT experiment

That was a quick introduction to GADTs - there are more details in the GHC
manual.  Now I will explain a less contrived example, reduced from my
experiment with GADTs in a larger API.  This experiment was inspired by my
experience with hoopl, which uses GADTs to classify CFG nodes as being open or
closed on entry and exit.  I covered some of those details in my hoopl post.
The important point is that *terminator* instructions (i.e., instructions that
can only come at the end of a basic block) have a type-level GADT tag.  In
hoopl, this had some nice properties and I saw a few places in the rest of my
code.  For background, most of my code analyzes programs represented in the
LLVM IR, which is a three-address code in SSA form.  It can be thought of as a
simple abstract instruction set with arithmetic operations, memory operations,
and control flow operations.  For the sake of argument, assume a simplified
LLVM IR ADT like this:

```haskell
data Instruction =
    AddInst Value Value
  | LoadInst Value
  | StoreInst Value Value
  | BranchInst BasicBlock
```

Here, `BranchInst` is a terminator instruction.  As in hoopl, I have a number
of functions that operate only on terminator instructions (and also a few other
general classes of instruction).  As in the examples in part one of this post,
I would like to write my functions that work only on terminator instructions
without resorting to partial functions or catch-all cases.  My experiment began
with converting my `Instruction` data type to a GADT:

```haskell
data ITag = Terminator | Other
data Instruction tag where
  AddInst :: Value -> Value -> Instruction Other
  LoadInst :: Value -> Instruction Other
  StoreInst :: Value -> Value -> Instruction Other
  BranchInst :: BasicBlock -> Instruction Terminator
```

So far, so good.  As before, this works fine and I can write convenient functions
over just `Terminator` instructions with no catch-all cases or partial pattern
matches.

```haskell
analyzeTerminator :: Instruction Terminator -> Result
analyzeTerminator (BranchInst _) = ...

analyzeInstruction :: Instruction a -> Result
analyzeInstruction i =
  case i of
    t@BranchInst _ -> analyzeTerminator t
    _ -> ...

analyzeFunction :: [Instruction a] -> Result
analyzeFunction = foldr analyzeInstruction mempty
```

Notice that I can pass any type of `Instruction` to `analyzeInstruction` and
still recover the tag by pattern matching.  `analyzeFunction` also type checks
and I can analyze an entire function easily.  However, `analyzeFunction`
raises an important question: how can I create this `[Instruction a]`?  The
constructors for the `Instruction` type return one of two types: `Instruction
Other` and `Instruction Terminator`.  Neither of those is `Instruction a`.
This would be easy to do with an existential wrapper around `Instruction`:

```haskell
data AnyInstruction = forall a . AI (Instruction a)
```

Unfortunately, that is somewhat inconvenient.  It adds an extra layer to
pattern matches everywhere in the code just to support (poorly) the few cases
where I would want to handle only a very specific subset.  Being curious, I
decided to experiment with a few GHC type system extensions to see if I could
create a `[Instruction a]` somehow.  That brought me to `-XImpredicativeTypes`
([`-XImpredicativeTypes`](http://www.haskell.org/haskellwiki/Impredicative_types)).
This is a pretty heavy extension that essentially allows you to put an
existential quantifier anywhere (even inside of a data type -- like `[]`).
This worked, after a fashion, but ended up breaking type inference pretty
badly.  Not quite the solution I was looking for.

After that I got slightly desperate.  I decided to try the ultimate hammer:
[unsafeCoerce](http://www.haskell.org/ghc/docs/latest/html/libraries/ghc-prim-0.3.0.0/GHC-Prim.html#v:unsafeCoerce-35-).
As the name implies, it is very unsafe, but it allows you to convert any
Haskell type to any other Haskell type.  In general, that makes no sense.
However, there is a note in the documentation for `unsafeCoerce` that states
that conversions between types with "the same representation" are safe (in
GHC).  The documentation explicitly mentions phantom types, but GADTs do not
seem that different (they just introduce new constraints whenever pattern
matched on).  This was the intuition behind wanting a `[Instruction a]` in the
first place.  I do not know if the representations of GADT values with
different return types are guaranteed to be the same, but constructing my
list with `unsafeCoerce` actually worked and ran without crashing.  So it
works but feels kind of wrong.

If that had been the only hangup, I think I would have bitten the bullet and
converted my data type to use GADTs.  However, a few minor problems convinced
me that it probably was not worth the effort.  First, I ended up needing two
type tags, which just started making type signatures long and slightly
unwieldy.  Second, I was actually using record syntax to create fields that
were shared between all constructors (for example, a unique identifier field).
This ran afoul of the GADT restriction where record labels can be shared only
between constructors of the same return type.  That was no longer the case with
my preponderance of type-level tags.  This problem was not insurmountable, but
it was tedious to work around.  Finally, not being able to use record field
names as selectors was fairly annoying, though it would have been less useful
since I had to rename so many.  I did not finish the conversion so I do not
know how badly type inference would have been impacted, but I imagine I would
have had to write a few more local type signatures.

# Conclusion

Between this rather involved experiment and my experience with hoopl, I feel
like GADTs are very powerful and useful, but perhaps best confined to a single
module (or a reasonably self-contained API).  The extra safety available from
the type-level tags was useful, but having it exposed everywhere was
problematic for me this time.  I will certainly try it again, and hopefully
I will find more cases where GADTs help more than they hurt.
