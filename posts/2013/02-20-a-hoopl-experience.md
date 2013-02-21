---
title: A Hoopl Experience
tags: haskell, llvm
date: 2013-02-20
---

# Introduction

I have read about Generalized Algebraic Data Types (GADTs) before, at
least
[as implemented in GHC](http://www.haskell.org/ghc/docs/latest/html/users_guide/data-type-extensions.html#gadt).
The standard type-safe expression evaluator was interesting, but it
never left much of an impression on me.  Last week, I ran into them in
real code for the first time while I was playing with
[hoopl](http://hackage.haskell.org/package/hoopl), a library for
representing control-flow graphs and performing dataflow analysis and
graph rewriting.  The use of GADTs in the hoopl code was enlightening
and now I think I have a reasonable feel for the power of GADTs; hoopl
uses them in concert with
[type families](http://www.haskell.org/ghc/docs/latest/html/users_guide/type-families.html)
in ways I am still digesting.

Hoopl uses GADTs to indicate the shape of objects (graphs, blocks, and
instructions) in a CFG: objects are either "open" (O) or "closed" (C)
on entry and exit.  Control flow can "fall into" nodes that are open
on entry and "fall through" nodes that are open on exit.  For example,
a typical addition instruction in a compiler IR is open on both entry
and exit, and so would have a type like `Insn O O`.  On the other
hand, block termination instructions, which cause control flow to jump
to another block, are _closed_ on exit because control flow does not
just linearly fall through them, so a branch instruction would have
type `Insn O C`.  Hoopl treats instructions that are jumped to as
being of type `Insn C O`; it seems conventional for (possibly virtual)
label instructions to serve this role.  Hoopl does not allow
instructions to have type `Insn C C`.  Note that basic blocks are also
either open or closed on entry and exit.

# A New CFG in Hoopl

I was building a control-flow graph for the LLVM IR, so I mirrored it
as closely as I could.  I mapped each BasicBlock name to a Label (type
`Insn C O`) and treated that as the first instruction in each block.
Every LLVM block ends in a terminator instruction (type `Insn O C`).
All other instructions then had type `Insn O O`.  Since the label is
closed on entry and the terminator is closed on exit, the basic block
that they form is of type `Block C C`: closed on both entry and exit.
Combining all of the basic blocks in a function then produces a `Graph
C C`.  In an alternative design, the first block of a control-flow
graph could be of type `Block O C`, making the graph `Graph O C`,
meaning it has a unique entry point.  More on that later.

Now having fancy types on your control-flow graph is interesting, but
is it useful?  When treating the control-flow graph as an opaque type,
I do not know yet.  The answer might even be no.  On the other hand,
the first thing I did with this control-flow graph was to write my own
dataflow analysis framework (admittedly based heavily on the one
provided by hoopl).  While hoopl provides its own dataflow analysis
framework, I had a few changes I wanted to make:

 1) I wanted monadic transfer functions.  While general side-effects
    in a transfer function would be awkward and possibly dangerous, I
    like to provide Reader environments to some analyses to thread
    immutable data through transfer functions.  I also like to provide
    a writer context to log diagnostics purely, instead of just
    dumping them to stderr.  With careful management, a cache in a
    State monad can sometime be very useful, too (perhaps avoiding
    repeated calls to a theorem prover).

 2) I removed the graph rewriting.  Even though half of the point of
    hoopl is integrated on-the-fly graph rewriting, I do not need that
    functionality (and the rest of my IR cannot yet support it).
    Removing it keeps things simple.

 3) I wanted a simpler API.  Despite the elegance of GADTs, I did not
    really want to have to deal with them in the public dataflow
    analysis API.  Nearly any use of a GADT requires client code to
    enable the extension, and doing so with catch-all pattern matches
    seems prone to triggering odd warnings.  Further, the additional
    safety in the public API would have been minimal for the LLVM IR.

# Suddenly GADTs

Even though one of my motivations was to hide GADTs from the user,
they proved very convenient in writing the dataflow analysis
framework.  The first step in using hoopl is to make your instruction
type an instance of the `NonLocal` class, which describes the
connectivity between blocks.

```haskell
class NonLocal thing where
  entryLabel :: thing C x -> Label
  successors :: thing e C -> [Label]
```

I made a GADT wrapper around my LLVM instructions, Insn, which had the
appropriate entry and exit type tags:

```haskell
data Insn e x where
  Lbl :: BasicBlock -> Label -> Insn C O
  Terminator :: Instruction -> [Label] -> Insn O C
  Normal :: Instruction -> Insn O O
  UniqueExitLabel :: Label -> Insn C O
  UniqueExit :: Insn O C
```

While most instructions have normal fallthrough semantics (tagged `O O`),
the virtual labels and terminator instructions have more
interesting types.  I use the last two constructors to provide a
unified exit node for each function, since LLVM does not have a unique
exit node.  The power of GADTs started to show while making `Insn` an
instance of `NonLocal`:

```haskell
instance NonLocal Insn where
  entryLabel (Lbl _ lbl) = lbl
  entryLabel (UniqueExitLabel lbl) = lbl
  successors (Terminator _ lbls) = lbls
  successors UniqueExit = []
```

Note that `entryLabel` only needs to handle the block entry
instructions (those with entry type `C`), and `successors` only needs
to handle the terminator instructions (those with exit type `C`).
Even compiling with `-Wall`, the compiler issues no warnings for this
code because it can prove that the pattern matching is exhaustive.
The type signatures for those two class methods restrict them to
accepting only values that are closed on entry and exit, respectively,
so only values with those type tags need to be handled.  The compiler
is able to infer this because the types of the `Insn` GADT
constructors introduce constraints that mean the only closed-on-entry
instructions are `Lbl` and `UniqueExitLabel`.  Likewise for the
terminator instructions.  Without GADTs, I would have had to either
implement dummy cases for the other constructors for each method, or
throw in a catchall case that called error or something similar.

While implementing the actual dataflow analysis code, GADTs made a few
more interesting appearances.  First, since my control-flow graphs are
all closed on entry and on exit, I did not have to handle cases that
would have been possible had I opted to allow control-flow graphs to
be open at the beginning.  I also did not have to write any cases
handling blocks that are open on entry.  A consequence of this
decision, though, is that my dataflow analysis must specify the entry
node into the control-flow graph; since no blocks are open on entry,
one cannot automatically be found.  On the other hand hoopl handles
graphs of all shapes and the documentation for the forward dataflow
analysis function has a note: "if the graph being analyzed is open at
the entry, there must be no other entry point, or all goes horribly
wrong...".  Perhaps being explicit here is better overall.  The note
indicates to me that the type system cannot guarantee (as things
stand) that there is only one open-on-entry block in the graph.
Although my library code has to be explicit in providing an entry
point, users of my library do not have to deal with this - each
control-flow graph tracks its own entry point and automatically
provides it to the dataflow framework.

The dataflow analysis implementation in hoopl uses GADTs everywhere,
but one other style of use struck me as particularly elegant, so I
think that I will mention it here.  It also took me quite a while to
understand.  I think it is described in the hoopl paper, but the
detail was lost on me until I was implementing it myself.  In hoopl,
transfer functions in the dataflow analysis return type `Fact x f`.
Here, type `x` represents the shape of the instruction on exit and `f`
is the type of the dataflow fact.  The definition of `Fact` is:

```haskell
type family Fact x f :: *
type instance Fact O f = f
type instance Fact C f = FactBase f
```

and the type of the transfer function is:

```haskell
transfer :: forall e x . n e x -> f -> Fact x f
```

This means that instructions that are open on exit return a single
dataflow fact, while instructions that are closed on exit (i.e.,
terminator instructions) return a `FactBase`.  A `FactBase` maps
*labels* to dataflow facts, so the transfer function applied to a
terminator instruction can propagate different information along each
outgoing edge.  I thought this was extremely elegant.  Unfortunately,
exposing the GADT API to dataflow clients would have been inconvenient
for me because my instructions do not have natural shape tags, so I
would have had to expose my GADT wrappers.  I decided that keeping the
implementation hidden was more important than mirroring this API
exactly, so I simply have a separate edge transfer function that
clients can optionally provide.  The real surprise to me, though, was
the interplay between type families and GADTs: transfer functions can
return different types *depending on their inputs* because the shape
tag selects the type family instance being used.  I think this might
only work as well as it does because the entire dataflow analysis
framework in hoopl is designed as a series of composed *fact
transformers*; otherwise, the heterogeneous return types probably
would not be very useful.  I suspect this is a useful pattern to
keep in mind.

# Impressions

Overall, I am very happy with my
[new control-flow graph](https://github.com/travitch/llvm-analysis/blob/master/src/LLVM/Analysis/CFG/Internal.hs)
and hoopl.  I also have a new-found respect for GADTs, now that I know
what they are and have some idea of how to use them.  The code for
hoopl is like a giant type-based tapestry and the 16 page Haskell '10
paper barely does it justice.

# Aside

As an aside, I had read a paper a while ago about an
[Applicative control-flow graph that used zippers](http://www.cs.tufts.edu/~nr/pubs/zipcfg-abstract.html)
I always thought it was a very cool idea, but I never implemented it
myself because I have not needed that level of sophistication yet (in
particular, I do not really need to be able to rewrite control-flow
graphs).  When I started reading the
[paper on hoopl](http://www.cs.tufts.edu/~nr/pubs/dfopt-abstract.html),
I thought the idea sounded familiar; I didn't realize that the first
two authors were the same on both papers until much later.
