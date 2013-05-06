---
title: Unification with unification-fd
tags: haskell, llvm
date: 2012-03-29
---

I finally feel like a real programming languages person.  I just used
unification to solve a problem besides type checking a variant of the
lambda calculus.  I used the excellent
[unification-fd](http://hackage.haskell.org/package/unification-fd-0.7.0)
package; it is a bit light on documentation but the included tests
were enough for me to figure out how to use it.  I might post
something more detailed later on.

My problem arose due to the type system rewrite in LLVM 3.0.  Prior to
this, types were all uniqued: there was one instance of each
structurally equal type.  This is no longer the case.  Each `struct`
type is named and there can be multiple structs with the same
structural type.  That is fine and actually closer to what I always
wanted.  Unfortunately, sometimes different names are assigned to the
same type in different compilation units (the duplicates have the same
base name but a numeric suffix).  Unfortunately, types sharing a name
are not always the same -- some libraries actually do re-use type
names for different types in different compilation units.

Example of the first case:

~~~~~~~~
%struct.glp_long = type { i32, i32 }
%struct.glp_long.0 = type { i32, i32 }
~~~~~~~~

Example of the second case:

~~~~~~~~
%struct.csa = type { %struct.glp_prob.10*, %struct.glp_bfcp.8, ... }
%struct.csa.556 = type { i32*, double*, i32*, double* }
~~~~~~~~

The first type is expected to unify, while the second is not.  I used
this to pick a representative type for the first case and to treat
each type name used in the second case as opaque.  Oddly enough,
everything worked the first time I ran it.  I noticed that some types
I expected to unify did not (e.g., the `glp_prob` and `glp_tree` types
in [glpk](http://www.gnu.org/software/glpk/)).  It turns out that the
library (intentionally?) does not always include the correct headers
to define types.  In those cases, it uses the C preprocessor to just
define them to be

~~~~~~~~~{.c}
struct foo { double _opaque_foo[100]; };
~~~~~~~~~

This is a bit annoying, but I managed to work around it outside of the
unification framework.  I'm not sure why they don't just forward
declare types like normal libraries.
