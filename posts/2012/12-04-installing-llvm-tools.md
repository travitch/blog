---
title: Installing llvm-tools
tags: haskell, programming, llvm
date: 2012-12-04
---

In my last post I neglected to provide installation instructions.
For most systems, it should be fairly straightforward:

 1) Ensure that `dot`, `llvm-config`, `ghc`, and `cabal` are in your
   `PATH`.  The first is provided by the
   [Haskell Platform](http://www.haskell.org/platform/).  The 2012
   releases should work.  Additionally, ensure that `~/.cabal/bin` is
   in your PATH, since the binaries will be installed there (and it
   may need to be in your path during the build process, too).

 2) Run the following script:

```bash
REPOSITORIES="hbgl-experimental
ifscs
itanium-abi
llvm-base-types
llvm-data-interop
llvm-analysis
llvm-tools"

mkdir llvm-haskell-packages
cd llvm-haskell-packages

# Download the repositories
for REPO in $REPOSITORIES
do
    git clone git://github.com/travitch/$REPO.git
done

# Add ./ prefixes to each repository (for cabal)
TOINSTALL=`echo ./$REPOSITORIES  | sed 's: : ./:g'`

# Build the tools along with dependencies
cabal install c2hs
cabal install $TOINSTALL
```

The installation will probably take some time since it will pull in
all of the necessary dependencies (graphviz often takes quite a
while).  I should note that this would be more convenient if I put
everything up on hackage, but I don't think it is stable enough for
that.
