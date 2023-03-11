+++
title = "Releasing haggle-0.2"
date = 2022-05-08
[taxonomies]
tags = ["haggle", "haskell", "data-structures"]
+++

I just pushed a new release of [haggle](https://hackage.haskell.org/package/haggle-0.2) to Hackage today. The haggle library implements a number of different representations of graph data structures with different capability and efficiency tradeoffs. It is inspired by both [fgl](https://hackage.haskell.org/package/fgl) and [boost::graph](https://www.boost.org/doc/libs/1_79_0/libs/graph/doc/index.html), with the goal of enabling users to choose the right graph representation for their needs, while avoiding the partial functions that are used in fgl.

This release largely adds some instances contributed by users (including some `Unbox` instances).  The one small API change is to finally expose a function, `vertexId`, to extract an `Int` from a `Vertex`.  One of haggle's original goals was to keep the `Vertex` type opaque to ensure that users could not create invalid vertices in graphs.  However, it became apparent that some part of the structure would need to be exposed, at least in a read-only capacity:

- Interfacing with external libraries (e.g., to render graphs using [GraphViz](https://graphviz.org/))
- Implementing many graph algorithms efficiently usually requires an index of vertices that benefits from a raw `Int` representation

While I had haggle paged in, I also took the opportunity to clean up some technical debt, fix compilation with some older versions of GHC, and migrate to Github Actions for CI (from Travis CI).  I used the excellent [haskell-ci](https://github.com/haskell-CI/haskell-ci) tool to auto-generate the CI configuration, which worked on the first try.  It was a great experience, so I expect to use it going forward on all of my simpler packages.

