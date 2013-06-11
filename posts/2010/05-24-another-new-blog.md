---
title: Another New Blog
tags: haskell,python,programming
date: 2010-05-24
---

Why a blog?
===========

Due to popular demand, I am trying to move some of my ranting and
raving to more static and less visible media.  At least here they are
persistent beyond server logs.  This seems fairly handy, though, and I
think that I'll probably record some useful information here (if I
ever happen to find any).

Didn't we hear this before?
---------------------------

It is true.  In the not too distant past, I claimed that I would start
a blog.  I spent some time agonizing over choosing a platform, as you
might expect.  I have moral objections to PHP, so that ruled out the
vast majority of available platforms.

My ideal system would have been something written in Haskell, but
there didn't seem to be any relevant projects with documentation.  I
decided that settling for Python would be acceptable.  There were a
few almost acceptable solutions, so I chose one at random.  It seemed
to focus on providing a rich web interface for composing entries,
which is basically the last thing I wanted.  Thus, I never wrote any
entries but still had a blog.  The best of both worlds for everyone,
no?

So what changed?
----------------

Tired of withholding my rambling from the world, I decided to create a
blog in Haskell that would be acceptable to me.  I only have one
requirement worth mentioning: I want to be able to compose entries
using Markdown or something similar.  At the moment, there seem to be
two active Haskell web frameworks:

  * [Yesod](http://docs.yesodweb.com/yesod/ "Yesod")
  * [Snap](http://snapframework.com/ "Snap")

Both are fully capable of producing fine results.  I tried Yesod first
because Snap hadn't been released yet.

### Yesod

This framework was a bit fancier than I would like.  It uses
quasi-quoting to embed fancy domain-specific languages in Haskell.
I'm not a huge fan of source extensions like that, but I can see the
value sometimes.  I wasn't particularly convinced that this was a good
use case.  Yesod uses Hamlet as its template engine; this seems to be
a re-implemented and extended version of Haml.  I am not a huge fan of
that markup language, either.  The biggest problem, though, was a bit
less subjective: my VPS doesn't have enough RAM to compile some parts
of Yesod.

### Snap

Snap is a new framework and is still very minimal.  I can appreciate
that and can tolerate their templating engine (heist).  I hooked it up
to [pandoc](http://johnmacfarlane.net/pandoc/) to render my markdown
entries and am as happy with a piece of software as I ever seem to be.
I store the entries themselves in normal files, so tracking them via
version control is pleasant and simple.  Not everything is implemented
yet, of course, but getting this far should at least be enough to
motivate me to continue.  The short TODO list is something like:

 * Add support for viewing individual entries
 * Add per-month archives
 * Allow filtering by tag
 * Add support for comments (maybe)

