---
title: Blog Software Update
date: 2013-09-02
tags: haskell
---

This blog is built on [hakyll](http://jaspervdj.be/hakyll/), a static
site generator written in Haskell.  I have been running on Hakyll 3.x
for quite some time now, but Hakyll 4 was released in January.  I kept
telling myself that I would update - I even had the port mostly
working a few months ago.  I never got around to finishing it, though.
Until today, that is.  There should be no visible changes if I did
everything right, but the [code](https://github.com/travitch/blog) is
much cleaner.  It was essentially a rewrite, so it was a good
opportunity to make some style fixes.  I had to cheat and work off of
another
[example](https://github.com/nickcharlton/nickcharlton.net/blob/master/site.hs)
of Hakyll usage, but it all worked out in the end.

The biggest change from Hakyll 3 to Hakyll 4 was a transition from an
[arrow](http://www.haskell.org/arrows/)-based API to a monadic API.
Hakyll was my first exposure to arrows, and I use some small parts of
the arrow API in my own code.  I think the monadic API is a bit
simpler, but have no strong opinions.  It does require fewer odd
combinators in day-to-day use.  I like the concept of arrows just
fine, but I haven't used them enough to always remember what each
combinator does off-hand.  I think the biggest change that affected me
is that Hakyll 4.1 uses pandoc 1.10, which just makes package
maintenance easier (most other things are updating to the newer
pandoc, too).  There also seem to be some fancy new features in the
template system in Hakyll 4.3 that I am not yet using.  I might try
out the new pagination and teaser functionality.
