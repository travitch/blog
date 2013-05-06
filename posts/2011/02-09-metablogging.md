---
title: Metablogging
tags: haskell
date: 2011-02-09
---

In lieu of writing about something useful, I'll spend some words on
this silly blog.  I seem to spend more time maintaining it than adding
content, so that seems fitting.

[Snap](http://snapframework.com "Snap") version 0.3 was released in
December.  Unfortunately, I was too busy at the time to update this
site to use it.  Perhaps that was fortunate, though, since version 0.4
was just released a few days ago.  In celebration, I took a little
time and updated everything to use 0.4.  Version 0.3 introduced the
feature that I had really been wanting for a while: a `MonadSnap`
typeclass.  Previously, `Snap` was just its own monad and every
request handler had to run in that monad.  This was workable, but
awkward at times.  In particular, it was difficult to build a custom
monad up around `Snap` and use it in a convenient way.  This required
manual state threading when I wanted to pass information through the
system.  Now I can easily throw together a custom monad using
`StateT`.

At some point (I believe the 0.4 release), the templating engine that
comes with Snap, Heist, switched its backend from hexpat to something
custom and more friendly to HTML templates.  This let me remove a few
ugly hacks from the templates to keep hexpat from collapsing DOM nodes
into forms that are not legal HTML.  I also do not have to hack in my
own doctype declarations anymore, which is a bit of a bonus.  The
Heist API also became more friendly to
[blaze-html](http://jaspervdj.be/blaze/ "blaze-html")
interoperability.  This is much more convenient than
manually-constructing XML using hexpat.  I do not manually generate
much HTML, but there are a few places where it can serve as handy glue
between components.  A few of my templates ended up breaking in minor
ways.  Eventually I figured out how to fix them, though, and
everything looks as it should now.  Heist has actually grown on me
quite a bit as a templating system since I first started using it.
The `bind`/`apply` primitives were not intuitive at first, but once I
got the hang of using them, this seems to be the right way to handle
templates.  I do not think that I have seen another templating system
allow for quite as much easy template re-use as this one.

There was also a change from using `ByteString` everywhere in Snap to
using `Text` in a few places.  I like seeing `Text` used more, and I
try to do the same in my own code.  Unfortunately, many Snap APIs
still use `ByteString`, so the two mix occasionally.  Presumably this
is done for speed, but having to convert from one to the other can be
a bit of a pain.  Maybe this will change one day.  I am eagerly
waiting for some real `Hashable` instances for `Text`, since I feel
like mine are hackish.  I also feel guilty about orphan instance
warnings (but not guilty enough to _not_ throw
`-fno-warn-orphan-instances` at the problem).

Fortunately, the filemanip package was updated to work with mtl-2 just
in time.  Well, not quite just in time -- I had rewritten the parts of
the code that relied on it about an hour before the update.  I will
survive, though.  I reverted those changes since I like the filemanip
API.

I also finally got around to adding a few of the features that I
promised myself that I would add.  Maybe these will get me more
interested in adding content:

 * Proper event logging (instead of writing to stdout)
 * Daemonization (instead of just running the server in screen)
 * A basic turing test for the comment box.  I do not really
   anticipate ever getting comments, but hopefully it will help cut
   down on the spam a bit.
