---
title: PLDI 2013 Trip Report
tags: conference
date: 2013-12-05
---

As you can see, this post is late.  Very late.  PLDI was in June.  At
least I posted this in the right year.  I had some trip notes but never got around to turning them into a post.

I was presenting a
[paper](http://pages.cs.wisc.edu/~travitch/ismm-2013/) about a static
analysis to infer memory ownership properties for C libraries at ISMM
this year, co-located with PLDI.  Overall, I enjoyed the conference.
Presenting is always fun, as is putting faces to all of the names you
read about.  I just want to cover a few papers that stood out for me
(in roughly chronological order):

One of my favorite papers was presented early: [Scalable Variable and
Data Type Detection in a Binary Rewriter](http://dl.acm.org/citation.cfm?id=2462165).  This work takes
(stripped) x86 binaries and converts them into the LLVM IR with
reasonable precision.  As the title hints, they recover many complex
types, which is both impressive and necessary for many tools.  My own
work is on the LLVM IR and having a tool like this would be very
useful to let me apply my work to a wider range of inputs.  Not having
perfect accuracy would probably be fine for many of my purposes, so
this was an especially exciting piece of work for me.

Being mostly a software person, I learned a bit about different types of hardware, specifically [memory that deteriorates](http://dl.acm.org/citation.cfm?doid=2462156.2462171).

On the last day of PLDI, I was actually torn between the two
post-lunch sessions.  I actually wanted to see all of the
presentations from both sessions (one session was about Monads and
FRP, while the other was about alias analysis).  Being a mostly
functional programmer, I decided to go to the Monads and FRP session.
I couldn't digest the whole thing during the presentation, but [Monadic
Abstract Interpreters](http://dl.acm.org/citation.cfm?id=2491979)
seemed very elegant.  I'd like to spend some more time on that paper
soon, actually.  That said, I was more immediately excited about
Asynchronous Functional Reactive Programming for GUIs, otherwise known
as [Elm](http://elm-lang.org/).  I've been vaguely familiar with FRP
for a while, but this was a good presentation introducing it and
actually demonstrating how to use it.  I find that most of the
discussions of FRP assume you are already an expert, rendering them
somewhat unapproachable.  I'm not directly interested in Elm itself,
but I would very much like to try out an FRP library or two in
Haskell.  A side project I am working on now should afford me the
opportunity soon.

On the ISMM.  I am not really an expert on memory management, but I am
interested in the topic, obviously.  I found myself nodding along with
the keynote, which basically argued that sequential consistency (i.e.,
what most programmers who are not computer architects or compiler
writers expect) does not need to be prohibitively expensive.  Maybe we
will actually see sequential consistency in the wild one day.

My favorite paper in ISMM (besides mine, of course) was Control Theory
for Principled Heap Sizing.  I know nothing about control theory and I
have never implemented a garbage collector myself (though, like
everyone, I have all sorts of opinions about what makes a good garbage
collector).  I am not really qualified to evaluate the approach or
results in the paper, but I really liked the connection between a
well-known problem in Computer Science and a field of mathematics that
I have never really seen before.  It makes a lot of sense to look to
other fields for inspiration, especially since the programming
languages and compiler communities do not often focus on this type of
modeling.

