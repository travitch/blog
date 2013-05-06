---
title: Presentation Software
tags: linux
date: 2012-04-08
---

Lately, I have been making my presentation slides with LaTeX beamer.
I use SVGs created with inkscape (and my handy wacom tablet) for most
diagrams and the [minted](https://code.google.com/p/minted/) package
(which eventually calls [pygments](http://pygments.org/)) for syntax
highlighted source code listings.  This combination works well and
produces very nice PDFs that I can present with evince.

Unfortunately, the compile time for a large presentation is excessive
and makes incremental updates a bit of a pain.  It is also wasteful
since I am not using many of the good things that TeX provides (I try
to minimize my reliance on bulleted lists and math in slides).  Many
of my slides are just SVG diagrams converted to PDFs.

I decided that I needed a bit of a change, so for a short presentation
I am giving soon I decided to use HTML + Javascript for my slides.  I
am not a huge fan of web technologies or doing much of anything in a
web browser, but if most of my slides are SVGs surrounded by a bit of
colored text, that is halfway to a web page anyway.  Looking around,
I saw a few options:

 * The classic [s5](http://meyerweb.com/eric/tools/s5/)
 * A relative newcomer, [prezi](http://prezi.com/)
 * The similarly flashy [impress.js](http://bartaz.github.com/impress.js)
 * [deck.js](http://imakewebthings.com/deck.js/)

I remember using s5 once in undergrad.  It works but isn't
particularly impressive.  prezi is really neat, but it requires using
some web-based editor and is based on flash, which make it obviously a
no-go.  impress.js produces very nice results.  It takes a different
approach from a slide-based presentation; your entire presentation
lives in a big poster and the viewport zooms around to different
sections, subject to various scaling and rotation transformations.
The effect is very slick, but distracts somewhat from the content.  I
don't think it is particularly suitable for any technical content.  It
would probably go over well for some kind of sales fluff-piece
presentation, though.  The additional effort of manually arranging
each slide in the plane of the poster is also a bit more than I would
want to deal with.

I decided to go with deck.js, which uses a more traditional
slide-based scheme.  At the end of the day, it really is a lot like
LaTeX beamer in your browser.  I threw in
[highlight.js](http://softwaremaniacs.org/soft/highlight/en/) to
highlight my source code snippets, and I can directly embed SVG
graphics without conversion.  I'm very happy with the results, and it
doesn't require any fancy LaTeX hackery to safely work with fragile
slides.  It even has a nice navigation overview that you can bring up
with the 'm' key to randomly jump between slides.  This setup has the
additional advantage of being able to use any HTML-friendly content,
including forms, javascript, iframes, and embedded videos without
having to switch away from the slide viewer.  Styling is also a bit
easier with CSS than beamer's lightly-documented internals.

I have to see how it performs at presentation time, but it seems to be
perfectly acceptable.  Now I have to try it with a more complex
presentation.
