---
title: One More Blog
tags: haskell
date: 2012-02-16
---

Another year, another blog.  It seems that I just re-wrote my blog for
the third time.

# The Second Rewrite

I never deployed the second rewrite, which was mostly for fun and to
play with [yesod](http://www.yesodweb.com/).  Despite my reservations
about its use of Template Haskell and quasiquotes, I eventually came
around to the whole model suggested by the framework.  Type-safe URLs
are very pleasant and the automatic routing of requests to typed
handlers is a huge time and boilerplate saver.  After I got the hang
of the framework, development was very fast and I had pretty much
everything working as I wanted in two or three days of occasional
effort.

The most annoying thing was that it required more full rebuilds than
other projects due to the Template Haskell code.  By default, routes
are specified in a plain text file that is read with Template Haskell
at compile time (which then produces routing code).  This is
convenient until you need to add or modify routes and yesod has no way
to know that you changed the routes file.  Working around this problem
is simple but annoying: do a full rebuild.  It looks like GHC 7.4.1
and the
[template-haskell-2.7.0.0](http://hackage.haskell.org/packages/archive/template-haskell/2.7.0.0/doc/html/Language-Haskell-TH-Syntax.html)
package have a new function `qAddDependentFile` that should be able to
fix this issue.

At the end of the rewrite, I was pretty happy with Yesod and I would
definitely use it for any significant web application that I happen to
need.  That said, the result was completely excessive for this blog.
The pages on this blog essentially never change and the only dynamic
content is the list of comments for each post.  Additionally, building
yesod with its dependencies on my VPS was alarming and always sends it
swapping.  It just wasn't very fun for this blog, which is supposed to
be fun.

# The Third Rewrite

Noting that my content was basically static and that the only way I
ever want to write a post is via a text editor and a git repository, I
decided to switch to a simple static site generator for the blog
proper and delegate comments to Disqus.  It turns out that there are
no simple static site generators, so I went with
[hakyll](http://jaspervdj.be/hakyll/index.html), which is really
awesome.  It uses pandoc (which I had used in all of my previous blog
incarnations) as its formatting engine, so all of my existing posts
were compatible.  My blog code is based around one of the examples
included with hakyll and clocks in at under 150 lines of code.  This
seems like a good number for something this simple.  To post, I just:

 1) Write a post in markdown
 2) Test it locally
 3) Commit it and push to my VPS

I use a post-receive hook (as described in
[this post by Chris Done](http://chrisdone.com/posts/2010-04-04-hakyll-and-git-for-you-blog.html))
to build and deploy new posts.

Hakyll is a great piece of software.  I would definitely use it again
and am very happy with the results.  It was apparently mostly
rewritten for the 3.x release series, and it now makes use of some
interesting types.  In particular, it is heavily based on
[arrows](http://www.haskell.org/arrows/).  Luckily, I had just been
looking at arrows lately so it wasn't as jarring as it otherwise might
have been.

While hakyll handles the content generation well, I still wanted to
allow comments (not that I get many of those).  I didn't even look
around very much and just went right for Disqus.  It seems to do the
job and integrating it with my generated site was very easy.  It only
requires a little Javascript snippet on each page that needs to
support comments, and a bit of extra magic to get comment counts on
the post listings.

## Design

I also took this opportunity to redo the design and make everything
much simpler.  The last design was also apparently visually offensive
to some.  I hope this is a bit easier on the eyes.  Maybe this is why
people don't pay me to make things look good.
