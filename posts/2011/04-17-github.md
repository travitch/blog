---
title: Github
tags: programming
date: 2011-04-17
---

Today I finally decided to stop hosting my own git repositories and
just move to [GitHub](http://github.com "GitHub").  Hosting them on my
VPS was never particularly difficult or demanding, but I was never
happy with the web interface I had set up
([cgit](http://hjemli.net/git/cgit/ "cgit")).  cgit itself is great
and easy enough to use.  I had my web server just dispatch CGI
requests to it and everything was fine.  The only problem was that I
didn't put enough effort into making it fit with the rest of my site
visually.  I was also never quite sure about the stability of the
hosting setup that I had constructed.

GitHub solved both of these issues: it seems reasonably stable and
doesn't give me the option to customize the appearance of the
repositories.  This way I don't have to agonize over it.  I went to go
sign up and was shocked to see that `travitch`, was taken.  Annoyed, I
looked to see what this person was hosting.  Well, it turns out that I
had already signed up a few months ago and completely forgotten about
it.  That was pretty convenient, so I ran with it (after I remembered
the password).  In the process of migrating my repositories, I got rid
of a few that I didn't want to maintain anymore.  I'm just deleting me
scripts for the uzbl browser since I don't use it anymore.  The code
for my PLDI 2009 work is going away temporarily (tarballs are still
available on my research page); this code will be superseded by
something better soon.

My former git hosting solution was obtuse enough that it actually took
me about 10 minutes to figure out how to disable it.  I set it up
months ago and never documented how I was starting all of the
associated services.  Eventually, I just uninstalled the service and
my package manager figured out what I was doing, somehow.
