---
title: IPv6 Support
tags: linux
date: 2012-02-29
---

I enabled IPv6 on the machine hosting this blog a few months ago, but
I hadn't actually tested it.  In fact, I don't have any other hosts
with IPv6 support so I can't really test it myself.  Since
[World IPv6 Day](http://www.worldipv6day.org/) is coming soon, I decided it was
time to get things working.

Looking around, I found an
[IPv6 website validator](http://ipv6-test.com/validate.php), which
actually let me figure out if things were working out.  There were
just two problems:

 * My AAAA DNS record didn't transfer automatically when I switched from GoDaddy
 * My web server supported IPv6 but wasn't listening on any IPv6 interfaces

I guess I am ready for the future now.  Maybe I'll even be able to
test my IPv6 support personally one day.
