---
title: Wacom Tablets and RHEL5
tags: linux
date: 2011-05-10
---

I have been making my presentation slides in LaTeX Beamer for a few
years now and it is a pretty pleasant process for the various reasons
I mentioned in the previous post.  More recently I decided that I
needed to depend less heavily on the staple of a staple of Beamer
presentations: the bullet point.  You can generally spot a Beamer
presentation from a few miles away, and I did not really want to be
_that guy_ any more.  Without bullet points, though, what is really
left for presentation slides?  I was at a loss and had no idea what to
do.  The TikZ package is an amazing and terrifying library for
building diagrams in LaTeX, but I usually end up feeling slightly ill
at the prospect of making any non-trivial figures with it.

That leaves drawing.  I am reasonably sure that a pigeon holding a
stick in its beak could draw better than me.  That has not really
stopped me, though, and I decided that I would start drawing my
diagrams.  I don't have the motor skills required to use a mouse, so I
went and picked up the cheapest Wacom drawing tablet I could find:
some Bamboo model.  I just plugged it into my laptop running Debian
Sid and everything Just Worked.  It works amazingly well in Inkscape
and The Gimp, so I am covered from the software end.

Of course, my laptop runs recent versions of software.  At the office,
my workstation has to run Red Hat Enterprise Linux 5.  The kernel in
RHEL5 is several years old and hardware support is often lacking.  In
fact, it has no clue what to do with this drawing tablet.  My first
workaround was to just use my laptop and hook up an extra monitor.
That did work, but I do not really like leaving my laptop on and idle
for extended periods of time like that.  My next workaround has proved
sufficient: VMWare.  I just run an up-to-date Debian install in a
VMWare virtual machine and give it ownership of the drawing tablet.
That kernel recognizes it and everything works fine.  The performance
of the VM on my workstation is far from stellar but it is sufficient
for drawing.

Now I just have to see if my presentations improve at all.
