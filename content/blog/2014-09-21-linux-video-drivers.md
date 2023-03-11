+++
title = "Linux Video Drivers"
date = 2014-09-21
[taxonomies]
tags = ["linux"]
+++

Today I ended up reading one of many articles decrying the current state of OpenGL.  I don't remember which one.  I will be interested to see where OpenGL does from here given its competition.  The whole topic got me curious to see what version of OpenGL my video drivers actually supported, so of course I checked with `glxinfo`.  Much to my surprise, I was running the open source [nouveau](http://nouveau.freedesktop.org/wiki/) driver.  I never thought about it while I was installing Ubuntu, and I guess this was the default.

I have to say that I am very impressed by how far nouveau has come.  The last time I read about it, it barely worked.  I have been using it for a few weeks now and I had not noticed a single problem.  I even fired up Steam to test things out and a random sample of one game (Skulls of the Shogun) worked with no problems.  I have no reason right now to switch to the official closed-source driver, so I will run with nouveau until a problem pops up.  Good work, whoever has been working on it!
