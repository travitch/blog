+++
title = "Emacs Startup Optimization"
date = 2016-09-12
[taxonomies]
tags = ["emacs"]
+++

Recently, I had the urge to optimize the startup time of my emacs
configuration.  I managed to reduce my startup time from three seconds
to under half a second.  It was a fun exercise even though I don't
really start emacs that often.  Even that minor reduction makes
starting a new emacs instance (e.g., from mutt or in a terminal) much
more pleasant.  I could use the server functionality in emacs (along
with `emacsclient`) to always connect to a single persistent instance,
but I like the isolation of separate processes.  I've had the server
crash before bringing down all of my sessions, which is fairly
annoying.  Most of the startup time savings came from systematically
employing lazy package loading through John Wiegley's excellent
[use-package](https://github.com/jwiegley/use-package).  Besides
making it easy to properly load packages lazily, use-package also
handles package installation and configuration.

`use-package` encourages a configuration style that isolates the
configuration for each package in a `use-package` form.  Moreover, the
configuration of each package is split into initialization steps taken
at emacs startup time (via the `:init` section) and configuration
steps executed after the package is loaded (via the `:config`
section).  Packages can also be loaded lazily, which significantly
improves startup time.  By default, `use-package` forms that bind keys or
are associated with a specific file type are loaded lazily.

After doing a bit of profiling with
[esup](https://github.com/jschaf/esup), which is an emacs startup
profiler, I found a few surprising initialization steps that were
taking up most of my startup time.  The most expensive parts are
difficult to optimize further: theme loading, font setting in GUI
mode, and initialization of the `package` library.  Aside from that,
`cc-mode` was one of the most expensive things to load; moving more of
the setup from `:init` to `:config` took care of that.

There was at least one interesting and unexpected consequence of my
`use-package` configuration that I did not initially appreciate.  I
decided to set

```lisp
(setq use-package-always-ensure t)
```

This option tells `use-package` to automatically download and install
packages that are not already installed.  This is convenient to
bootstrap a new emacs installation or to bring an existing one up to
date with the latest changes to your emacs configuration.  It has side
effect of breaking when the package being set up is not in one of the
emacs package repositories (i.e., local packages).  This came up while
setting up a major mode for
[netlogo](https://ccl.northwestern.edu/netlogo/).  It eventually
occurred to me that I could tell `use-package` to *not* try to install
this package by setting `:ensure nil` in the `use-package` form:

```lisp
(use-package netlogo-mode
  :ensure nil
  :load-path "~/.config/local/emacs.d/lisp"
  :mode ("\\.nls$\\|\\.nlogo$" . netlogo-mode))
```

In the process, I cleaned out old configurations I didn't need anymore and
enabled a few new packages including:

* [swiper](https://github.com/abo-abo/swiper) (for sophisticated finding and replacing)
* [avy](https://github.com/abo-abo/avy) (for quick navigation)
* [switch-window](https://github.com/abo-abo/avy) (for faster movement between windows)

It was fun to refresh my entire configuration.  I should probably do
that more often.


