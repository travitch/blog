+++
title = "Piping data to Emacs"
date = 2026-07-26
[taxonomies]
tags = ["emacs"]
+++

About once per month I find myself wanting to pipe some data into Emacs and being annoyed that I can't.  I built [Pipemacs](https://github.com/travitch/pipemacs) to let me do it.

Somewhat surprisingly, Emacs does not natively support piping data into it via standard input (e.g., as you might do from a shell).  The *Emacs Way* is to just run all commands via [Eshell](https://www.gnu.org/software/emacs/manual/html_node/eshell/).  I don't live in Emacs quite enough for that workflow to be natural.  My most common use case is just running normal commands but wanting to browse the output as it is produced (i.e., streaming) in something more capable than `less`.  I mostly used Vim for this, but was always annoyed that Emacs didn't make it easier.

I am not the first person to want this.  The closest tool I found was [mxp](https://github.com/agzam/mxp), which works by 1) starting a local TCP server in emacs, 2) populating a buffer with the data streamed to the TCP server, and 3) streaming data to it using the `/dev/tcp` Bash built-in.  This is flexible and works well, but mxp will only send data to a running Emacs instance using `emacsclient`.  I don't run Emacs in server mode, so that wasn't quite what I wanted: I run many separate Emacs instances and almost always want to stream command output to a new instance.  I thought about extending mxp to be able to spawn new instances, but wanted to try a slightly simpler approach, which turned into Pipemacs.

Pipemacs supports sending data to a new Emacs instance or an existing Emacs server using `emacsclient`.  It also uses Emacs support for TCP connections, but hosts the server in Pipemacs instead of Emacs (i.e., Emacs becomes the client instead).  This results in running less code (and simpler code) in Emacs.  It also avoids having to use string substitution to set the port in the elisp script that mxp uses.

While I was working on Pipemacs I wondered why mxp used TCP connections instead of something like a named pipe.  I don't know the author's full rationale, but it quickly became apparent that the TCP connection approach is at the very least easier.  Surprisingly, Emacs does not expose many functions for incrementally reading from (or writing to) files.  The best interface it provides for incremental I/O is through its process abstraction, where there is a special process type for TCP connections.  This interface lets you provide a callback (a *process filter*) to handle received input and gives you the `process-send-string` function to send data out of Emacs.

As I was writing Pipemacs it occurred to me that while sending data to Emacs over standard input was useful, I could also send the modified buffer contents back to the Pipemacs shim (`pm`) to be echoed back to standard output.  This lets the caller put Pipemacs into the middle of shell pipelines to either inspect or modify data:

```
foo | pm --writeback | bar
```

I'm not sure if this is very useful or a good idea yet, but it was too easy to implement to not just include it.

