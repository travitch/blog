---
title: (Programming) Language Interoperability
date: 2013-12-27
tags: programming
---

My advisor recently pointed me to
[an article](http://queue.acm.org/detail.cfm?id=2543971) in the ACM
Queue about a familiar topic: interoperability between programming
languages.  My research for my thesis was very much in this area, and
I am happy to see others thinking about the topic.  I have not been
able to find a whole lot of related work.  Broadly speaking, I agree
with the sentiment of the article and differ on a few minor points.
For example, I don't think that mutability is a significant barrier to
language interoperability.  Certainly, I would like to see immutable
data become the norm in every language, but that is almost an
orthogonal concern.  Even in Haskell, mutable data is easily handled
within the IO monad.  Existing Haskell language interop tools can deal
with this sensibly.  Expecting every library in every language to be
designed with purely functional languages in mind is unrealistic and
not productive.  In fact, sometimes mutation is the right tool for the
job, and in some of those cases, a Haskell programmer might *want* to
drop down into a more low-level language (e.g., rust) to more easily
handle their mutable data.  Of course, having mutability highlighted
in the type system of a language interoperability solution would be
highly desirable.

Instead of focusing on the article itself, I want to spend some time
musing about a point raised in some of the comments: using C as a
least-common denominator for gluing languages together.  At the
moment, this is certainly the best option for language
interoperability.  However, I think a distinction should be made in
this context between using C the language and C the calling
convention.  The value of C in this domain is not related to the
language itself.  In fact, as the article and some of the comments
point out, it is in some ways a difficult language to interoperate
with due to its memory management, simplistic data model, and the
ad-hoc nature of its error handling.  That said, the underlying C
calling convention is, if not simple, consistent for each platform.
Nearly every language knows how to issue calls with the C calling
convention.  Libraries like libffi make this easy to provide for any
language implementation.

Granted, the C calling convention is not very expressive.  It is
sufficient if you only need to pass pointers and machine-sized data
types across language boundaries.  If you require deeper integration,
and in particular data interoperability, the C calling convention
falls short for most languages.  This is not insurmountable.  Only
agreement stands in the way of adopting more expressive calling
convention.  Even better if this common calling convention is a proper
superset of the C calling convention.  If the details of the calling
convention were abstracted behind a library interface that various
language implementations could use, it could even be evolved in a
backwards compatible way to improve performance over time as more work
is done in the area.  Perhaps, as we have the "C" calling convention,
this could be the "X" calling convention.

Ahead-of-time compiled languages may need to explicitly export certain
functions under the "X" calling convention, much like a C++ function
can be declared `extern "C"`.  If called from within their native
language, the compiler could either use the `extern "X"` version of
the function directly or possibly generate both the externalized
version of the function and a "native" version of the function.
Just-in-time compiled languages with a compiler available at run-time,
like Java, could simply generate `extern "X"` variants of functions
on-demand.

A useful cross-language calling convention would also need to expose
some amount of introspection data so that callers can determine what
functions the library provides.  This is one area where Microsoft's
COM works well from the caller's perspective.  A language
implementation aware of the "X" calling convention could automatically
generate this information.  A dynamic language run-time supporting the
calling convention could allow the introspection data to be queried at
run-time.  Static languages would require a separate tool to load the
(versioned) introspection data so that static type-safe call stubs
could be created.  This is roughly the client experience provided by
gobject-introspection.  In that case, however, the library interface
descriptions are largely hand-generated.  Keeping the compiler in the
loop would simplify the process and reduce the number of errors.

I should note that it is unlikely that all features of every language
could be exposed through a common interface.  Languages have many
differences, and some differences between languages may be
incompatible.  That is fine.  Languages have differences, and we
attempt to use the language with the most helpful quirks for any
particular problem.  A language interoperability layer that simplifies
data transfer and exposes most functionality would still be a vast
improvement over the status quo.  We have this problem already with C
and C++.  A C++ function being made callable from C must obey a number
of restrictions.  This is still useful.  It would be an interesting
research question to figure out what subset of features is reasonable,
and how language-specific extension could be handled.

There are many practical considerations for an "X" calling convention.
Error handling is important for any library.  While many languages use
exceptions to report errors, others do not.  Worse, nearly no
implementations of exceptions are compatible.  Any "X" calling
convention would need to standardize error handling.  One simple way
would be to require an implementation generating an "X"-callable
function to wrap it in a catch-all handler and return information
about any exceptional conditions via a standard mechanism (e.g., an
extra return value).  Deeper integration is, of course, possible, but
a simple solution may be best.  It requires little of language
implementors and does not require significant performance sacrifices
for the common case of intra-language calls.

Another significant problem in language interoperability is resource
management.  Objects allocated by the run-time system of one language
(malloc in C, the JVM in Java) must be managed by that run-time
system.  The "X" calling convention could expose resource management
hooks.  For example, a Python program calling a Java library would
receive objects allocated by the JVM; the calling convention could
expose pointers to the appropriate functions to call when the JVM
object is no longer reachable from Python.  The object would be pinned
in the JVM until Python un-pinned it.  Then it could be garbage
collected as it would normally in a Java-only environment.

These are just a few assorted thoughts.  Clearly, there would be many
details to work out.  These ideas are complementary to my earlier
work.  Instead of performing a deep analysis to make idiomatic
bindings, the additional features of this hypothetical calling
convention allow enough information to be exposed to require a bit
less analysis.  Furthermore, the extra information in this
hypothetical calling convention would make it easier to issue
cross-language calls into languages besides C.  The C calling
convention is our current lingua franca: we can do better, and doing
better would make true polyglot programming simpler.
