---
title: Parsing in Python
tags: python, programming
date: 2010-06-10
---

I recently had to do some simple parsing in Python.  Aside: the Python
requirement isn't particularly principled (I am actually not a huge
fan), but I go where [matplotlib](http://matplotlib.sourceforge.net/)
is.  I don't think that I have seen a more useful plotting library or
program yet.  Having the full power of a programming language
available to munge data into an appropriate form is a major
improvement over ad-hoc data manipulation or selection primitives in
tools like gnuplot.  My only complaint at this point is that showing a
legend for a particular plot *outside* of the plot is more difficult
than it should be.

Anyway, manually writing a recursive-descent parser would have been
simple enough, but I can pretty safely say that I'll probably never do
*that* again.  I decided to take the opportunity to look around the
Python parsing library/generator landscape, with the constraint that I
did not want something too complicated since this is basically a
one-off parser and intrinsically a simple grammar.

# pyparsing

The first thing I found was
[pyparsing](http://pyparsing.wikispaces.com/).  It seems robust and
well thought out, but under-documented.  The intended documentation
seems to be some book by the author of the library.  That is a fine
idea, but also completely unappealing to me.

# pyPEG

Next up was [pyPEG](http://fdik.org/pyPEG/).  I had actually heard
good things about this parsing framework already, and it passed the
documentation test pretty handily.  The library is actually very
small, which is a good thing (there is just one file that you can drop
into your project trivially).

PEGs are Parsing Expression Grammars, and it looks substantially like
a basic top-down parsing strategy with one nice additional feature:
production rules can include the `?`,`+`, and `\*` operators from the
realm of regular expressions (hence Expression Grammar).  These
operators provide a very convenient way to express optional or
repeated parts of a grammar without having to introduce unnecessary
recursive rules.

This reminds me a great deal of the typical constructs provided by a
parser combinator library, though obviously more restricted.

## Evaluation

The library was pretty much just what I needed.  I can deal with slow
for this application since I only need to parse one thing.  The
grammar is very readable, though the AST that it gives you back is a
bit cumbersome: it is composed of nested tuples and lists.  It is not
exactly fun to pick through.

### The Good

 * Documentation
 * Small, simple
 * PEGs are expressive

### The Not So Great

 * Pretty slow
 * No semantic actions


## Notes

pyPEG cuts down on boilerplate by relying heavily on convention.  This
results in very readable grammars at the source level at the expense
of flexibility.  The lack of semantic actions is probably related to
this choice.  A simple example demonstrates all of the features that I
found:

~~~~~~ {.python}
MANY = -1
MANY1 = -2

def _whitespace(): return MANY, [" ", "\t"]
def _number(): return _whitespace, re.compile(r'\d+'), _whitespace
def sample(): return MANY1, _number, '\n'
~~~~~~

Starting from the top:

 * `MANY` and `MANY1` are equivalent to the `\*` and `+` regular
   expression operators, respectively.  They are represented in pyPEG
   as numbers, so I alias them for readability.
 * Productions whose names begin with an underscore do not appear in
   the AST.  The documentation refers to this as suppressing the
   output of the rule name.  This limited form of control over the AST
   is about all that is available.
 * Productions are specified as *tuples*.  The regular operators apply
   to the parser that they precede.
 * Python lists denote choice.  The first production rule says that
   whitespace is any (possibly empty) sequence of spaces or tabs.
 * Regular expressions can be used to capture values.  The `_number`
   production rule returns a sequence of digits as the value of its
   parse.
 * The final production rule actually yields an AST of the form
   ('sample', [n1, ...]).

The AST is a bit annoying to work with.  Non-terminals that are not
suppressed with a leading underscore are tagged by the name of the
production rule that generated them.  What happens when you want to
generate sub-parsers using higher order functions?  Consider

~~~~~~~{.python}
def pairParserGen(name, regex):
    def pairParser(): return name, "=", "\"", re.compile(regex), "\""
    return pairParser
~~~~~~~

A function like this would generate production rules to parse
key-value pairs with a given `name` as the key and a value that
matches the provided `regex`.  pyPEG uses the name of the function
that represents the production rule it is applying when generating the
tag element of a tuple in the AST.  With this higher order rule
generation, all of the pairs parsed would share the same tag in the
AST: 'pairParser'.  That makes actually using the AST even more
challenging.  The fix is actually straightforward:

~~~~~~~{.python}
def pairParserGen(name, regex):
    def pairParser(): return name, "=", "\"", re.compile(regex), "\""
    pairParser.__name__ = name
    return pairParser
~~~~~~~

# Conclusion

I don't think I actually have a conclusion.  pyPEG is pretty nice,
though.
