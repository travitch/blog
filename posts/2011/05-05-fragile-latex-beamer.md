---
title: Fragile LaTeX Beamer Slides
tags: tex
date: 2011-05-05
---

I am currently working on the talk slides for my thesis proposal.  My
preferred presentation medium is LaTeX Beamer -- a documentclass for
generating slides.  It gives you access to the usual selection of
excellent LaTeX tools.  Many argue that most of what LaTeX gives you
is exactly what you do not want in a presentation:

 * Ease of content generation without worrying about formatting
 * Easy abuse of well-typeset math
 * Lots of bullet points

While it is true that you can use these tools to make the standard
Awful Beamer Presentation, you can also ignore those things and make
good presentations.  Currently, I do not actually have much text at
all (and zero bullet points); instead, I mostly have diagrams and
illustrations.  I could have made all of the diagrams in TikZ, but
that can be a little tedious.  I have been using Inkscape, which is
actually really good and a lot of fun.

That is not really the important point here, though.  I also have many
short source code listings in the presentation.  For papers, I usually
stick to the
[listings](http://en.wikibooks.org/wiki/LaTeX/Packages/Listings "LaTeX
Listings") package.  However, it is not very appealing for
presentations because it is very painful to get properly
syntax-highlighted code listings out of it.  Instead, I much prefer
[minted](http://tug.ctan.org/tex-archive/macros/latex/contrib/minted/
"Minted"), which uses the excellent [pygments](http://pygments.org/
"pygments") program to do the actual syntax highlighting.  Both of these
packages work very well, except for one small problem when combined with
Beamer: the output of both tools is _fragile_.  My understanding is
that this means that the content cannot be moved freely.  Beamer has
provisions to work around this, though.  A slide can optionally be
marked as fragile, which changes the method LaTeX uses to process it.

~~~~~~~~~~~~~{.latex}
\begin{frame}[fragile]
  %% Fragile content goes here
  \begin{minted}{haskell}
    ...
  \end{minted}
\end{frame}
~~~~~~~~~~~~~

This works and fragile content like code listings can be embedded as
one would expect.  However, there is a major restriction: fragile
slides cannot have overlays.  Overlays are the mechanism by which
Beamer allows content in a single logical slide to be hidden or
progressively revealed.  They are a major workhorse of a Beamer
presentation, so this restriction is unpleasant.  I only found the
solution yesterday in
[some presentation](http://faq.ktug.or.kr/wiki/uploads/beamer_guide.pdf
"Beamer Guide"), and this post is mostly to make sure I remember it.
The idea is to construct the fragile content _outside_ of a slide,
save it, and then use it in a slide as desired later.

~~~~~~~~~~~~~~{.latex}
\defverbatim[colored]\contentName{
  \begin{minted}{haskell}
    ...
  \end{minted}
}

\begin{frame}
  \contentName
\end{frame}
~~~~~~~~~~~~~~

Note that the slide is not marked as fragile _and_ overlays will work
as expected.  Anyway, I thought it was pretty useful and very
under-documented.  One warning: be very careful to put the end tag for
the fragile environment (listings, minted, etc) on its own line.  In
particular, the closing brace of the macro being defined to hold the
fragile content should come on a separate line from the end of the
environment.  The terrifying things LaTeX does to construct some of
these pieces of fragile content seems to depend on the end of the
environment being on its own line.  The error messages that it
produces when this is not the case are, as always in LaTeX, completely
useless.
