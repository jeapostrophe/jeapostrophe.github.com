#lang scribble/manual
@(require (for-label racket/base
                     rackunit
                     racket/list)
          "post.rkt")

@title{LaTeX and Word Counts}
@categories["LaTeX" "Mathematics"]

A few years ago, a proposal came to a committee I was on to limit the
documents we evaluated based on their word count, rather than their
page length.

The rationale was that so-called "graphics" research would include
many pictures which would increase the page length, but not
necessarily the word count.

I was opposed to this proposal on technical grounds.

@(the-jump)

My rationale was that it is easy to manually count pages, but
difficult (and not worth it) to count words AND manual counting is the
only reliable method to discover the number of words.

This proposal was made by someone who uses Word (ugh) to prepare their
documents, so it was easy, in their mind, to find the word count of a
document and report it when the document was turned in.

I, however, like all True Computer Scientists use LaTeX (and Scribble)
to prepare my documents, via rendering to PostScript or PDF.

@section{Word Counts from PostScript}

You may think that you could just count the words directly from the
PostScript. This is not the case, however.

PostScript is a very low-level programming language with a basic set
of vector-graphic-like primitives for moving the pen and drawing
shapes, letters, etc. A sentence like "The quick brown fox jumped over
the lazy dog." could get warped into many drawing commands,
particularly when effects like sentence spacing, rivers,
justifications are taking into consideration during the compilation
process. Compilers to PostScript, like LaTeX, in fact do this, making
the PostScript practically un-usable for analysis.

PDF improves on PostScript in numerous ways for analysis
purposes (such as computing where a new page will occur in the code)
but it is not significant enough discover individual words in the
presence of advanced typography and styling.

But, is it really surprising that it is hard to learn higher-level
things about a program from its compiled form? What if we looked
directly at the LaTeX?

@section{Word Counts from LaTeX}

Unfortunately, LaTeX is a Lambda-complete language due to its advanced
macro system. By writing macros, you can add new output (words) to the
document and change the meaning of terms later in the document by
introducing new macros and/or changing the environment.

This means that you can't analyze pieces of LaTeX independent from
everything that came before them and that pretty much the only way to
analyze them is to run them, but because of the Lambda-completeness,
evaluation may not terminate.

I wanted to demonstrate this to those on the committee, so I wrote a
short example program that had a very difficult to discover word
count.

@section{The Collatz Conjecture}

The Collatz Conjecture, proposed in 1937, states that if you take any
natural number, n, and repeat on either n/2 if n is even or 3n+1 if n
is odd, then you will eventually reach 1.

Every natural number, n, has a Collatz sequence, or the sequence of
numbers visited starting from n. (The conjecture says that all such
sequences include 1.) For example, the sequence for 6 is 6, 3, 10, 5,
16, 8, 4, 2, 1.

The Collatz Conjecture has not been proved, nor has any
counter-example been discovered.

@section{The Collatz Sequence in LaTeX}

Below is a short LaTeX file that renders to the Collatz sequence of a
random integer between 0 and 1,000,000,000:

@filebox["collatz.tex"]{
@verbatim{
\documentclass{article}
\usepackage{fp}

\FPrandom \n
\FPmul \n \n {1000000000}
\FPround \n \n 0

\newcommand{\collatz}
{
 \FPclip \n \n
 \FPprint \n
 \FPifeq \n 1
 \else
\ifodd \FPprint \n
    \FPmul \n 3 \n
     \FPadd \n \n 1
     \collatz 
 \else
    \FPdiv \n \n 2
     \collatz 
 \fi
 \fi
}

\begin{document}
\collatz
\end{document}
}}

This 27 line program demonstrates the absurd power of LaTeX. When you
run it, sometimes you get pages and pages of numbers. Other times, the
number of words will be so small it fits on just a few lines.

@section{Word Counts from LaTeX (redux)}

The beauty of this program is that you can't tell how many words will
be in the output by looking at the program source or the output,
because the output is only visible in the PostScript in a way that
can't really be discovered automatically.

The only way to get the word count is either manually or, perhaps, a
hacked version of LaTeX that computer the word count as it went. But,
of course, there are ways around the hacked version like by inserting
negative space sequences:

@filebox["negspace.tex"]{
@verbatim{
\documentclass{article}
\begin{document}
The qui \hspace{+9.5px}brown\hspace{-39.5px}ck

The quick brown
\end{document}
}}

This document appears to have six words to the human eye, but I am
highly skeptical of any analysis that would decide that from the seven
word-like tokens in the source.

@section{The Resolution}

Naturally, this argument swayed the rest of the committee and we stuck
with page limits rather than switching to word limits.

Luckily, no one noticed the problem with the program. You see, it only
considers numbers between 10^0 and 10^9, which have all been verified
to not repeat. We'd have to go up past 10^18, but the LaTeX fp package
only handles up to the top of 10^17, so we could never get to a number
where there is doubt about termination. Of course, the program still
has a very unpredictable output, which is computationally challenging
to discover. Additionally, it is natural to imagine putting more work
into the fp package, such as reimplementing GMP in it to get arbitrary
length integers.

Thus we see that LaTeX will always dominate Word. (And, of course,
since Scribble allows embedding Racket directly, potentially
non-terminating documents like this are much easier to write.)
