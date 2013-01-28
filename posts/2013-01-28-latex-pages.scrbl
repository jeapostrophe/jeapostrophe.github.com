#lang scribble/manual
@(require (for-label racket/base
                     rackunit
                     racket/list)
          "../post.rkt")

@title{LaTeX, Page Counts, and Sub-Documents}
@categories["LaTeX" "Make"]

I was recently preparing a grant proposal and had an annoying problem:
the proposal had to be submitted as many separate PDFs and each had
different page length requirements. Clearly I could make one giant PDF
with LaTeX and then use something like @onscreen{Print to File} many
times with different page ranges, but that would just be horrible. In
addition, wouldn't it be annoying to keep track of how many pages I
had already written for each section?

@(the-jump)

In order to solve the first problem, I created a beautiful
@exec{Makefile} that would make each of the individual documents by
using
@link["http://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/"]{PDFtk} to
split them off of the master. The proper way to do that is:

@commandline{pdftk ${INPUT}.pdf cat ${FROM}-${TO} output ${OUTPUT}.pdf}

where @exec{${FROM}} or @exec{${TO}} could be dropped.

The @exec{Makefile} took care of this splendidly:

@filebox[@exec{Makefile}]{
@verbatim{
all: summ.pdf desc.pdf bio.pdf budget.pdf refs.pdf

summ.pdf: master.pdf
    pdftk $< cat 1 output $@"@"

desc.pdf: master.pdf
    pdftk $< cat 2-16 output $@"@"

bio.pdf: master.pdf
    pdftk $< cat 17-18 output $@"@"

budget.pdf: master.pdf
    pdftk $< cat 19 output $@"@"

refs.pdf: master.pdf
    pdftk $< cat 20-end output $@"@"

%.pdf: %.tex
    pdflatex $(basename $@"@")
    bibtex $(basename $@"@")
}}

But the annoying problem during development was... how to ensure that
there will actually always be 20+ pages, even when I'm in the middle
of writing? In particular, before the description has reached the 15
page mark?

The key was to use a little LaTeX macro to add pages until the page
count reached the correct number.

@filebox[@exec{master.tex}]{
@verbatim{
\usepackage{forloop}
\begin{document}
% 1
\setcounter{page}{1} \include{summ}
% 2 - 16
\setcounter{page}{1} \include{desc}
\newcounter{descpages}
\forloop{descpages}{\value{page}}{\value{page} < 16}{
 ~\newpage
}
% 17 - 18
\setcounter{page}{1} \include{bio}
\newcounter{biopages}
\forloop{biopages}{\value{page}}{\value{page} < 2}{
 ~\newpage
}
% 19
\setcounter{page}{1} \include{budget}
% 20 - end
\setcounter{page}{1} \bibliography{all}
\end{document}         
}}

Although it is a totally trivial and stupid program (that I can't
figure out how to abstract and make nicer), it is still wonderful that
LaTeX can do this sort of thing and make my work-flow so much simpler.

I love programs and being able to make them do what I need. I can't
imagine how sad life would be if you didn't have the ability to change
your computer when it wasn't behaving the way you wanted.

@(the-end)
