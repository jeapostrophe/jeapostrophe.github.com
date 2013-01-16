#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/list)
          "../post.rkt")

@title{Professor Layton and the Diabolical Box, Puzzle #132}
@categories["Puzzles" "Racket"]

I try to solve a puzzle of some kind every morning. I use
@link["https://en.wikipedia.org/wiki/Sudoku"]{Sudoku},
@link["https://en.wikipedia.org/wiki/Picross"]{Picross}, and often,
@link["https://en.wikipedia.org/wiki/Professor_Layton"]{Professor
Layton}. Sometimes it is convenient to write a program to solve some
of the more annoying "search" puzzles. I'll post the Racket programs
with a little bit of commentary.

@(the-jump)

This is for @link["http://tinyurl.com/6pjbzrs"]{Puzzle 132}. Here is
the puzzle:

> Two brothers have inherited their parents' five-piece art
collection. According to the will, the older brother will get a set of
paintings worth twice what the younger brother gets. In order to
ascertain the value of the paintings, the brothers called in an
appraiser, who valued each painting as shown below. For his services,
the appraiser was promised the one painting left over after the
brothers divided the art according to their parents' wishes.

> Assuming that individual paintings can't be divided, which one does
the appraiser get?

There is then a picture of five paintings with prices underneath. They
are: A worth 20,000, B worth 60,000, C worth 55,000, D worth 45,000, E
worth 95,000.

I encoded this information into a vector in Racket:

@chunk[<paintings>
       (define paintings
         (vector 20 60 55 45 95))]

We won't keep track of the labels, we'll just remember that, for
example, 0 is A and 4 is E. Also, we divide everything by 1,000 so we
don't have to type so much.

The basic algorithm we'll use is a trivial search: try assigning each
painting to each brother and stop when the value of the older
brother's paintings is twice that of the younger.

The trick, however, is that we'll represent the assignment as the
older brother's set combined with the younger brother's set. We'll do
this simultaneously with a bit-vector, where the @litchar{1}s indicate
that the older brother gets it and the @litchar{0}s indicate that the
younger brother does. We'll independently pick one painting which will
be "left over" that the appraiser will get. Here's the main loop:

@chunk[<solver>
       (for* ([assignment (in-range (add1 #b11111))]
              [appraiser (in-range (vector-length paintings))])
         (define older-value
           (assignment->value assignment appraiser #t))
         (define younger-value
           (assignment->value assignment appraiser #f))
         (when (= older-value (* 2 younger-value))
           (return appraiser)))]

One thing to note here: @racket[for*] is like a nested
@racket[for]---we loop over the assignments /and/ loop over every
painting each round. (This, by the way, means that we do twice as much
work because we consider both assignments for the appraiser's
painting.)

Two other cute things: First, we use a literal binary number to write
down the completely full set, but we have to add one to actually visit
it. Second, the @racket[assignment->value] function (below) will take
an argument to determine whether to add up the @litchar{1}s or the
@litchar{0}s. Here's it's definition

@chunk[<valuation>
       (define (assignment->value assignment ignored which)
         (for/sum ([painting (in-range (vector-length paintings))]
                   #:unless (= painting ignored))
                  (if (eq? which (bitwise-bit-set? assignment painting))
                    (vector-ref paintings painting)
                    0)))]

The @racket[for/sum] variant adds up the result of each iteration of
the loop, the @racket[#:unless] clause skips the iteration where the
appraiser's painting is considered, and the @racket[if] determines
which brother we're considering.

If you know anything about Racket, there may be one more confusing
thing about the code in @racket[<valuation>]... @racket[return]!
Expressions in Racket don't normally have non-local returns like
that. How can we make the inner area of the loop stop and return the
appraiser painting that works? It's simple: bind @racket[return] to an
escape continuation:

@chunk[<escape-continuation>
       (let/ec return
         <solver>)]

Was this faster or slower than doing it the old fashion way...? Who
knows.

Can you work out what the answer is...?

By the way, if you use this code at home, make sure you put the code in this
order:

@chunk[<*>
       <paintings>

       <valuation>

       (vector-ref
        (vector 'A 'B 'C 'D 'E)
        <escape-continuation>)]

@(the-end)
