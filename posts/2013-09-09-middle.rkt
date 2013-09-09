#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/unit
                     racket/match
                     racket/list
                     data/heap)
          "../post.rkt")

@title{SIQ: Detecting Cyclic Linked Lists}
@categories["Racket" "Stupid Interview Questions"]

In this post we will look at cyclic lists in Racket: how to generate
them and two ways to detect them.

@(the-jump)

First, we shall generate cyclic lists using @racket[N] @racket[cons]
cells. The basic idea is to make all the possible @racket[cons]es,
then pick a random place for the cycle to start and end. It is
actually easier to think of it as picking a place where the cycle ends
and then the place it points back to. 

One cute thing we have to deal with is that in Racket @racket[cons]es
are immutable, so it seems like we can't actually ever create any
cycles. Luckily, There is something called @racket[make-reader-graph]
that takes an immutable data-structure (like pairs) that contains
special @emph{placeholder} values that can be modified to create
cycles and then translates the whole thing into a single immutable
value with cycles. For instance,

@chunk[<mrg-ex>
       (let* ([ph (make-placeholder #f)]
              [x (cons 1 ph)])
         (placeholder-set! ph x)
         (make-reader-graph x))]

This code creates a cyclic list of ones.

Our code has five steps: make all the @racket[cons]es, wire the
list "forwards", pick the cycling nodes, rewire the cycle, and then
make the reader graph.

@chunk[<mcl>
       (define (make-cyclic-list N)
         (define cs
           (for/list ([i (in-range N)])
             (cons i (make-placeholder empty))))

         (for ([from (in-list cs)]
               [to (in-list (rest cs))])
           (placeholder-set! (cdr from) to))

         (match-define
          (list cycle-start cycle-end)
          (sort (build-list 2 (λ (i) (random N)))
                <))         

         (placeholder-set!
          (cdr (list-ref cs cycle-end))
          (cons +inf.0 (list-ref cs cycle-start)))

         (make-reader-graph (first cs)))]

This function is guaranteed to return a cycle, marked with the
@racket[+inf.0] value, with an arbitrarily long prefix of non-cyclic
list. Here are some example outputs, printed by Racket's
default (cycle detecting) printer:

@verbatim{
'(0 . #0=(1 2 +inf.0 . #0#))
#0='(0 1 2 3 4 5 6 7 +inf.0 . #0#)
'(0 1 2 3 4 5 6 . #0=(7 8 +inf.0 . #0#))
'(0 1 2 3 . #0=(4 5 6 7 8 9 +inf.0 . #0#))
}

Next, we'll be writing functions that detect cycles, so we first need
a testing system. We'll just use variously sized cyclic and non-cyclic
lists and make sure the right answer comes out. (@racket[build-list]
creates a non-cyclic list of length @racket[N].)

@chunk[<test>     
       (define (test-has-a-cycle? N has-a-cycle?)
         (for ([i (in-range N)])
           (check-false (has-a-cycle? (build-list N add1)))
           (check-true (has-a-cycle? (make-cyclic-list N)))))]

I think the most obvious way to detect a cycle is to "mark" the
@racket[cons]es as you see them and return @racket[#t] if you ever get
a marked @racket[cons]. It's not obvious how to "mark" a @racket[cons]
in Racket though, because we don't have any space to use. This sort of
thing is one of the best uses for a @racket[hasheq]-table. Since the
table is indexed by @racket[eq?]-ness, it is like adding
another "field" to every object in the Racket program.

The code practically writes itself: if the list is @racket[empty],
then return @racket[#f]; if the @racket[cons] is marked, return
@racket[#t]; otherwise, mark and move on.

@chunk[<marking>
       (define (marking-cycle? l)
         (let loop ([marks (hasheq)] [l l])
           (cond
             [(empty? l)
              #f]
             [(hash-has-key? marks l)
              #t]
             [else
              (loop (hash-set marks l #t)
                    (cdr l))])))]

Unfortunately, this version uses a lot of space by storing the mark
table. There's another algorithm we could use that doesn't mark at
all, by Robert W. Floyd called
@link["http://en.wikipedia.org/wiki/Cycle_detection#Tortoise_and_hare"]{the
tortoise and the hare algorithm}.

It uses two pointers where one points at the @racket[i]th element and
the other points at the @racket[2i]th element. No matter when the
cycle starts or how long it is, it must be the case that a duplicate
will appear between these two elements for some @racket[i]. The only
downside is that the cycle may be traversed many times until a
multiple of the cycle length is discovered.

The only tricky thing about implementing this is that it requires lots
of @racket[empty?] tests to make sure you don't run off the end of the
list. If you ever do, then you know it isn't a cycle. In my version, I
define a helper that guards calls to @racket[cdr] and will return
@racket[#f] from the top-level if it ever sees one. It does this by
using an escape continuation, which is like an exception handler: when
you call it, it rolls back the stack and returns the value.

@chunk[<non-marking>
       (define (non-marking-cycle? l)
         (let/ec esc
           (define (cdr* l)
             (if (empty? l)
               (esc #f)
               (cdr l)))           
           (let loop ([tortoise (cdr* l)]
                      [hare (cdr* (cdr* l))])
             (unless (eq? tortoise hare)
               (loop (cdr* tortoise)
                     (cdr* (cdr* hare)))))
           #t))]

And that's it!

@section{Yo! It's almost time to go!}

But first let's remember what we learned today!

You can create immutable cycles in Racket with
@racket[make-reader-graph].

You should create a test suite before you write your program.

A @racket[hasheq] provides a way to add an additional "field" on every
object.

It is better to spent ten minutes reading Wikipedia than thirty
minutes trying to make your own clever algorithm.

If you'd like to run this exact code at home, you should put it in
this order:

@chunk[<*>
       (require racket/match
                racket/list
                rackunit)

       <mrg-ex>

       (define N 10)

       <mcl>
       (make-cyclic-list N)
       
       <test>

       <marking>
       (test-has-a-cycle? N marking-cycle?)

       <non-marking>
       (test-has-a-cycle? N non-marking-cycle?)]

@(the-end)
