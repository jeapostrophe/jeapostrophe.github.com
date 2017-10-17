#lang scribble/lp
@(require (for-label racket/base
                     rackunit/chk
                     racket/list
                     racket/generator
                     racket/file)
          "../post.rkt")

@title{Creating Cyclic Data in Racket}
@categories["Racket" "Data Structures"]

Racket has the ability to construct immutable cyclic data, such as an
infinite list of @racket[1]s:

@chunk[<ex1>
       (shared ([ones (cons 1 ones)])
               (check-equal? (list-ref ones 1985) 1))]

In this post, I discuss how this feature could be implemented.

@(the-jump)

In our implementation of @racket[shared], called @racket[sharish], we
will attempt to use the same concepts as Racket, namely
@emph{placeholders}. The idea is that we first create a placeholder
for each identifier, then we fill it in (by calling
@racket[placeholder-extract!] with a function that sets the
identifier), then we close all the cycles in one go.

@chunk[<sharish>
       (define-syntax-rule (sharish ([x xe] ...) xb)
         (let ([x (empty-placeholder)] ...)
           (placeholder-fill! x xe) ...
           (placeholder-extract! x (λ (xv) (set! x xv))) ...
           xb))]

This allows cycles because the names are bound before any of the
@racket[xe]s are evaluated and they can mention the names wherever
they need to.

The one caveat with our implementation is that we need to use
mutation. @racket[shared] cheats by calling @racket[make-reader-graph]
which is implemented in the virtual machine, so it can mutate
data-structures that aren't normally mutable at the Racket level.

Here's an extensive test suite:

@chunk[<ex2>       
       (struct posn (x y) #:mutable #:transparent)
       (sharish ([a (mcons  1 x)]
                 [b (mcons #t x)]
                 [c (mcons  a x)]
                 [d (vector a b c d)]
                 [e (box d)]
                 [f (posn d e)]
                 [x (mlist a b c d e f x)])

                (chk #:t (eq? 1 (mcar a))
                     #:t (eq? x (mcdr a))
                     #:t (eq? #t (mcar b))
                     #:t (eq? x (mcdr b))
                     #:t (eq? a (mcar c))
                     #:t (eq? x (mcdr c))
                     #:t (eq? a (vector-ref d 0))
                     #:t (eq? b (vector-ref d 1))
                     #:t (eq? c (vector-ref d 2))
                     #:t (eq? d (vector-ref d 3))
                     #:t (eq? d (unbox e))
                     #:t (eq? d (posn-x f))
                     #:t (eq? e (posn-y f))
                     #:t (eq? a (mlist-ref x 0))
                     #:t (eq? b (mlist-ref x 1))
                     #:t (eq? c (mlist-ref x 2))
                     #:t (eq? d (mlist-ref x 3))
                     #:t (eq? e (mlist-ref x 4))
                     #:t (eq? f (mlist-ref x 5))
                     #:t (eq? x (mlist-ref x 6))))]

The implementation revolves around the placeholder data-structure. It
contains a flag to note if the extraction is complete, a queue of
data-structures to fill in with its value (more on this below), and
its value.

@chunk[<ph-def>
       (struct placeholder (extracted? fillers value)
               #:mutable)
       (define (empty-placeholder)
         (placeholder #f null undefined))
       (define (placeholder-fill! ph v)
         (when (placeholder-extracted? ph)
           (error 
            'placeholder-fill!
            "Cannot fill after extraction: ~e" ph))
         (set-placeholder-value! ph v))]

All the real work happens in @racket[placeholder-extract!]. We'll
define it recursively by cases on the first position:

@chunk[<ph-e>
       (define (placeholder-extract! v fill!)
         (define loop placeholder-extract!)
         (match v
           <value-cases>
           <container-cases>
           <ph-case>
           <struct-case>))]

All the work happens on the placeholder case. The idea is that if the
placeholder is already extracted, then just look up the
value. Otherwise, we have to detect if we are a cyclic reference or
the initial reference. We can do this by seeing if there are
any "fillers". It is important to detect this, because otherwise we
will go into an infinite loop. After the value is available, we fill in
all the cyclic references and then record that we've already walked
it.

@chunk[<ph-case>
       [(? placeholder?)
        (cond
          [(placeholder-extracted? v)
           (fill! (placeholder-value v))]
          [else
           (define first-time? (null? (placeholder-fillers v)))
           (set-placeholder-fillers! 
            v
            (cons fill! (placeholder-fillers v)))
           (when first-time?
             (loop (placeholder-value v)
                   (λ (x)
                     (for ([fill! (in-list (placeholder-fillers v))])
                       (fill! x))
                     (set-placeholder-extracted?! v #t))))])]]

This makes all the other cases very simple. If we see an atomic value,
we just send it to the filler:

@chunk[<value-cases>
       [(or (== undefined) (? number?) (? boolean?) (? null?))
        (fill! v)]]

If we see a container, then we recur on each piece with a filler
appropriate to the piece:

@chunk[<container-cases>
       [(? vector?)
        (for ([i (in-naturals)]
              [e (in-vector v)])
          (loop e (λ (x) (vector-set! v i x))))
        (fill! v)]
       [(box bv)
        (loop bv (λ (x) (set-box! v x)))
        (fill! v)]
       [(mcons a d)
        (loop a (λ (x) (set-mcar! v x)))
        (loop d (λ (x) (set-mcdr! v x)))
        (fill! v)]]

Finally, if we reach a structure, we see if it has any reflective
information available. If it does, then it's just a variety of
container:

@chunk[<struct-case>
       [(app (λ (x) (call-with-values (λ () (struct-info x)) list))
             (list (? struct-type? st) #f))
        (define-values
          (name
           init-field-cnt auto-field-cnt
           accessor-proc mutator-proc
           immutable-k-list super-type skipped?)
          (struct-type-info st))
        (for ([i (in-range init-field-cnt)])
          (loop (accessor-proc v i)
                (λ (x)
                  (mutator-proc v i x))))
        (fill! v)]]

The main difference between this code and the real implementation is
that the real implementation can either (a) modify things that aren't
really mutable or (b) transform mutable versions of data-structures to
the immutable versions after they've been filled in.

This code also shows you cycles have to occur in "first-order"
positions. For instance, you couldn't call @racket[append] and expect
there to be sharing in the lists, because @racket[append] is a
function that operates on lists and not a constructor.

The great thing about this code is that it's under 100
lines... including the test suite.

@section{Yo! It's almost time to go!}

But first let's remember what we learned today!

Cyclic data in a strict language requires mutation somewhere.

Cyclic data is dangerous because you might go into an infinite loop
looking at it.

If you'd like to run this exact code at home, you should put it in
this order:

@chunk[<*>
       (require racket/mpair
                racket/match
                racket/shared
                rackunit/chk)

       (define undefined
         (letrec ([x x]) x))
       
       <ph-def>
       <ph-e>
       <ex1>
       <sharish>
       <ex2>]

Or just download the
@link["https://github.com/jeapostrophe/exp/blob/master/sharish.rkt"]{raw
version}.

@(the-end)
