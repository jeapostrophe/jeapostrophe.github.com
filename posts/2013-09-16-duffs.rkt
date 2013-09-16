#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/unit
                     racket/match
                     racket/list
                     data/heap)
          "../post.rkt")

@title{Duff's Device in Racket}
@categories["Racket" "Insanity" "C"]

In this post, we look at implementing the famous "Duff's Device" in
Racket.

@(the-jump)

Duff's Device is an abuse of C's syntax to create a loop that you can
jump part-way through. The idea is that an operation @racket[body]
needs to be performed @racket[count] times. You perform the operation
@racket[K] times inside of a loop. If @racket[count] is divisible by
@racket[K], then you go through the loop @racket[(/ count K)]
times. If it is not, then you jump part-way through the loop and do it
@racket[(remainder count K)] times, then go through the loop for the
remaining times.

Here's the code in C where @racket[K] is @racket[8]:

@verbatim{
send(to, from, count)
register short *to, *from;
register count;
{
        register n = (count + 7) / 8;
        switch(count % 8) {
        case 0: do {    *to = *from++;
        case 7:         *to = *from++;
        case 6:         *to = *from++;
        case 5:         *to = *from++;
        case 4:         *to = *from++;
        case 3:         *to = *from++;
        case 2:         *to = *from++;
        case 1:         *to = *from++;
                } while(--n > 0);
        }
}          
}

This is a bit ugly, because you have to actually copy out the code the
appropriate number of times. It seems like the perfect thing for a
macro to do for you, but how can we get the weird interaction between
@racket[switch] and @racket[do-while]?

First, let's see the final program:

@chunk[<use>
       (define (send out count)
         (define from 0)
         (duff 8 count (out (post++ from))))

       (define-syntax-rule (post++ id)
         (begin0 id (set! id (add1 id))))]

We could vary the @racket[8] to be any positive natural.

Since the repeated operation doesn't return a value, we have to
observe it with an effect. Here's an example that just accumulates the
calls into a list:

@chunk[<test>
       (define (test-send count)
         (define l empty)
         (send (λ (e) (set! l (cons e l)))
               count)
         (reverse l))

       (check-equal? (test-send 1) 
                     (list 0))
       (check-equal? (test-send 4) 
                     (list 0 1 2 3))
       (check-equal? (test-send 8) 
                     (list 0 1 2 3 4 5 6 7))
       (check-equal? (test-send 16)
                     (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
       (check-equal? (test-send 9) 
                     (list 0 1 2 3 4 5 6 7 8))]

The implementation of @racket[duff] is where the meat is.

First, we need the @racket[switch] with the drop-through where the
operations are duplicated. We've already implemented that in the
@racket[cas-cad-e] macro from @postref["2013-06-24-cas-cad-"].

In the @racket[cas-cad-e], we have the cases in the order @racket[0]
to @racket[K] down to @racket[1]. We can generate that as the template
sequence @racket[(i ...)] with:

@chunk[<case-seq>
       (reverse
        (build-list (sub1 (syntax->datum #'k)) add1))]

We can put that together in the @racket[cas-cad-e] with:

@chunk[<cas-cad-e>
       (cas-cad-e d
        [(0) (op)]
        [(i) (op)] ...)]

This just goes inside of a loop that is like the C version, but the
main thing is that we need to explicitly put the loop outside of the
@racket[cas-cad-e] and let the fall-through handle the loop happening
in the last case.

@chunk[<loop>
       (let loop ([n (quotient (+ count (sub1 k)) k)]
                  [d (remainder count k)])
         <cas-cad-e>
         (when (> n 1)
           (loop (sub1 n) 0)))]

Finally, the macro just wraps it all up:

@chunk[<duff>
       (define-syntax duff
         (syntax-parser
          [(duff k:nat count:id body:expr)
           (with-syntax ([(i ...) <case-seq>])
             #'(let ([op (λ () body)])
                 <loop>))]))]

This would be like if the C version were:

@verbatim{
send(to, from, count)
register short *to, *from;
register count;
{
        register n = (count + 7) / 8;
        d = count % 8;
        do {
         switch(d) {
         case 0:     *to = *from++;
         case 7:     *to = *from++;
         case 6:     *to = *from++;
         case 5:     *to = *from++;
         case 4:     *to = *from++;
         case 3:     *to = *from++;
         case 2:     *to = *from++;
         case 1:     *to = *from++;
         }
         d = 0;
        } while(--n > 0);
}
}

Although not exactly, because setting @racket[d] to @racket[0] only
happens if the loop will be taken.

@section{Yo! It's almost time to go!}

But first let's remember what we learned today!

Macros automate program transformations, even crazy ones like Duff's
Device.

If you'd like to run this exact code at home, you should put it in
this order:

@chunk[<*>
       (require (for-syntax racket/base
                            syntax/parse 
                            racket/syntax)
                rackunit
                racket/list)       
       
       (define-syntax cas-cad-e
         (syntax-parser
          [(_ e:expr [opt body:expr ...+] ...)
           (with-syntax*
            ([(forward-id ...)         (generate-temporaries #'(opt ...))]
             [(reverse-id ...)         (reverse (syntax->list #'(forward-id ...)))]
             [((reverse-body ...) ...) (reverse (syntax->list #'((body ...) ...)))]
             [((next-id ...) ...)      (reverse (cdr (syntax->list #'((forward-id) ... ()))))])
            #'(let* ([reverse-id (lambda () reverse-body ... (next-id) ...)] ...)
                (case e [opt (forward-id)] ...)))]))

       <duff>
       <use>
       <test>]

@(the-end)
