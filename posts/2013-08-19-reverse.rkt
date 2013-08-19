#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/unit
                     racket/match
                     racket/list
                     data/heap)
          "../post.rkt")

@title{SIQ: Reversing Strings}
@categories["Racket" "Stupid Interview Questions"]

A common interview question is reversing the characters in a
string. In this post, I present three ways to do this and measure the
performance of the various approaches in Racket, as well as show how
to verify the correctness of the algorithms.

@(the-jump)

First, let's look at the simplest and most obviously correct way to do
it: turn the string into a list and then @racket[reverse] the list,
and go back to a string:

@chunk[<slow>
       (define (reverse-string/slow s)
         (list->string (reverse (string->list s))))]

This version is likely to be slow because, for a @racket[_n] character
string, it processes @racket[(* 3 _n)] characters, allocates one
@racket[_n] string, and @racket[(* 2 _n)] @racket[cons] cells.

A faster method would just process each character once and only
allocate the new string once. We could do that by directly computing
the characters to "swap".

@chunk[<medium>
       (define (reverse-string s)
         (define len (string-length s))
         (build-string len (λ (i) (string-ref s (- len i 1)))))]

You may think this version traverses the string a second time to
compute the length, but in Racket this value is built-in to the
string, so it is immediately available.

We might be able to go a lot faster if we did the reversal "in-place"
by just swapping the characters. Since we're just reversing the
string, we could go from the outside to inside and swap the
characters.

@chunk[<really-fast>       
       (define (reverse-string/really-fast s)
         (define len (string-length s))
         (for ([c (in-string s)]
               [i (in-range (quotient len 2))])
           (define ni (- len i 1))
           (define nc (string-ref s ni))
           (string-set! s ni c)
           (string-set! s i nc))
         s)]

The only unfortunate part of this implementation is that it
permanently mutates the string. If we made a copy of the string first,
which is a faster operation than copying the characters one-by-one, we
could probably beat the performance of the medium version but preserve
its semantics.

@chunk[<fast>
       (define (reverse-string/fast s)
         (reverse-string/really-fast (string-copy s)))]

If we test these implementations on 40,000 random strings of length
1000, then the performance is pretty clear:

@itemlist[

@item{1387 ms for the slow}

@item{877 ms for the medium}

@item{550 ms for the fast}

@item{396 ms for the really fast}
          
]

I am most interested in the speed of the "fast" version, because it is
a drop-in replacement for the "slow" and "medium" versions, but almost
three times faster.

@section{Yo! It's almost time to go!}

But first let's remember what we learned today!

You can do performance engineering in Racket, just like you can in C.

Validating optimizations and performance is much simpler with Racket.

If you'd like to run this exact code at home, you should put it in
this order:

@chunk[<*>
       (require rackunit
                racket/string
                racket/list)

       (define (random-char)
         (if (zero? (random 5))
           #\space
           (integer->char (+ (char->integer #\A) (random 26)))))
       (define (random-string N)
         (list->string (build-list N (λ (i) (random-char)))))

       <slow>
       <medium>
       <really-fast>
       <fast>

       (define N 100)

       (define ss (build-list N (λ (i) (random-string 100))))       
       
       (define-syntax-rule
         (time-exp exp)
         (let-values ([(as cpu real gc) (time-apply (λ () exp) empty)])
           (collect-garbage) (collect-garbage) (collect-garbage)
           (values (first as)
                   cpu)))

       (define-values
         (slows st)
         (time-exp
          (for/list ([s (in-list ss)])
            (reverse-string/slow s))))

       (define-values
         (mediums mt)
         (time-exp
          (for/list ([s (in-list ss)])
            (reverse-string s))))

       (define-values
         (fasts ft)
         (time-exp
          (for/list ([s (in-list ss)])
            (reverse-string/fast s))))

       (define-values
         (reallys rt)
         (time-exp
          (for/list ([s (in-list ss)])
            (reverse-string/really-fast s))))

       (for ([s (in-list slows)]
             [l (in-list slows)]
             [m (in-list mediums)]
             [f (in-list fasts)]
             [r (in-list reallys)])
         (check-equal? m l)
         (check-equal? f l)
         (check-equal? r l))

       (printf "~a\n" (list st mt ft rt))]

@(the-end)
