#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/unit
                     racket/match
                     racket/list
                     data/heap)
          "../post.rkt")

@title{Burrows-Wheeler Transform}
@categories["Racket" "Algorithms" "Compression"]

The Burrows-Wheeler Transform is a cute algorithm that used in
compression. This post shows a simple Racket implementation.

@(the-jump)

The
@link["http://www.hpl.hp.com/techreports/Compaq-DEC/SRC-RR-124.html"]{Burrows-Wheeler
Transform} is based on the idea of lexicographically ordering all the
rotations of a string, then extracting the final characters of each
ordered rotations. This pushes common sub-sequences close together and
creates repetitions. It can be inverted by re-constructing the table
of rotations and extracting the row that corresponds to the original
input, which can be identified with a sentinel value.

The core of encoding is trivial, assuming we have a function that
returns all rotations of a string.

@chunk[<encode>
       (define (encode s)
         (define rotation-table
           (sort (rotations s) string<?))
         (list->string
          (map string-last
               rotation-table)))]

It is easy to write such a @racket[rotations] function, as well, since
we just consider the rotation beginning at each character of the input
string:

@chunk[<rotations>
       (define (rotations s)
         (for/list ([i (in-range (string-length s))])
           (rotation-start-at s i)))]

Then constructing a rotation starting at some character @racket[_i]
just means we take all the characters after @racket[_i] then all the
characters before @racket[_i]:

@chunk[<rotation>
       (define (rotation-start-at s i)
         (string-append (substring s i)
                        (substring s 0 i)))]

Decoding is a little bit tricky in the details, but simple if we have
access to the rotation table. We just take the row that ends in the
sentinel:

@chunk[<decode>
       (define (decode s)
         (findf (λ (s) (sentinel? (string-last s)))
                (make-rotation-table s)))]

The hard part is getting the rotation table. The key idea is to add
the decoded string over and over from the right, sorting each
time. Here we us a few cute Racket-isms to do it. We turn the string
into a list so that we can @racket[map] over it, using the two list
version of @racket[map] that calls the function on the paired elements
of each list.

@chunk[<rotation-table>
       (define (make-rotation-table s)
         (define len (string-length s))
         (define sl (string->list s))
         (for/fold ([t (make-list len "")])
             ([i (in-range len)])
           (sort (map string-cons sl t) string<?)))]

Something I love about encoding and decoding algorithms is that they
are simple to verify: you just make sure you can take a round
trip. Here's some code to generate a large number of strings and then
compare the original string to the decoded encoding. (In this example,
we define the sentinel character as @litchar[~].)

@chunk[<check-random>
       (for ([i (in-range N)])
         (define s 
           (build-string (add1 i) 
                         (λ (j)
                           (if (= i j)
                             #\~
                             (integer->char (+ 65 (random 26)))))))
         (check-equal? (decode (encode s)) s))]

This is not an especially fast version of this algorithm, but I find
it to be very pretty.

If you'd like to use this code at home, you should put it in this
order:

@chunk[<*>
       (require rackunit
                racket/list)

       (define (string-last s)
         (string-ref s (sub1 (string-length s))))

       <rotation>      
       <rotations>
       <encode>

       (define (string-cons c s)
         (string-append (string c) s))

       (define (sentinel? c)
         (char=? #\~ c))

       <rotation-table>
       <decode>       

       (define-syntax-rule (round-trip in out)
         (begin
           (check-equal? (decode (encode in)) in)
           (check-equal? (encode in) out)
           (check-equal? (decode out) in)))

       (round-trip
        "^BANANA~"
        "BNN^AA~A")

       (round-trip
        "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES~"
        "TEXYDST.E.IXIXIXXSSMPPS.B..E.~.UESFXDIIOIIITS")

       (define N 100)
       <check-random>]

@(the-end)
