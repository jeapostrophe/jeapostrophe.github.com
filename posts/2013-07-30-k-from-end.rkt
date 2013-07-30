#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/unit
                     racket/match
                     racket/list
                     data/heap)
          "../post.rkt")

@title{SIQ: Kth Element from the End of a List}
@categories["Racket" "Stupid Interview Questions"]

One of my students brough this interview question to my attention:

What is the most efficient way to find the @racket[_k]th element from
the end of a single linked list?

Let's see how to do it a bunch of different ways in Racket.

@(the-jump)

First, let's set up a testing infrastructure. We'll have a very large
list where each element is its offset from the end, so that
@racket[(equal? (k-from-end k l) k)].

@chunk[<testing>
       (define l
         (for/list ([i (in-range N)])
           (- N i 1)))
       (define (test k-from-end)
         (for ([k (in-range N)])
           (check-equal? (k-from-end k l) k)))]

However, since we're interested in speed, as well as correctness, we
should time when we test.

@chunk[<timing>
       (define (time label k-from-end)
         (define start (current-inexact-milliseconds))
         (test k-from-end)
         (define end (current-inexact-milliseconds))
         (printf "~a: ~a\n" (real->decimal-string (- end start)) label))]

Once this infrastructure is in place, we can try a variety of
techniques.

@section{Reverse and then Index}

One simple way to get the answer is to reverse the list and then index
into it:

@chunk[<reverse&list-ref>
       (define (reverse&list-ref k l)
         (list-ref (reverse l) k))]

We could try to make this faster by using an unsafe @racket[list-ref],
as well:

@chunk[<reverse&unsafe-list-ref>
       (define (reverse&unsafe-list-ref k l)
         (unsafe-list-ref (reverse l) k))]

And we could try to make an unsafe @racket[reverse]:

@chunk[<unsafe-reverse&unsafe-list-ref>
       (define (unsafe-reverse l)
         (let loop ([acc null] [l l])
           (if (null? l)
             acc
             (loop (cons (unsafe-car l) acc)
                   (unsafe-cdr l)))))
       (define (unsafe-reverse&unsafe-list-ref k l)
         (unsafe-list-ref (unsafe-reverse l) k))]

@section{Computed Index}

Rather than reversing and indexing to @racket[k], we could directly go
to the @racket[k]th from the end:

@chunk[<length&list-ref>
       (define (length&list-ref k l)
         (list-ref l (- (length l) k 1)))]

And an unsafe version of this as well:

@chunk[<length&unsafe-list-ref>
       (define (length&unsafe-list-ref k l)
         (unsafe-list-ref l (- (length l) k 1)))]

And we could try to make the call to @racket[length] unsafe as well:

@chunk[<unsafe-length&unsafe-list-ref>
       (define (unsafe-length l)
         (let loop ([acc 0] [l l])
           (if (null? l)
             acc
             (loop (unsafe-fx+ 1 acc) (unsafe-cdr l)))))
       (define (unsafe-length&unsafe-list-ref k l)
         (unsafe-list-ref l (- (unsafe-length l) k 1)))]

@section{Double Walk}

After we've exhausted the obvious solutions, we could try something a
bit more clever. For instance, we could bump one pointer out by
@racket[k] and then walk two pointers at the same time. When the one
@racket[k] down hits the end, the close pointer should be at the
correct element.

@chunk[<double-walk>
       (define (double-walk k l)
         (let loop ([l l]
                    [k-down
                     (list-tail l (+ k 1))])
           (if (empty? k-down)
             (first l)
             (loop (rest l)
                   (rest k-down)))))]

And an unsafe version:

@chunk[<unsafe-double-walk>
       (define (unsafe-double-walk k l)
         (let loop ([l l]
                    [k-down
                     (unsafe-list-tail l (unsafe-fx+ k 1))])
           (if (null? k-down)
             (unsafe-car l)
             (loop (unsafe-cdr l)
                   (unsafe-cdr k-down)))))]

@section{Fused length and index}

Another idea is to take the computed index and realize that when
trying to find the length, you have to traverse the whole list, but on
the way back up the list, you'd know when you were @racket[k] from the
end.

@chunk[<fused-length+list-ref>
       (define (fused-length+list-ref k l)
         (let/ec esc
           (let loop ([l l])
             (cond
               [(empty? l)
                0]
               [else
                (define n (loop (rest l)))
                (if (= n k)
                  (esc (first l))
                  (add1 n))]))))]

And there's a similar unsafe version:

@chunk[<unsafe-fused-length+list-ref>
       (define (unsafe-fused-length+list-ref k l)
         (let/ec esc
           (let loop ([l l])
             (cond
               [(null? l)
                0]
               [else
                (define n (loop (unsafe-cdr l)))
                (if (unsafe-fx= n k)
                  (esc (unsafe-car l))
                  (unsafe-fx+ n 1))]))))]

@section{Ring Buffer}

Finally, another idea is to create a fixed-size vector of @racket[k]
elements and fill it in as you traverse the whole list and then
quickly extract the appropriate element as you get to the end.

@chunk[<ring-buffer>
       (define (ring-buffer k+ l)
         (define k (add1 k+))
         (define rb (make-vector k #f))
         (let loop ([l l] [next 0])
           (cond
             [(empty? l)
              (vector-ref rb (modulo (- next k) k))]
             [else 
              (vector-set! rb next (first l))
              (loop (rest l)
                    (modulo (add1 next) k))])))]

And a similarly unsafe version:

@chunk[<unsafe-ring-buffer>
       (define (unsafe-ring-buffer k+ l)
         (define k (unsafe-fx+ k+ 1))
         (define rb (make-vector k #f))
         (let loop ([l l] [next 0])
           (cond
             [(null? l)
              (unsafe-vector-ref rb (unsafe-fxmodulo (unsafe-fx- next k) k))]
             [else 
              (unsafe-vector-set! rb next (unsafe-car l))
              (loop (unsafe-cdr l)
                    (unsafe-fxmodulo (unsafe-fx+ next 1) k))])))]

@section{Results}

Now, given all these different versions, which one is the fastest? If
we experiment @racket[N] set to @racket[1000], then the results are:

@verbatim{
21.38: length&list-ref
23.16: unsafe-length&unsafe-list-ref
23.43: unsafe-double-walk
26.12: unsafe-fused-length+list-ref
26.81: reverse&unsafe-list-ref
28.98: length&unsafe-list-ref
29.67: double-walk
30.07: reverse&list-ref
30.92: unsafe-reverse&unsafe-list-ref
34.69: fused-length+list-ref
36.75: unsafe-ring-buffer
50.45: ring-buffer
}

My student told me that his interviewer expected him to create the
@racket[double-walk] version to successfully pass the
test. Interestingly, the most obvious answer @racket[length&list-ref]
is the fastest in Racket and using unsafe operations rarely produces a
significant speed up.

@section{Yo! It's almost time to go!}

But first let's remember what we learned today!

Write the program that is best to read and understand. Racket is fast
enough without your help.

If you'd like to run this exact code at home, you should put it in
this order:

@CHUNK[<*>
       (require rackunit
                racket/list
                racket/unsafe/ops)

       (define N 1000)
       <testing>
       <timing>
       (define-syntax-rule (time-it f)
         (begin (collect-garbage)
                (collect-garbage)
                (collect-garbage)
                (time 'f f)))

       <reverse&list-ref>
       (time-it reverse&list-ref)

       <reverse&unsafe-list-ref>
       (time-it reverse&unsafe-list-ref)

       <unsafe-reverse&unsafe-list-ref>
       (time-it unsafe-reverse&unsafe-list-ref)

       <length&list-ref>
       (time-it length&list-ref)

       <length&unsafe-list-ref>
       (time-it length&unsafe-list-ref)

       <unsafe-length&unsafe-list-ref>
       (time-it unsafe-length&unsafe-list-ref)

       <double-walk>
       (time-it double-walk)

       <unsafe-double-walk>
       (time-it unsafe-double-walk)

       <fused-length+list-ref>
       (time-it fused-length+list-ref)

       <unsafe-fused-length+list-ref>
       (time-it unsafe-fused-length+list-ref)

       <ring-buffer>
       (time-it ring-buffer)

       <unsafe-ring-buffer>
       (time-it unsafe-ring-buffer)]

@(the-end)
