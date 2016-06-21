#lang racket/base

(define (cut-rod/slow p n)
  (cond
    [(= n 0)
     0]
    [else
     (for/fold ([q -inf.0])
               ([i (in-range 0 n)])
       (max q
            (+ (vector-ref p i)
               (cut-rod/slow p (- n i 1)))))]))

(define (cut-rod p n)
  (define n->cut-rod-n
    (make-vector (add1 (vector-length p)) #f))
  (define (inner-cut-rod n)
    (define x (vector-ref n->cut-rod-n n))
    (cond
      [x
       x]
      [else
       (define rx
         (inner-cut-rod-do-real-work n))
       (vector-set! n->cut-rod-n n rx)
       rx]))
  (define (inner-cut-rod-do-real-work n)
    (cond
      [(= n 0)
       0]
      [else
       (for/fold ([q -inf.0])
                 ([i (in-range 0 n)])
         (max q
              (+ (vector-ref p i)
                 (inner-cut-rod
                  (- n i 1)))))]))
  (inner-cut-rod n))

(module+ test
  (printf "Starting...\n")
  (cut-rod
   (vector 1 5 8 9 10 17 17 20 24 30
           31 32 33 34 35 36 37 38 39
           40 41 42 43 44 44 45 46 47 59
           60)
   30))
