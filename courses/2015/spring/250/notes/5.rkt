#lang racket/base

(define (sublist l start len)
  (for/list ([i (in-range len)])
    (list-ref l (+ start i))))

(define (buckets n l)
  (for/list ([i 
              (in-range (- (length l) 
                           n))])
    (sublist l i n)))

(module+ test
  (define rs
    (for/list ([i (in-range 1000)])
      (random 10)))
  (define bs 
    (buckets 5 rs))
  (define FREQ (make-hash))
  (for ([b (in-list bs)])
    (hash-update! FREQ b add1 0))
  
  1/100000
  
  (for ([(k freq) (in-hash FREQ)]
        #:when (> freq 1))
    (printf "~a -> ~a\n" k freq)))
