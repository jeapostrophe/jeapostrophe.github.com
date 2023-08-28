#lang racket/base

(define ts
  (for/list ([i (in-range 5000)])
    (thread
     (λ ()
       (for ([j (in-range 10)])
         (displayln (cons i j)))))))
(for-each thread-wait ts)
