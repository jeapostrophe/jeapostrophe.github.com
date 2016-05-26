#lang racket/base
(require plot)

(define c1 1/2)
(define c2 1000)
(define (g n)
  (f (/ n 2)))
(define (f n)
  (expt 2 n))
(plot (list (function (λ (x) (* c1 (g x)))
                      #:label "c1"
                      #:color 0)
            (function f
                      #:label "f"
                      #:color 1)
            (function (λ (x) (* c2 (g x)))
                      #:label "c2"
                      #:color 2))
      #:x-min 0
      #:x-max 30
      #:y-min 0)
