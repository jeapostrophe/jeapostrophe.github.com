#lang racket/base
(require plot)

(define (naive-mult n)
  (if (<= n 1) 1
      (+ (* 8 (naive-mult (/ n 2)))
         (* 4 (naive-adds n)))))
(define (strassen-mult n)
  (if (<= n 1) 1
      (+ (* 7 (strassen-mult (/ n 2)))
         (* 18 (naive-adds n)))))
(define (naive-adds n)
  (* n n))

(plot (list (function naive-mult
                      #:label "naive"
                      #:color 0)
            (function strassen-mult
                      #:label "strassen"
                      #:color 1))
      #:x-min 0
      #:x-max 4500
      #:y-min 0)
