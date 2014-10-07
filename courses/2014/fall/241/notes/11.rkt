#lang racket
(require plot)

(define (quicksort-cost c n)
  (cond
   [(<= n 1)
    1]
   [(= (modulo n 2) 1)
    (+ (quicksort-cost c (- n 1))
       n)]
   [else
    (+ (quicksort-cost c (floor (* c n)))
       (quicksort-cost c (floor (* (- 1 c) n)))
       n)]))
(plot #:y-min 0
      #:x-min 0
      #:x-max 100
      (list
       (points (for/list ([i (in-range 100)])
                 (vector i (quicksort-cost 1/2 i)))
               #:color 0)
       (function (λ (n)
                   (quicksort-cost 1/2 n))
                 #:color 0
                 #:label "1/2")
       (points (for/list ([i (in-range 100)])
                 (vector i (quicksort-cost 9/10 i)))
               #:color 1)
       (function (λ (n)
                   (quicksort-cost 9/10 n))
                 #:color 1
                 #:label "9/10")
       (function (λ (n)
                   (* n (/ (log n) (log 2))))
                 #:color 2
                 #:label "nlgn")
       (function (λ (n)
                   (* 2 (* n (/ (log n) (log 2)))))
                 #:color 3
                 #:label "2nlgn")))


(define (f n q)
  (+ (sqr q) (sqr (- n q 1))))

(define n 50)

(plot (list
       (function (λ (q) (f n (max 0 (min (sub1 n) q))))
                 #:color 1
                 (* -1 n) (* 2 n))
       (function (λ (q) (f n q))
                 #:color 2
                 (* -1 n) (* 2 n)))
      #:y-min 0
      #:y-max (* 2 (expt n 2)))
