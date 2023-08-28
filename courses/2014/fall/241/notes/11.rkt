#lang racket
(require plot)

(define (quicksort-cost bad-case? c n)
  (cond
   [(<= n 1)
    1]
   [(and bad-case? (= (modulo n 2) 1))
    (+ (quicksort-cost bad-case? c (- n 1))
       n)]
   [else
    (+ (quicksort-cost bad-case? c (floor (* c n)))
       (quicksort-cost bad-case? c (floor (* (- 1 c) n)))
       n)]))

(define (nlogn c n)
  (* c n (/ (log n) (log 2))))

(plot #:y-min 0
      #:x-min 0
      #:x-max 100
      (list
       (function (λ (n)
                   (quicksort-cost #t 1/2 (floor n)))
                 #:color 0
                 #:label "bad then 1/2")
       (function (λ (n)
                   (quicksort-cost #t 9/10 (floor n)))
                 #:color 1
                 #:label "bad then 9/10")
       (function (λ (n)
                   (quicksort-cost #f 1/2 (floor n)))
                 #:color 2
                 #:label "1/2")
       (function (λ (n)
                   (quicksort-cost #f 9/10 (floor n)))
                 #:color 3
                 #:label "9/10")
       (function (λ (m)
                   (nlogn 1 (floor m)))
                 #:color 4
                 #:label "nlgn")
       (function (λ (m)
                   (nlogn 2 (floor m)))
                 #:color 5
                 #:label "2nlgn")
       (function (λ (m)
                   (nlogn 3 (floor m)))
                 #:color 6
                 #:label "3nlgn")))


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
