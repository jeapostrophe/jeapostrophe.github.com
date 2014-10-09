#lang racket
(require plot
         rackunit)
(plot-new-window? #t)

;; A list is either
;; - an empty, or
;; - a (cons element list)

;; A nat is either
;; - 0, or
;; - (succ nat)

;; F : (A0 : ANS)
;;     (FS :    (Am   : ANS)
;;           -> (Am+1 : ANS)
;;     ( n : nat)
;;  -> (An : ANS)
(define (NatInd BaseCase InductiveStep n)
  (cond
   [(zero? n)
    BaseCase]
   [else
    (InductiveStep (NatInd BaseCase InductiveStep (sub1 n)))]))

(define (DOUBLE n)
  (NatInd 0 (λ (doubled_number_inside)
              (+ doubled_number_inside 2))
          n))

(module+ test
  (check-equal? (DOUBLE 0) 0)
  (check-equal? (DOUBLE 4) 8)
  (check-equal? (DOUBLE 3) 6))

(define (ListInd BaseCase InductiveStep l)
  (cond
   [(empty? l)
    BaseCase]
   [else
    (InductiveStep (first l)
                   (ListInd BaseCase InductiveStep (rest l)))]))

#;(define (Insert e l)
  (cond
   [(empty? l)
    (list e)]
   [else
    (if (<= e (first l))
        (cons e l)
        (cons (first l)
              (Insert e (rest l))))]))

(define (Insert e l)
  (vector-ref (ListInd (vector empty (list e))
           (λ (the-first non-inserted-rest*inserted-rest)
             (match-define (vector non-inserted-rest inserted-rest)
                           non-inserted-rest*inserted-rest)
             (if (<= e the-first)
                 (vector (cons the-first non-inserted-rest)
                         (cons e (cons the-first non-inserted-rest)))
                 (vector (cons the-first non-inserted-rest)
                         (cons the-first inserted-rest))))
           l) 1))
(define (ISort l)
  (ListInd empty (λ (elem sorted-rest) (Insert elem sorted-rest))
           l))

(module+ test
  (check-equal? (ISort '(3 8 4 7 1 0 48 1))
                '(0 1 1 3 4 7 8 48)))

;; STRUCTURAL RECURSION
;;   recursive call is to a structural sub-piece of input
;; (define (map f l)
;;   (cond
;;    [(empty? l)
;;     ...
;;     use f
;;     ...]
;;    [(pair? l)
;;     ...
;;     use f
;;     use (first l)
;;     (map f (rest l))
;;     ...]))

(define (f l)
  (f (cons 1 l)))
(define (g l)
  (g (cons 1 (rest l))))
(define (kollatz n)
  (if (even? n)
      (quotient n 2)
      (kollatz (+ 1 (* 3 n)))))
(module+ test
  (kollatz 15))

;; QS(n) = n * lg (n)
;; IS(n) = n^2
;;       = n * LookOverEntireArray
;;       = n * LookUntilSomethingSmaller
;; QS'(n) = QS(n) - SomeWork 
;;        = QS(n) - CostPerLevel * lg (k)
;;        = QS(n) - n * lg (k)
;;        = n * lg (n) - n * lg (k)
;;        = n * lg (n / k)
;; Original Goal:
;; ALG(n) = n * k + n * lg (n / k)
;;        = QS'(n) + IS'(n)
;;        = IS'(n) + n * lg (n / k)
;; New Goal:
;; IS'(n) = n * k
;;        = (n/k) * k^2
;;        > HowManySubArrays * k^2

;; (k unsorted things smaller than R) R (k unsorted things bigger than R)

;; Choose k such that
;; n * lg n >= n * k + n * lg (n / k)
;; n * lg n >= n * k + n * lg n - n * lg k
;;        0 >= n * k - n * lg k
;;        0 >= k - lg k
;;     lg k >= k 

;; Practce
;; Q * n * lg n >= I * n * k + Q * n * lg (n / k)
;; n * lg n >= n * k + n * lg n - n * lg k
;;        0 >= n * k - n * lg k
;;        0 >= k - lg k
;;     lg k >= (1 / Q) k 
