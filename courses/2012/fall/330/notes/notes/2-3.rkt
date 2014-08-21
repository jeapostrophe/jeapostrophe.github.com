;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 2-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; sum : listof numbers -> number
;; compute the sum of all the numbers in the list
(define (sum l)
  (foldr ;; make-me-a-homomorphism-sir
   +
   0
   l)
  #;
  (cond
    [(empty? l)
     0]
    [else
     (+ (first l)
        (sum (rest l)))]))

(check-expect (sum empty)
              0)
(check-expect (sum (list 2))
              2)
(check-expect (sum (list 1 2))
              3)

;; homo-morph-ism aka "fold"
(check-expect (sum (cons 1 (cons 2 empty)))
                   (   + 1 (   + 2     0)))

;; foldr = bottom up
;; foldl = top down

;; my-foldr : ( A B -> B ) B (listof A) -> B
(define (my-foldr replace-cons replace-empty l)
  (cond
    [(empty? l)
     replace-empty]
    [else
     (replace-cons
      (first l)
      (my-foldr replace-cons
                replace-empty
                (rest l)))]))

;; even-stevens : (listof number) -> (listof number)
(define (even-stevens l)
  (filter even? l)
  #;
  (cond
    [(empty? l)
     empty]
    [else
     (cond 
       [(even? (first l))
        (cons (first l)
              (even-stevens (rest l)))]
       [else
        (even-stevens (rest l))])]))

(check-expect (even-stevens (list 1 2 3 5 7 11))
              (list 2))

(define (odd-mccoob l)
  (filter odd? l))

(check-expect (odd-mccoob (list 1 2 3 5 7 11))
              (list 1 3 5 7 11))

(define (my-filter ? l)
  (cond
    [(empty? l)
     empty]
    [(? (first l))
     (cons (first l)
           (my-filter ? (rest l)))]
    [else
     (my-filter ? (rest l))]))

(define (greater-than-5? x)
  (> x 5))
(check-expect (filter greater-than-5?
                      (list 1 4 6 8 10))
              (list 6 8 10))

;; filtering functions

;; map ping functions

(define (evenate x)
  (* 2 x))

;; map : (A -> B) (list A) -> (list B)
(define (my-map f l)
  (cond
    [(empty? l)
     empty]
    [else
     (cons (f (first l))
           (map f (rest l)))]))

(define (evenator l)
  (my-map evenate l)
  #;
  (cond
    [(empty? l)
     empty]
    [else
     (cons (* 2 (first l))
           (evenator (rest l)))]))

(check-expect (evenator (list 1 2 3 7))
              (list 2 4 6 14))

(check-expect (map number->string (list 1 2 3))
              (list "1" "2" "3"))
(check-expect (map even? (list 1 2 3))
              (list false true false))

(define (nenator n l)
  (local [(define (nenate x)
            (* n x))
          (define fonzzie "Ehy!")]
    (map nenate
         l)))

(check-expect (nenator 2 (list 1 2 3 7))
              (list 2 4 6 14))
(check-expect (nenator 10 (list 1 2 3 7))
              (list 10 20 30 70))

#|
(define (nenate x)
  (* n x))
-- first remove the name -->
(define (x)
  (* n x))
-- then change define to lambda -->
(lambda (x)
  (* n x))
-- then move from definition spot to use spot
|#

(define (omg-nenator n l)
  ;; look mah.... no locals
  (map (λ (x) (* n x))
       l))

(check-expect (omg-nenator 2 (list 1 2 3 7))
              (list 2 4 6 14))
(check-expect (omg-nenator 10 (list 1 2 3 7))
              (list 10 20 30 70))

(define (delayed-add x)
  (local [(define (do-it! y)
            (+ x y))]
    do-it!))

(define add-5 (delayed-add 5))

(add-5 6)
(add-5 7)