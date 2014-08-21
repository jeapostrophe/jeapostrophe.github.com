#lang plai

(test (+ 1 1)
      2)

;; compose-funk : (b -> c) (a -> b) -> (a -> c)
(define (compose-funk after before)
  ;; Test 1
  ;(compose-funk add1 add1)
  ;; (define (f x)
  ;;   (add1 (add1 x)))
  ;; f

  ;; Test 2
  ;; (define (f x)
  ;;   (number->string (add1 x)))
  ;; f

  ;; Generalize
  (local [(define (f x)
            (after (before x)))]
         f))

(test ((compose-funk add1 add1) 10)
      12)

(test ((compose-funk number->string add1) 7)
      "8")

;; bucket : LoN -> LLoN
(define (bucket-w/o-foldr lon)
  (cond
   [(empty? lon)
    empty]
   [else
    (if
     (and (not (empty? (bucket-w/o-foldr (rest lon))))
          (not (empty? (first (bucket-w/o-foldr (rest lon)))))
          (= (first (first (bucket-w/o-foldr (rest lon))))
             (first lon)))
     
     (cons
      (cons (first lon) (first (bucket-w/o-foldr (rest lon))))
      (rest (bucket-w/o-foldr (rest lon))))
          
     (cons (list (first lon))
           (bucket-w/o-foldr (rest lon))))]))

(define (add-to-bucket-list first-thing fun-applied-to-rest)
  (if
     (and (not (empty? fun-applied-to-rest))
          (not (empty? (first fun-applied-to-rest)))
          (= (first (first fun-applied-to-rest))
             first-thing))
     
     (cons
      (cons first-thing (first fun-applied-to-rest))
      (rest fun-applied-to-rest))
          
     (cons (list first-thing)
           fun-applied-to-rest)))
  
(define (bucket lon)
  (foldr add-to-bucket-list empty lon))
  
(test (bucket empty)
      empty)

(test (bucket '(1))
      '((1)))

(test (bucket '(2 1))
      '((2) (1)))

(test (bucket (list 1 1 2 2 3 2 2 1 1))
      '((1 1) (2 2) (3) (2 2) (1 1)))

(test (bucket (list 1 1 1 2 2 3 2 2 1 1))
      '((1 1 1) (2 2) (3) (2 2) (1 1)))

;; 1 + 1
;; + 1 1
;; 1 1 +

;(define-struct num (the-number))
;(define-struct add (the-lhs the-rhs))

;; abstract syntax
(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [mult (lhs AE?)
       (rhs AE?)])

;; abstract program
(add (num 1)
     (num 1))


;; concrete program
(define e '(+ 1 1))
e
(empty? e)
(cons? e)
(first e)
(second e)
(third e)

;; concrete syntax
#|
AE = <number>
   | (* <AE> <AE>)
   | (+ <AE> <AE>)
|#

;; parse : concrete -> abstract
(define (parse c)
  (cond
   [(number? c)
    (num c)]
   [(and (list? c)
         (= 3 (length c))
         (equal? '+ (first c)))
    (add (parse (second c))
         (parse (third c)))]
   [(and (list? c)
         (= 3 (length c))
         (equal? '* (first c)))
    (mult (parse (second c))
          (parse (third c)))]
   [else
    (error 'parse "Bd programmer, no cake ~e" c)]))

(parse e)

;; paren'd thing is an s-expression, abbrv'd sexpr
;; <xml><a>stupid</a><way>of writing</way><sexpr /></xml>
;; (xml (a stupid) (way of writing) (sexpr))

(test (parse '(+ 1 1))
      (add (num 1) (num 1)))
(test/exn (parse '(+ 1 1 1))
          "no cake")

(test (parse '(* 3 1))
      (mult (num 3) (num 1)))

;; interp : program -> meaning

;; calc : AE -> number
(define (calc some-ae)
  (type-case
   AE some-ae
   [num (n)
        n]
   [add (lhs rhs)
        (+ (calc lhs)
           (calc rhs))]
   [mult (lhs rhs)
         (* (calc lhs)
            (calc rhs))]))

(test (calc (parse '5))
      5)
(test (calc (parse '42))
      42)

(test (calc (parse '(+ 1 1)))
      2)
(test (calc (parse '(+ 1 99)))
      100)

(test (calc (parse '(* 3 2)))
      6)
(test (calc (parse '(* 1/99 99)))
      1)

(test (calc (parse '(+ (+ (+ (+ 1 1) (+ 1 1)) (+ (+ 1 1) (+ 1 1))) (+ 1 1))))
      10)
