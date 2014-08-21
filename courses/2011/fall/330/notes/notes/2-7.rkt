#lang plai

(test (+ 1 1)
      2)
(test (+ 1 1)
      3)

;; compose-funk : (b -> c) (a -> b) -> (a -> c)
(define (compose-funk after before)

  ;; Test 1
  ;; (define (add2 x)
  ;;   (add1 (add1 x)))
  ;; add2

  ;; Test 2
  ;; (define (add1-and-stringify x)
  ;;   (number->string (add1 x)))
  ;; add1-and-stringify

  ;; Generalize
  (define (f x)
    (after (before x)))
  f)

(define (add2 x)
  (add1 (add1 x)))
(test ((compose-funk add1 add1) 7)
      9)
(test (add2 7)
      9)
(test (add1 (add1 7))
      9)
(test (add1 8)
      9)
(test 9
      9)

(define (add1-and-stringify x)
  (number->string (add1 x)))
(test ((compose-funk number->string add1) 7)
      "8")

;; bucket : LoN -> LLoN
(define (bucket-w/o-foldr lon)
  (cond
   [(empty? lon)
    empty]
   [else
    (if
     (and
      (not (empty? (bucket (rest lon))))
      (not (empty? (first (bucket (rest lon)))))
      (= (first lon)
         (first (first (bucket (rest lon))))))
     
     (cons
      (cons (first lon) (first (bucket (rest lon))))
      (rest (bucket (rest lon))))

     (cons (list (first lon))
           (bucket (rest lon))))]))

(define (add-to-buckets first-thing fun-applied-to-the-rest)
  (if
   (and
    (not (empty? fun-applied-to-the-rest))
    (not (empty? (first fun-applied-to-the-rest)))
    (= first-thing
       (first (first fun-applied-to-the-rest))))
   
   (cons
    (cons first-thing (first fun-applied-to-the-rest))
    (rest fun-applied-to-the-rest))

   (cons (list first-thing)
         fun-applied-to-the-rest)))

(define (bucket lon)
  (foldr add-to-buckets
         empty
         lon))
    
(test (bucket empty)
      empty)

(test (bucket '(1))
      '((1)))

(test (bucket '(2 1))
      '((2) (1)))

(test (bucket '(1 1 2 2 3 3 2 1))
      '((1 1) (2 2) (3 3) (2) (1)))
      
(test (bucket '(1 1 1 2 2 3 3 2 1))
      '((1 1 1) (2 2) (3 3) (2) (1)))

;; concrete syntax (BNF = Backus-Naur Form)
; AE = <number>
;    | ( + <AE> <AE> )

(define e '(+ 1 1))
e
(empty? e)
(cons? e)
(list? e)
(first e)
(second e)
(third e)

;; abstract syntax
;(define-struct num (n))
;(define-struct add (lhs rhs))
(define-type AE
  [num (n number?)]
  [add (lhs AE?) (rhs AE?)])

(define p 
  (add (num 1) (num 1)))
p

; parse : concrete -> program
(define (parse c)
  (cond
    [(number? c)
     (num c)]
    [(and (list? c)
          (= (length c) 3)
          (equal? '+ (first c)))
     (add (parse (second c))
          (parse (third c)))]
    [else
     (error 'parse "Bad programmer")]))

(parse e)

(test/exn (parse '(+ 1 1 1))
          "Bad programmer")
(test/exn (parse '(+ 1 ))
          "Bad programmer")

;; well-formed vs valid

;; <xml>is <just>a <stupid/></just> <way>of writing</way> parenthesized <things /></xml>
;; (xml is (just a (stupid)) (way of writing) parenthesized (things))

; interp : program -> meaning

; calc : AE -> number
(define (calc some-ae)
  (type-case
   AE some-ae
   [num (the-number-inside)
        the-number-inside]
   [add (the-lhs the-rhs)
        (+ 
         (calc the-lhs)
         (calc the-rhs))]))

(test (calc (parse '42))
      42)
(test (calc (num 42))
      42)
(test (calc (parse '(+ 1 1)))
      2)
(test (calc (add (num 1) (num 1)))
      2)
(test (calc (parse '(+ (+ (+ 1 1) 1) (+ 1 (+ (+ 1 1) 1)))))
      7)
