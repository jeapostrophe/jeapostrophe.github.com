#lang plai

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
        (rhs AE?)]
  [id (s symbol?)]
  [with (name symbol?)
        (named-thing AE?)
        (body AE?)])

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
   | (with (<id> <AE>) <AE>)
   | <id>
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
   [(and (list? c)
         (= 3 (length c))
         (equal? 'with (first c)))
    (with (first (second c))
          (parse (second (second c)))
          (parse (third c)))]
   [(symbol? c)
    (id c)]
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

;; subst : id AE AE -> AE
(define (subst i v e)
  (type-case
   AE e
   [num (n)
        (num n)]
   [add (lhs rhs)
        (add
         (subst i v lhs)
         (subst i v rhs))]
   [mult (lhs rhs)
         (mult
          (subst i v lhs)
          (subst i v rhs))]
   [id (s)
       (if (equal? s i)
           v
           (id s))]
   [with (i* v* e*)
         (if (equal? i i*)
             (with i*
                   (subst i v v*)
                   e*)
             (with i*
                   (subst i v v*)
                   (subst i v e*)))]))  

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
            (calc rhs))]
   [id (s)
       (error 'calc "Unbound undiscovery infinite: ~e" s)]
   [with (i v e)
         ;(calc (subst i v e))
         (calc (subst i (num (calc v)) e))
         ]))

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


(define awesomeness 42)
(define really-hard-to-compute (+ 1 1))

(test (calc (parse '(with (x (+ 1 1)) (+ x x))))
      4)
(test (calc (parse '(with (x 2) (+ x x))))
      4)
(test (calc (parse '(with (x 2) (+ 2 x))))
      4)
(test (calc (parse '(with (x 2) (+ 2 2))))
      4)
(test (calc (parse '(+ 2 2)))
      4)

(test/exn (calc (parse 'x))
          "Unbound")
(test (calc (parse '(with (x (+ 1 1)) (* x x))))
      4)
(test (calc (parse '(with (x 1) (with (y 2) (+ x y)))))
      3)

(test (subst 'x (parse '1) (parse '(with (y 2) (+ x y))))
      (parse '(with (y 2) (+ 1 y))))

(test (calc (parse '(with (y 2) (+ 1 y))))
      3)

;; This tells if we are substituting text or not:
(test (calc (parse '(with (y x) 3)))
      3)

;; f(x) = 1 + x
;; f(2) = 1 + 2

;; Substitution 
;; Definition 1: switch what you know for what you don't

;; Substitution of i for v in e
;; Rephrase: switch v for i inside e
;; Rephrase: replace all is with v inside e

(test (subst 'x (parse '1) (parse '(+ x x)))
      (parse '(+ 1 1)))

;; This result, implies definition 1 is wrong
;(test (subst 'x (parse '1) (parse '(with (x 2) x)))
;      (parse '(with (1 2) 1)))

;; Definition 1a: replace all is with v inside e provided they are not themselves binding
;(test (subst 'x (parse '1) (parse '(with (x 2) x)))
;      (parse '(with (x 2) 1)))

;; Definition 1b: replace all is with v inside e provided they are not inside a with
(test (subst 'x (parse '1) (parse '(with (x 2) x)))
      (parse '(with (x 2) x)))
;(test (subst 'x (parse '1) (parse '(with (y 2) x)))
;      (parse '(with (y 2) x)))

;; Definition 2: replace all is with v inside e provided they are not inside a with that has the same i as the thing being named
(test (subst 'x (parse '1) (parse '(with (x 2) x)))
      (parse '(with (x 2) x)))
; (with (x 1) (with (y 2) x))
(test (subst 'x (parse '1) (parse '(with (y 2) x)))
      (parse '(with (y 2) 1)))

(test (subst 'x (parse '1) (parse '(with (y x) x)))
      (parse '(with (y 1) 1)))

;(test (subst 'x (parse '1) (parse '(with (x x) x)))
;      (parse '(with (x x) x)))

;; Definition 3: replace all is with v inside e provided they are not inside a with body where the with has the same i as the thing being named
;(test (subst 'x (parse '1) (parse '(with (x x) x)))
;      (parse '(with (1 1) x)))

;; Definition 3': replace all valid id references of i with v inside e provided they are not inside a with body where the with has the same i as the thing being named
(test (subst 'x (parse '1) (parse '(with (x x) x)))
      (parse '(with (x 1) x)))

;; A free occurrence is an occurrence that does not appear inside a with binding the id
;; Definition 4: replace all free occurrences of i with v in e

;; A value is a ...
;; A binding is a naming of a value
;; An occurrence is a reference to an identifier
;; A bound occurrence is a non-free occurrence
;; A scope is the program text that a binding binds.

;(with (x 1) (with (y 2) (+ x y)))

; De Bruijn Index == aka look ma no names
;(with 1 (with 2 (+ (@ 1) (@ 0))))

