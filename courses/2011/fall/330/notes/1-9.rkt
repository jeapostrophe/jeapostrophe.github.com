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
        (body AE?)]
  [app (fun-name symbol?) 
       (arg AE?)])

;; abstract data type for functions
(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
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
   | (<id> <AE>)
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
   [(and (list? c)
         (= 2 (length c))
         (symbol? (first c)))
    (app (first c) (parse (second c)))]
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
                   (subst i v e*)))]
    [app (fun-name arg-expr)
         (app fun-name (subst i v arg-expr))]))  

;; lookup-fundef : symbol? (listof FunDef) -> FunDef
(define (lookup-fundef name fun-defs)
  (cond
    [(empty? fun-defs)
     (error name "function not found")]
    [else
     (if (symbol=? name (fundef-fun-name (first fun-defs)))
         (first fun-defs)
         (lookup-fundef name (rest fun-defs)))]))

;; interp : AE (listof FunDef) -> meaning
(define (interp some-ae fun-defs)
  (type-case
      AE some-ae
    [num (n)
         n]
    [add (lhs rhs)
         (+ (interp lhs fun-defs)
            (interp rhs fun-defs))]
    [mult (lhs rhs)
          (* (interp lhs fun-defs)
             (interp rhs fun-defs))]
    [id (s)
        (error 'interp "Unbound undiscovery infinite: ~e" s)]
    [with (i v e)
      ;(interp (subst i v e))
      (interp (subst i (num (interp v fun-defs)) e) fun-defs)
      ]
    [app (fun-name arg-expr)
         ;; ('double (num 3))
         ;; (fundef 'double 'x (add (id 'x) (id 'x)))
         (local [(define fun-def (lookup-fundef fun-name fun-defs))]
           (interp (subst (fundef-arg-name fun-def)
                          (num (interp arg-expr fun-defs))
                          (fundef-body fun-def))
                   fun-defs))]))

(test (interp (parse '5) empty)
      5)
(test (interp (parse '42) empty)
      42)

(test (interp (parse '(+ 1 1)) empty)
      2)
(test (interp (parse '(+ 1 99)) empty)
      100)

(test (interp (parse '(* 3 2)) empty)
      6)
(test (interp (parse '(* 1/99 99)) empty)
      1)

(test  (interp (parse '(+ (+ (+ (+ 1 1) (+ 1 1)) (+ (+ 1 1) (+ 1 1))) (+ 1 1))) empty)
      10)


(define awesomeness 42)
(define really-hard-to-compute (+ 1 1))

(test (interp (parse '(with (x (+ 1 1)) (+ x x))) empty)
      4)
(test (interp (parse '(with (x 2) (+ x x))) empty)
      4)
(test (interp (parse '(with (x 2) (+ 2 x))) empty)
      4)
(test (interp (parse '(with (x 2) (+ 2 2))) empty)
      4)
(test (interp (parse '(+ 2 2)) empty)
      4)

(test/exn (interp (parse 'x) empty)
          "Unbound")
(test (interp (parse '(with (x (+ 1 1)) (* x x))) empty)
      4)
(test (interp (parse '(with (x 1) (with (y 2) (+ x y)))) empty)
      3)

(test (subst 'x (parse '1) (parse '(with (y 2) (+ x y))))
      (parse '(with (y 2) (+ 1 y))))

(test (interp (parse '(with (y 2) (+ 1 y))) empty)
      3)

;; This tells if we are substituting text or not:
(test (interp (parse '(with (y x) 3)) empty)
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



(test (parse '(foo 1)) (app 'foo (num 1)))

(test (interp (parse '(double 3)) (list (fundef 'double 'x (add (id 'x) (id 'x)))))
      6)
(test (interp (parse '(double (+ 3 2))) (list (fundef 'double 'x (add (id 'x) (id 'x)))))
      10)
(test (interp (app 'double (add (num 3) (num 2))) (list (fundef 'double 'x (add (id 'x) (id 'x)))))
      10)
(test (local [(define fun-def (lookup-fundef 'double (list (fundef 'double 'x (add (id 'x) (id 'x))))))]
           (interp (subst (fundef-arg-name fun-def)
                          (num (interp (add (num 3) (num 2)) (list (fundef 'double 'x (add (id 'x) (id 'x))))))
                          (fundef-body fun-def))
                   (list (fundef 'double 'x (add (id 'x) (id 'x))))))
      10)
(test (local [(define fun-def (fundef 'double 'x (add (id 'x) (id 'x))))]
           (interp (subst (fundef-arg-name fun-def)
                          (num (interp (add (num 3) (num 2)) (list (fundef 'double 'x (add (id 'x) (id 'x))))))
                          (fundef-body fun-def))
                   (list (fundef 'double 'x (add (id 'x) (id 'x))))))
      10)
(test (local [(define fun-def (fundef 'double 'x (add (id 'x) (id 'x))))]
           (interp (subst (fundef-arg-name fun-def)
                          (num (interp (add (num 3) (num 2)) (list (fundef 'double 'x (add (id 'x) (id 'x))))))
                          (fundef-body fun-def))
                   (list (fundef 'double 'x (add (id 'x) (id 'x))))))
      10)
(test (local [(define fun-def (fundef 'double 'x (add (id 'x) (id 'x))))]
           (interp (subst 
                    'x
                    (num 5)
                    (add (id 'x) (id 'x)))
                   (list (fundef 'double 'x (add (id 'x) (id 'x))))))
      10)
(test (local [(define fun-def (fundef 'double 'x (add (id 'x) (id 'x))))]
           (interp (add (num 5) (num 5))
                   (list (fundef 'double 'x (add (id 'x) (id 'x))))))
      10)
(test (local [(define fun-def (fundef 'double 'x (add (id 'x) (id 'x))))]
           10)
      10)

(test (interp (parse '(f 5)) (list (fundef 'f 'n (app 'g (add (id 'n) (num 5))))
                                   (fundef 'g 'm (add (id 'm) (num 1)))))
      11)


(interp (parse '(f 5)) (list (fundef 'f 'n (app 'g (add (id 'n) (num 5))))
                                   (fundef 'g 'm (add (id 'n) (num 1)))))
