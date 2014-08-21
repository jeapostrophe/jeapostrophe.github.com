#lang plai
(halt-on-errors)

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

;; An environment or Deferred Substitution
(define-type DefrdSubst
  [mtSub]
  [aSub (name symbol?)
        (named-value number?)
        (all-the-others DefrdSubst?)])

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
    (error 'parse "Bad programmer, no cake ~e" c)]))

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

;; lookup-binding : symbol? DefrdSub -> AE?
(define (lookup-binding s ds)
  (type-case
   DefrdSubst ds
   [mtSub ()
          (error 'lookup-binding "Unbound identifier ~e" s)]
   [aSub (name named-value more)
         (if (symbol=? s name)
             named-value
             (lookup-binding s more))]))

;; interp* : AE (listof FunDef) DefrdSub -> meaning
(define (interp* some-ae fun-defs ds)
  (type-case
   AE some-ae
   [num (n)
        n]
   [add (lhs rhs)
        (+ (interp* lhs fun-defs ds)
           (interp* rhs fun-defs ds))]
   [mult (lhs rhs)
         (* (interp* lhs fun-defs ds)
            (interp* rhs fun-defs ds))]
   [id (s)
       (lookup-binding s ds)]
   [with (i v e)
         (interp* e
                  fun-defs
                  (aSub i
                        (interp* v fun-defs ds)
                        ds))]
   [app (fun-name arg-expr)
        (local [(define fun-def (lookup-fundef fun-name fun-defs))]
               (interp* (fundef-body fun-def)
                        fun-defs
                        (aSub (fundef-arg-name fun-def)
                              (interp* arg-expr fun-defs ds)
                              ;; Evil = Dynamic Scope
                              ;ds
                              ;; Correct = Static Scope
                              (mtSub))))]))

(define (interp ae fundefs)
  (interp* ae fundefs (mtSub)))

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
(test (interp (parse '(with (x 1) (with (y x) (+ x y)))) empty)
      2)

(test (subst 'x (parse '1) (parse '(with (y 2) (+ x y))))
      (parse '(with (y 2) (+ 1 y))))

(test (interp (parse '(with (y 2) (+ 1 y))) empty)
      3)

;; This tells if we are substituting text or not:
(test/exn (interp (parse '(with (y x) 3)) empty)
          "Unbound")

(test (subst 'x (parse '1) (parse '(+ x x)))
      (parse '(+ 1 1)))
(test (subst 'x (parse '1) (parse '(with (x 2) x)))
      (parse '(with (x 2) x)))
(test (subst 'x (parse '1) (parse '(with (x 2) x)))
      (parse '(with (x 2) x)))
(test (subst 'x (parse '1) (parse '(with (y 2) x)))
      (parse '(with (y 2) 1)))
(test (subst 'x (parse '1) (parse '(with (y x) x)))
      (parse '(with (y 1) 1)))
(test (subst 'x (parse '1) (parse '(with (x x) x)))
      (parse '(with (x 1) x)))

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

;(test (interp (parse '(f 5)) (list (fundef 'f 'n (app 'f (add (id 'n) (num 5))))))
;      11)

;; f(n) = g(n+5)
;; f(f) = g(f+5)
(test (interp (parse '(f 5)) (list (fundef 'f 'f (app 'g (add (id 'f) (num 5))))
                                   (fundef 'g 'm (add (id 'm) (num 1)))))
      11)

;; induction vs co-induction
;; recursion vs co-recursion

;; Lisp1 vs (we are Lisp2)

(test (interp (parse '(with (x 5)
                            (+ (+ x x)
                               (* x x))))
              empty)
      35)

(test (interp (parse
               ;; ds = mt
               '(with (x 5)
                      ;; ds = x -> 5 :: mt
                      (+
                       ;; ds = x -> 5 :: mt
                       (with (x 10)
                             ;; ds = x -> 10 :: x -> 5 :: mt
                             (+ x x))
                       ;; ds = x -> 5 :: mt
                       (* x x))))
              empty)
      45)

(test (interp (parse
               '(with (x 5)
                      (+
                       (with (x x)
                             (+ x x))
                       (* x x))))
              empty)
      35)

(test (interp (parse '(+ (with (x 5)
                               x)
                         (with (x 7)
                               x)))
              empty)
      12)


;; f(n) = g(n+5)
;; g(m) = m+1
;; f(5)
(test (interp (parse '(f 5)) (list (fundef 'f 'n (app 'g (add (id 'n) (num 5))))
                                   (fundef 'g 'm (add (id 'm) (num 1)))))
      11)

;; f(n) = g(n+5)
;; g(m) = n+1
;; f(5)
(test/exn (interp (parse '(g 5)) (list (fundef 'g 'm (add (id 'n) (num 1)))))
          "Unbound identifier")
(test/exn (interp (parse '(with (n 10) (g 5))) (list (fundef 'g 'm (add (id 'n) (num 1)))))
          "Unbound identifier")
(test/exn (interp (parse '(f 5)) (list (fundef 'f 'n (app 'g (add (id 'n) (num 5))))
                                       (fundef 'g 'm (add (id 'n) (num 1)))))
          "Unbound identifier")


