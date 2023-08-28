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
  [fun (name symbol?)
       (body AE?)]
  [app (fun-expr AE?) 
       (arg AE?)])


(define-type AEv
  [numV (n number?)]
  [closureV (name symbol?)
            (body AE?)
            (env DefrdSubst?)])

;; An environment or Deferred Substitution
(define-type DefrdSubst
  [mtSub]
  [aSub (name symbol?)
        (named-value AEv?)
        (all-the-others DefrdSubst?)])

;; concrete syntax
#|
AE = <number>
   | (* <AE> <AE>)
   | (+ <AE> <AE>)
   | (with (<id> <AE>) <AE>)
   | <id>
   | (fun (<id>) <AE>)
   | (<AE> <AE>)
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
         (= 3 (length c))
         (equal? 'fun (first c)))
    (fun (first (second c))        
         (parse (third c)))]
   [(and (list? c)
         (= 2 (length c)))
    (app (parse (first c)) (parse (second c)))]
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
(test (parse '(fun (x) (+ x x)))
      (fun 'x (add (id 'x) (id 'x))))

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
   [fun (parameter body)
        (if (equal? i parameter)
            (fun parameter body)
            (fun parameter (subst i v body)))]        
    [app (fun-expr arg-expr)
         (app (subst i v fun-expr)
              (subst i v arg-expr))]))

(test (subst 'x (num 5)
             (parse '(fun (x) (+ x x))))
      (parse '(fun (x) (+ x x))))
(test (subst 'y (num 5)
             (parse '(fun (x) (+ x y))))
      (parse '(fun (x) (+ x 5))))
(test (subst 'y (num 5)
             (parse '((fun (x) (+ x y)) 10)))
      (parse '((fun (x) (+ x 5)) 10)))

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

(define (lift binop lhs rhs)
  (type-case
   AE lhs
   [num (lhsn)
        (type-case
         AE rhs
         [num (rhsn)
              (num (binop lhsn rhsn))]
         [else
          (error 'interp "Not a number")])]
   [else
    (error 'interp "Not a number")]))

(define (liftV binop lhs rhs)
  (type-case
   AEv lhs
   [numV (lhsn)
         (type-case
          AEv rhs
          [numV (rhsn)
                (numV (binop lhsn rhsn))]
          [else
           (error 'interp "Not a number")])]
   [else
    (error 'interp "Not a number")]))

;; interp/subst : AE -> number
(define (interp/subst some-ae)
  (type-case
   AE some-ae
   [num (n)
        (num n)]
   [add (lhs rhs)
        (lift + (interp/subst lhs)
              (interp/subst rhs))]
   [mult (lhs rhs)
         (lift * (interp/subst lhs)
               (interp/subst rhs))]
   [id (s)
       (error 'interp "Unbound identifier ~e" s)]
   [with (i v e)
         (interp/subst
          (subst i
                 (interp/subst v)
                 e))]
   [fun (arg-name fun-body)
        (fun arg-name fun-body)]
   [app (fun-expr arg-expr)
        (type-case
         AE (interp/subst fun-expr)
         [fun (arg-name fun-body)
              (interp/subst
                (subst arg-name
                       (interp/subst arg-expr)
                       fun-body))]
         [else
          (error 'interp "Not a function")])]))

;; interp* : AE DefrdSub -> meaning
(define (interp* some-ae ds)
  (type-case
   AE some-ae
   [num (n)
        (numV n)]
   [add (lhs rhs)
        (liftV + (interp* lhs ds)
           (interp* rhs ds))]
   [mult (lhs rhs)
         (liftV * (interp* lhs ds)
            (interp* rhs ds))]
   [id (s)
       (lookup-binding s ds)]
   [with (i v e)
         (interp* e
                  (aSub i
                        (interp* v ds)
                        ds))]
   [fun (arg-name fun-body)
        (closureV arg-name fun-body ds)]
   [app (fun-expr arg-expr)
        (type-case
         AEv (interp* fun-expr ds)
         [closureV (arg-name fun-body saved-env)
                   (interp* fun-body
                            (aSub arg-name
                                  (interp* arg-expr ds)
                                  saved-env))]
         [else
          (error 'interp "Not a function")])]))

(define (interp** ae)
  (define ans
    (interp* ae (mtSub)))
  (type-case
   AEv ans
   [numV (n) n]
   [else ans]))

(define (interp ae)
  #;(define ans (interp/subst ae))
  #;(type-case AE ans
             [num (n) n]
             [else ans])
  (interp** ae))

(test/exn (interp (parse '(5 4)))
          "Not a function")
  
(test (interp/subst (parse '(fun (x) x)))
      (parse '(fun (x) x)))

  
(test (interp (parse '5))
      5)
(test (interp (parse '42))
      42)

(test (interp (parse '(+ 1 1)))
      2)
(test (interp (parse '(+ 1 99)))
      100)

(test (interp (parse '(* 3 2)))
      6)
(test (interp (parse '(* 1/99 99)))
      1)

(test  (interp (parse '(+ (+ (+ (+ 1 1) (+ 1 1)) (+ (+ 1 1) (+ 1 1))) (+ 1 1))))
      10)

(test (interp (parse '(with (x (+ 1 1)) (+ x x))))
      4)
(test (interp (parse '(with (x 2) (+ x x))))
      4)
(test (interp (parse '(with (x 2) (+ 2 x))))
      4)
(test (interp (parse '(with (x 2) (+ 2 2))))
      4)
(test (interp (parse '(+ 2 2)))
      4)

(test/exn (interp (parse 'x))
          "Unbound")
(test (interp (parse '(with (x (+ 1 1)) (* x x))))
      4)
(test (interp (parse '(with (x 1) (with (y 2) (+ x y)))))
      3)
(test (interp (parse '(with (x 1) (with (y x) (+ x y)))))
      2)

(test (subst 'x (parse '1) (parse '(with (y 2) (+ x y))))
      (parse '(with (y 2) (+ 1 y))))

(test (interp (parse '(with (y 2) (+ 1 y))))
      3)

;; This tells if we are substituting text or not:
(test/exn (interp (parse '(with (y x) 3)))
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

(test (parse '(foo 1)) (app (id 'foo) (num 1)))

(test (interp (parse '(with (double (fun (x) (+ x x)))
                            (double 3))))
      6)
(test (interp (parse '(with (double (fun (x) (+ x x)))
                            (double (+ 3 2)))))
      10)

(test (interp** (parse '(with (g (fun (m) (+ m 1)))
                              (with (f (fun (n) (g (+ n 5))))
                                    (f 5)))))
      11)

;; induction vs co-induction
;; recursion vs co-recursion

;; Lisp1 vs (we are Lisp2)

(test (interp (parse '(with (x 5)
                            (+ (+ x x)
                               (* x x)))))
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
                       (* x x)))))
      45)

(test (interp (parse
               '(with (x 5)
                      (+
                       (with (x x)
                             (+ x x))
                       (* x x)))))
      35)

(test (interp (parse '(+ (with (x 5)
                               x)
                         (with (x 7)
                               x))))
      12)


;; f(n) = g(n+5)
;; g(m) = n+1
;; f(5)
(test/exn (interp (parse '(with (g (fun (m) (+ n 1)))
                                (g 5))))
          "Unbound identifier")
(test/exn (interp** (parse '(with (g (fun (m) (+ n 1)))
                                (with (n 10) (g 5)))))
          "Unbound identifier")

(test (interp (parse '(with (x 5) (+ x x))))
      10)
;; f(x) = x + x; f(5)
;; kaizersozay(x) = x + x; kaizersozay(5)

(test (interp (parse '((fun (x) (+ x x)) 5)))
      10)

(test (interp/subst (parse '(with (y 5) (fun (x) (+ x y)))))
      (parse '(fun (x) (+ x 5))))
(test (interp/subst (parse '(with (y (+ 1 4)) (fun (x) (+ x y)))))
      (parse '(fun (x) (+ x 5))))

;; Capturing avoiding substiutiton is the correct model
;; for function and subst. But it is very complicated.

(test/exn (interp (parse '(with (f (fun (n) (+ m n)))
                                (with (m 5)
                                      (f 10)))))
          "Unbound identifier")
