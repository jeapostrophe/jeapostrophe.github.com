#lang plai
(halt-on-errors)

;; abstract syntax
(define-type AE
  [num (n number?)]
  [binop (op procedure?)
         (lhs AE?)
         (rhs AE?)]
  [id (s symbol?)]
  [fun (arg-name symbol?)
       (body AE?)]
  [app (fun-expr AE?) 
       (arg AE?)])

(define (with name named-thing body)
  (app (fun name body) named-thing))

(define (add lhs rhs)
  (binop + lhs rhs))
(define (mult lhs rhs)
  (binop * lhs rhs))

;; An environment or Deferred Substitution
(define-type DefrdSubst
  [mtEnv]
  [anEnv (name symbol?)
        (named-value AEV?)
        (all-the-others DefrdSubst?)])

(define-type AEV
  [numV (n number?)]
  [delayedV (expr AE?)
            (env DefrdSubst?)
            (ans (or/c false/c AEV?))]
  [closureV (name symbol?)
        (body AE?)
        (env DefrdSubst?)])

;; concrete syntax
#|
AE = <number>
   | (* <AE> <AE>)
   | (+ <AE> <AE>)
   | (with (<id> <AE>) <AE>)
   | (fun (<id>) <AE>)
   | <id>
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
    (app (parse (first c))
         (parse (second c)))]
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
(test (parse '(fun (x) x))
      (fun 'x (id 'x)))
(test (parse '((fun (x) x) 5))
      (app (fun 'x (id 'x)) (num 5)))

;; lookup-binding : symbol? DefrdSub -> AE?
(define (lookup-binding s ds)
  (type-case
   DefrdSubst ds
   [mtEnv ()
          (error 'lookup-binding "Unbound identifier ~e" s)]
   [anEnv (name named-value more)
         (if (symbol=? s name)
             named-value
             (lookup-binding s more))]))

(define (lift binop lhs rhs)
  (type-case
   AEV lhs
   [numV (lhs-v)
        (type-case
         AEV rhs
         [numV (rhs-v)
               (begin (printf "Adding ~a and ~a\n" lhs-v rhs-v)
                      (numV (binop lhs-v rhs-v)))]
         [else
          (error 'interp "Not a number")])]
   [else
    (error 'interp "Not a number")]))

;; strict : AEV -> AEV (but not a DelayedV)
(define (strict v)
  (type-case
   AEV v
   [numV (n)
         v]
   [closureV (name body env)
             v]
   [delayedV (expr env ans)
             (if ans
                 ans
                 (local [(define the-right-ans (strict (interp* expr env)))]
                        (set-delayedV-ans! v the-right-ans)
                        the-right-ans))]))

;; interp* : AE DefrdSub -> AEV
(define (interp* some-ae ds)
  (type-case
   AE some-ae
   [num (n)
        (numV n)]
   [binop (op lhs rhs)
        (lift op
              (strict (interp* lhs ds))
              (strict (interp* rhs ds)))]
   [id (s)
       (lookup-binding s ds)]
   [fun (arg-name body-expr)
        ;; Save the Environment
        ;; Create A Closure, Today!
        (closureV arg-name body-expr ds)]
   [app (fun-expr arg-expr)
        ;; interp* : AE ds -> number
        ;; fun-expr : AE
        (type-case
         AEV (strict (interp* fun-expr ds))
         [closureV (arg-name fun-body saved-env)
              (interp* fun-body
                       (anEnv arg-name
                             (delayedV arg-expr ds #f)
                             ;; Evil = Dynamic Scope
                             #;ds
                             ;; Correct = Static Scope
                             saved-env))]
         [else
          (error 'interp "Not a function")])]))

(define (interp ae)
  (define ans
    (strict (interp* ae (mtEnv))))
  (type-case
   AEV ans
   [numV (n) n]
   [else ans]))

(test/exn (interp (parse '(5 4)))
          "Not a function")
(test/exn (interp (parse '(+ (fun (x) x) 1)))
          "Not a number")      

(test (interp (parse '(fun (x) x)))
      (closureV 'x (id 'x) (mtEnv)))

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

(test (interp (parse '(+ (+ (+ (+ 1 1) (+ 1 1)) (+ (+ 1 1) (+ 1 1))) (+ 1 1))))
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
(test (interp (parse '(with (x 1) (with (x x) (+ x x)))))
      2)

(test (interp (parse '(with (y 2) (+ 1 y))))
      3)

;; This tells if we are substituting text or not:
(test (interp (parse '(with (y x) 3)))
      ;; Lazy
      3
      ;; Eager
      #;"Unbound")

(test (parse '(foo 1)) (app (id 'foo) (num 1)))
(test (interp (parse '(with (double (fun (x) (+ x x)))
                            (double 3))))
      6)
(test (interp (parse '(with (double (fun (x) (+ x x)))
                            (double (+ 3 2)))))
      10)

(test (interp (parse '(with (g 10)
                            (with (f (fun (n) (+ g 5)))
                                  (f 5)))))
      15)
(test (interp (parse '(with (g (fun (m) (+ m 1)))
                            (with (f (fun (n) (g (+ n 5))))
                                  (f 5)))))
      11)

;(test (interp (parse '(f 5)) (list (fundef 'f 'n (app 'f (add (id 'n) (num 5))))))
;      11)

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
(test/exn (interp (parse '(with (g (fun (m) (+ n 1)))
                                (with (n 10) (g 5)))))
          "Unbound identifier")

(test (interp (parse '(with (x 5) (+ x x))))
      10)

;; f(x) = x + x; f(5)
;; Voldemort(x) = x+x; Voldemort(5)
;; (\ x. x + x) 5
(test (interp (parse '((fun (x) (+ x x)) 5)))
      10)

(test (interp (parse '(with (x (+ 1 (fun (x) x)))
                            3)))
      ;; Lazy
      3
      ;; Eager
      ;"Not a number"
      )
      
(test (interp (parse '(with (x 22)
                            x)))
      22)
(test (interp (parse '(with (x (+ 11 11))
                            x)))
      22)
(test (interp (parse '(with (x (+ 11 11))
                            (+ x x))))
      44)
