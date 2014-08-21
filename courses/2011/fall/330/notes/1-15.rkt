#lang plai
(halt-on-errors)

;; abstract syntax
(define-type AE
  [num (n number?)]
  [binop (op procedure?)
         (lhs AE?)
         (rhs AE?)]
  [if0 (cond-e AE?)
       (true-e AE?)
       (false-e AE?)]
  #;[rec (name symbol?)
       (named-expr AE?)
       (body AE?)]
  [id (s symbol?)]
  [fun (arg-name symbol?)
       (body AE?)]
  [app (fun-expr AE?) 
       (arg AE?)])

(define (rec name named-value body)
  ;; XXX Safely pick unique names (gensym)
  (with 'the-real-work
        (fun name
             named-value)
        (with 'inner-fac
              (fun 'the-real-fac
                   (app (id 'the-real-work)
                        (fun 'n 
                             (app (app (id 'the-real-fac) (id 'the-real-fac)) (id 'n)))))
              (with name
                    (fun 'n
                         (app (app (id 'inner-fac) (id 'inner-fac)) (id 'n)))
                    body))))


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
  [closureV (name symbol?)
        (body AE?)
        (env DefrdSubst?)])

;; concrete syntax
#|
AE = <number>
   | (* <AE> <AE>)
   | (+ <AE> <AE>)
   | (- <AE> <AE>)
   | (with (<id> <AE>) <AE>)
   | (fun (<id>) <AE>)
   | <id>
   | (<AE> <AE>)
   | (if0 <AE> <AE> <AE>)
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
         (equal? '- (first c)))
    (binop -
           (parse (second c))
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
         (equal? 'rec (first c)))
    (rec (first (second c))
         (parse (second (second c)))
         (parse (third c)))]
   [(and (list? c)
         (= 3 (length c))
         (equal? 'fun (first c)))
    (fun (first (second c))
         (parse (third c)))]
   [(and (list? c)
         (= 4 (length c))
         (equal? 'if0 (first c)))
    (if0 (parse (second c))
         (parse (third c))
         (parse (fourth c)))]
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

(define (lifted op . args)
  (apply op (map denum args)))

(define (denum v)
  (type-case
   AEV v
   [numV (n)
         n]
   [else
    (error 'interp "Not a number")]))

(define (lift binop lhs rhs)
  (numV (lifted binop lhs rhs)))

;; The right env is...
;; |------|
;; | fac  |____> wrong
;; |  = 0 |
;; |------|

;; The right value is...
;; (closureV 'n (code ....)
;; |------|
;; | fac  |____> wrong
;; |  = 0 |
;; |------|
;; )

;; |---------|
;; | fac     |____> wrong
;; |  = clsr |
;; |---------|

;; The right value is...
;; (closureV 'n (code ....)
;; |---------|
;; | fac     |____> wrong
;; |  = clsr |
;; |---------|
;; )

(define (bind-and-evaluate-cyclically name named-value the-wrong-env)
  (define the-wrong-value
    (numV 0))
  (define the-right-env ;; (almost... at least it willl be)
    (anEnv name
           the-wrong-value
           the-wrong-env))
  (define the-right-value
    (interp* named-value
             the-right-env))
  (set-anEnv-named-value! the-right-env
                          the-right-value)
  the-right-env)

;; interp* : AE DefrdSub -> meaning
(define (interp* some-ae ds)
  (type-case
   AE some-ae
   [num (n)
        (numV n)]
   [binop (op lhs rhs)
        (lift op
              (interp* lhs ds)
              (interp* rhs ds))]
   [id (s)
       (lookup-binding s ds)]
   [if0 (cond-e true-e false-e)
        (if (lifted zero? (interp* cond-e ds))
            (interp* true-e ds)
            (interp* false-e ds))]
   #;[rec (name named-value body-expr)

        (local [(define the-wrong-env
                  ds)
                (define the-right-env
                  (bind-and-evaluate-cyclically
                   name named-value
                   the-wrong-env))]
               (interp* body-expr
                        the-right-env))

        ]
   [fun (arg-name body-expr)
        ;; Save the Environment
        ;; Create A Closure, Today!
        (closureV arg-name body-expr ds)]
   [app (fun-expr arg-expr)
        ;; interp* : AE ds -> number
        ;; fun-expr : AE
        (type-case
         AEV (interp* fun-expr ds)
         [closureV (arg-name fun-body saved-env)
              (interp* fun-body
                       (anEnv arg-name
                             (interp* arg-expr ds)
                             ;; Evil = Dynamic Scope
                             #;ds
                             ;; Correct = Static Scope
                             saved-env))]
         [else
          (error 'interp "Not a function")])]))

(define (interp ae)
  (define ans
    (interp* ae (mtEnv)))
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
(test/exn (interp (parse '(with (y x) 3)))
          "Unbound")

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

(test (interp (parse '(if0 0 1 2)))
      1)
(test (interp (parse '(if0 1 1 2)))
      2)

(test/exn (interp (parse '(with (fac
                             (fun (n)
                                  (if0 n
                                       1
                                       (* n (fac (- n 1))))))
                            (fac 4))))
          "Unbound identifier")

(define fac-of-N
  '(rec (fac
         (fun (n)
              (if0 n
                   1
                   (* n (fac (- n 1))))))
        (fac N)))
  
(test (interp (parse `(with (N 0) ,fac-of-N)))
      1)
(test (interp (parse `(with (N 1) ,fac-of-N)))
      1)
(test (interp (parse `(with (N 2) ,fac-of-N)))
      2)
(test (interp (parse `(with (N 4) ,fac-of-N)))
      24)
(test (interp (parse `(with (N 5) ,fac-of-N)))
      120)

#;(test (interp (parse '(rec (x (+ x 1))
                            x)))
      1)

(test (interp (parse '(fun (x) (x x))))
      (closureV 'x (app (id 'x) (id 'x))
                (mtEnv)))

;;(test (interp (parse '((fun (x) (x x))
;;                       (fun (x) (x x)))))
;;      42)

(test (interp (parse '(with (fac
                             (fun (the-real-fac)
                                  (fun (n)
                                       (if0 n
                                            1
                                            (* n ((the-real-fac the-real-fac) (- n 1)))))))
                            ((fac fac) 4))))
      24)
(test (interp (parse '(with (fac
                             (fun (the-real-fac)
                                  (fun (n)
                                       (if0 n
                                            1
                                            (* n ((the-real-fac the-real-fac) (- n 1)))))))
                            ((fac fac) 5))))
      120)


(test (interp (parse '(with (the-real-work
                             (fun (fac)
                                  (fun (n)
                                       (if0 n
                                            1
                                            (* n (fac (- n 1)))))))
                            (with (inner-fac
                                   (fun (the-real-fac)
                                        (the-real-work
                                         (fun (n)
                                              ((the-real-fac the-real-fac) n)))))
                                  (with (fac
                                         (fun (n)
                                              ((inner-fac inner-fac) n)))
                                        (fac 5))))))
      120)

(test (interp (parse '(rec (fac
                            (fun (n)
                                 n))
                           the-real-work)))
      120)

;; Y Combinator
(test/exn (interp (parse '(with (Y homework)
                            (with (fac
                                   (Y
                                    (fun (fac)
                                         (fun (n)
                                              (if0 n
                                                   1
                                                   (* n (fac (- n 1))))))))
                                  (fac 5)))))
      "Unbound identifier")
