#lang plai-typed
(print-only-errors #t)

;; ExprC =
;; | num
;; | (+ ExprC ExprC)
;; | (* ExprC ExprC)
;; | id
;; | (id ExprC)
;; | (λ (id) ExprC)

(define-type ExprC
  [numC (val : number)]
  [addC (lhs : ExprC) (rhs : ExprC)]
  [multC (lhs : ExprC) (rhs : ExprC)]
  [idC (id : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [lamC (arg : symbol) (body : ExprC)])

;; parse : s-exp -> ExprC
(define (parse se)
  (cond
    [(s-exp-number? se)
     (numC (s-exp->number se))]
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 3)
          (equal? '+ (s-exp->symbol (first (s-exp->list se)))))
     (addC
      (parse (second (s-exp->list se)))
      (parse (third (s-exp->list se))))]
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 3)
          (equal? '* (s-exp->symbol (first (s-exp->list se)))))
     (multC
      (parse (second (s-exp->list se)))
      (parse (third (s-exp->list se))))]
    [(s-exp-symbol? se)
     (idC (s-exp->symbol se))]
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 2))
     (appC
      (parse (first (s-exp->list se)))
      (parse (second (s-exp->list se))))]
    ;; | (λ (id) ExprC)
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 3)
          (symbol=? (s-exp->symbol (first (s-exp->list se))) 'λ)
          (s-exp-list? (second (s-exp->list se))))
     (lamC (s-exp->symbol (first (s-exp->list (second (s-exp->list se))))) (parse (third (s-exp->list se))))]
    [else
     (begin
       (display se)
       (error 'parse "You are wrong"))]))

(test (parse '(λ (x) (+ x 1)))
      (lamC 'x (addC (idC 'x) (numC 1))))
(test (parse '((λ (x) (+ x x)) 13))
      (appC (lamC 'x (addC (idC 'x) (idC 'x))) (numC 13)))

(test (parse '23)
      (numC 23))
(test (parse '(+ 23 5))
      (addC (numC 23) (numC 5)))
(test (parse '(+ 23 (+ 23 5)))
      (addC (numC 23)
            (addC (numC 23) (numC 5))))
(test (parse '(* 23 5))
      (multC (numC 23) (numC 5)))
(test (parse (symbol->s-exp 'x))
      (idC 'x))

(define-type Value
  [numV (n : number)]
  [closV (f : (Value -> Value))])

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (symbol -> Value))
(define mt-env
  (λ (id)
    (error 'lookup "undefined identifier")))
(define (extend-env new-binding old-env)
  (λ (id)
    (cond
      [(symbol=? id (bind-name new-binding))
       (bind-val new-binding)]
      [else
       (lookup id old-env)])))

;; lookup: symbol Env -> number
;; lookup (in Python): Env symbol -> Value
(define (lookup id env)
  (env id))

(test/exn (lookup 'x mt-env) "undefined")
(test (lookup 'x (extend-env (bind 'x (numV 5)) mt-env))
      (numV 5))

;; interp : ExprC (listof FunDefC) (listof Binding) -> number
(define (interp p env)
  (type-case
   ExprC p
   [numC
    (val)
    (numV val)]
   [addC
    (lhs rhs)
    (numV
     (+ (numV-n (interp lhs env))
        (numV-n (interp rhs env))))]
   [multC
    (lhs rhs)
    (numV
     (* (numV-n (interp lhs env))
        (numV-n (interp rhs env))))]
   [idC
    (id)
    (lookup id env)]
   [lamC
    (arg body)
    (closV
     ;; Racket Closure = { body, arg, env }
     ;; This is OOP!!!!!!!!
     (λ (arg-value)
       (interp body
               (extend-env
                (bind arg arg-value)
                env))))]
   [appC
    (fun arg)
    (local
     [(define f (interp fun env))]
     ((closV-f f) (interp arg env)))]))

;; Testing begins here!

;; interp* : s-exp -> number
(define (interp* s)
  (interp (parse s) mt-env))

(test (interp* '23)
      (numV 23))
(test (interp* '(+ 23 5))
      (numV 28))
(test (interp* '(+ 23 (+ 23 5)))
      (numV 51))
(test (interp* '(* 23 5))
      (numV 115))
(test (interp* '((λ (x) 5) 13))
      (interp* '5))
(test (interp* '((λ (x) (+ x x)) 13))
      (interp* '(+ 13 13)))
(test (interp* '(((λ (x)
                    (λ (y)
                      (+ y 2)))
                  4)
                 5))
      (numV 7))
(test (interp* '(((λ (x)
                    (λ (y)
                      (+ y x)))
                  4)
                 5))
      (numV 9))

(test/exn (interp* '(+ x 5))
          "undefined")

;; f(5) = ...some list...
;; f'(5) = ... add an element here and change this to that ...
;; f(6) = ...some other list...
;; Incremental Computation

;; What is the interface that numbers are?

;; (foldr f a (cons 1 (cons 2 empty)))
;; =
;; (f 1 (f 2 a))

(define ZERO '(λ (add1) (λ (zero) zero)))
(define ONE '(λ (add1) (λ (zero) (add1 zero))))
(define TWO '(λ (add1) (λ (zero) (add1 (add1 zero)))))

(test (interp* `((,ZERO (λ (x) (+ x 1))) 0))
      (numV 0))
(test (interp* `((,ONE (λ (x) (+ x 1))) 0))
      (numV 1))
(test (interp* `((,TWO (λ (x) (+ x 1))) 0))
      (numV 2))

;; (define (append x y) (foldr cons y x))

(define PLUS
  '(λ (fst-num)
     (λ (snd-num)
       (λ (add1)
         (λ (zero)
           ((snd-num add1)
            ((fst-num add1) zero)))))))
(test (interp* `((((,PLUS ,ONE) ,TWO) (λ (x) (+ x 1))) 0))
      (numV 3))

(define MULT
  '(λ (fst-num)
     (λ (snd-num)
       (λ (add1)
         (λ (zero)           
           ((snd-num
             (λ (other-num)
               ((fst-num add1) other-num)))
            zero))))))
(test (interp* `((((,MULT ,TWO) ,TWO) (λ (x) (+ x 1))) 0))
      (numV 4))
(test (interp* `((((,MULT ,ZERO) ,TWO) (λ (x) (+ x 1))) 0))
      (numV 0))
(test (interp* `((((,MULT ,TWO) ,ZERO) (λ (x) (+ x 1))) 0))
      (numV 0))

;; iszero? 0 = 55
;; iszero? 1 = run forever
(test (interp* `((,ZERO
                  (λ (some-num)
                    ((λ (x) (x x))
                     (λ (x) (x x)))))
                 55))
      (numV 55))
(test (interp* `((,ONE
                  (λ (ignored)
                    42))
                 55))
      (numV 42))
(test (interp* `((,TWO
                  (λ (ignored)
                    42))
                 55))
      (numV 42))
