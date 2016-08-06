#lang plai-typed
(print-only-errors #t)


#|
(define (double x) (+ x x))
(define (quadruple x) (double (double x)))
(define (const5 _) 5)
|#

#;(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

;; ExprC =
;; | num
;; | (+ ExprC ExprC)
;; | (* ExprC ExprC)
;; | id
;; | (id ExprC)
;; | (λ (id) ExprC)


;; 4 + 5 = 9
;; x + 2 = x + 2
;; f(x) = x + 2
;; g(z) = z(3)
;; g(f) => f(3) => 5

(define-type ExprC
  [numC (val : number)]
  [addC (lhs : ExprC) (rhs : ExprC)]
  [multC (lhs : ExprC) (rhs : ExprC)]
  [idC (id : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  )


(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])


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
#;(test (parse '(double 13))
      (appC 'double (numC 13)))

;; subst : ExprC symbol ExprC -> ExprC
#;(define (subst what for in)
  (type-case
      ExprC in
    [numC
     (val)
     in]
    [addC
     (lhs rhs)
     (addC (subst what for lhs)
           (subst what for rhs))]
    [multC
     (lhs rhs)
     (multC (subst what for lhs)
            (subst what for rhs))]
    [idC
     (id)
     (cond [(symbol=? id for)  what]
           [else  in])]
    [appC
     (fun arg)
     (appC fun (subst what for arg))
     ]))
#|
(test (subst (numC 13) 'x (idC 'x))
      (numC 13))
(test (subst (numC 13) 'x (idC 'y))
      (idC 'y))
(test (subst (numC 13) 'x (numC 5))
      (numC 5))
(test (subst (numC 13) 'x (addC (idC 'x) (idC 'x)))
      (addC (numC 13) (numC 13)))
(test (subst (numC 13) 'x (addC (idC 'x) (idC 'y)))
      (addC (numC 13) (idC 'y)))
(test (subst (numC 13) 'x (addC (idC 'x) (appC 'double (idC 'x))))
      (addC (numC 13) (appC 'double (numC 13))))
(test (subst (numC 13) 'x (multC (idC 'x) (idC 'x)))
      (multC (numC 13) (numC 13)))
(test (subst (numC 13) 'x (appC 'double (idC 'x)))
      (appC 'double (numC 13)))
(test (subst (numC 13) 'x (appC 'double (appC 'double (idC 'x))))
      (appC 'double (appC 'double (numC 13))))
;; get-fundef : symbol (listof FunDefC) -> FunDefC
(define (get-fundef name fds)
  (cond [(empty? fds)  (error 'get-fundef "unknown function")]
        [(symbol=? name (fdC-name (first fds)))  (first fds)]
        [else  (get-fundef name (rest fds))]))
|#

;; (appC 'double (numC 3))
;; fun = 'double
;; arg = (numC 3)
;; ans = 6
;; fd = double (addC x x) x
;; env = empty

;; lookup: symbol Env -> number
;; 'x empty -> error 
;; 'x '((bind 'x 5))
;; and = 5
(define (lookup id env)
  (cond
    [(empty? env) (error 'lookup "undefined identifier")]
    [else
     (cond
       [(symbol=? id (bind-name (first env))) 
        (bind-val (first env))]
       [else 
        (lookup id (rest env))])]))
         

(test/exn (lookup 'x empty) "undefined")
(test (lookup 'x (list (bind 'x (numV 5)))) (numV 5))

;; interp : ExprC (listof FunDefC) (listof Binding) -> number
(define (interp p env)
  (type-case
      ExprC p
    [numC
     (val)
     (numV val)]
    [addC
     (lhs rhs)
     (numV (+ (numV-n (interp lhs env)) (numV-n (interp rhs env))))]
    [multC
     (lhs rhs)
     (numV (* (numV-n (interp lhs env)) (numV-n (interp rhs env))))]
    [idC
     (id)
     (lookup id env)]
    #;'(((fun f1 x 
              (fun f2 y 
                   (+ y 2))) 4) 5)
    [appC
     (fun arg)
     (local [(define f (interp fun mt-env))]
       (interp (closV-body f) (extend-env 
                               (bind (closV-arg f) (interp arg env))
                               (closV-env f))))]
    [lamC
     (arg body)
     (closV arg body env)]
    ))

(define-type blah
  [foo (x : (number number -> number))])

;; Testing begins here!

#;(define fundefs
  (list (fdC 'double 'x (addC (idC 'x) (idC 'x)))
        (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
        (fdC 'const5 '_ (numC 5))))

;; interp* : s-exp -> number
(define (interp* s)
  (interp (parse s) empty))

(test (interp* '(λ (x) 3))
      (closV 'x (numC 3) mt-env))

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
#;(test (interp* '(quadruple 13))
      (interp* '(double (double 13))))

;; f1(x) = f2(4)
;; f2(y) = x + y

#;(test/exn (interp (appC 'f1 (numC 3)) (list (fdC 'f1 'x (appC 'f2 (numC 4)))
                                            (fdC 'f2 'y (addC (idC 'x) (idC 'y)))) mt-env)
      "undefined")

(test/exn (interp* '(+ x 5))
          "undefined")
