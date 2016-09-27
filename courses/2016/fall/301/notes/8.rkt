#lang plai-typed

(module+ day7
  (define-type ExprC
    [numC (n : number)]
    [idC (s : symbol)]
    [appC (fun : symbol) (arg : ExprC)]
    [plusC (l : ExprC) (r : ExprC)]
    [multC (l : ExprC) (r : ExprC)])

  (define (subtC [l : ExprC] [r : ExprC]) : ExprC
    (plusC l (multC (numC -1) r)))

  (define (negC [o : ExprC]) : ExprC
    (subtC (numC 0) o))

  (define-type FunDefC
    [fdC (name : symbol) (arg : symbol) (body : ExprC)])

  ; get-fundef : symbol * (listof FunDefC) -> FunDefC
  (define (get-fundef [s : symbol] [fds : (listof FunDefC)]) : FunDefC
    (cond
      [(empty? fds) (error 'get-fundef "function definition not found")]
      [(eq? s (fdC-name (first fds))) (first fds)]
      [else (get-fundef s (rest fds))]))

  (test (get-fundef 'add1
                    (list (fdC 'add1 'x (plusC (idC 'x) (numC 1)))))
        (fdC 'add1 'x (plusC (idC 'x) (numC 1))))
  
  (test (get-fundef 'add2
                    (list (fdC 'add1 'x (plusC (idC 'x) (numC 1)))
                          (fdC 'add2 'y (plusC (idC 'y) (numC 2)))))
        (fdC 'add2 'y (plusC (idC 'y) (numC 2))))

  (define-type Binding
    [bind (name : symbol) (val : number)])

  (define-type-alias Env (listof Binding))
  (define mt-env empty)
  (define extend-env cons)

  (define (lookup [for : symbol] [env : Env]) : number
    (cond
      [(empty? env) (error 'lookup "name not found")]
      [(symbol=? for (bind-name (first env)))
       (bind-val (first env))]
      [else (lookup for (rest env))]))

  (define (interp [expr : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
    (type-case
     ExprC expr
     [numC (n) n]
     [idC (n) (lookup n env)]
     [appC
      (f a)
      (local ([define fd (get-fundef f fds)])
        (interp (fdC-body fd)
                (extend-env (bind (fdC-arg fd)
                                  (interp a env fds))
                            mt-env)
                fds))]
     [plusC (l r) (+ (interp l env fds) (interp r env fds))]
     [multC (l r) (* (interp l env fds) (interp r env fds))]))

  (test (interp (plusC (numC 10) (appC 'const5 (numC 10)))
                mt-env
                (list (fdC 'const5 '_ (numC 5))))
        15)

  (test (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
                mt-env
                (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
        16)

  (test (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
                mt-env
                (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
                      (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
        22))
