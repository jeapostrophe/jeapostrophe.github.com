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

(module+ main

  ;; (add1 5)
  ;; (+ 1 1)
  ;;
  ;; ((λ (x) (+ x 1))
  ;;  1)
  ;;
  ;; (5 6)

  (define-type ExprC
    [numC (n : number)]
    [idC (s : symbol)]
    [fdC (arg : symbol) (body : ExprC)]
    [appC (fun : ExprC) (arg : ExprC)]
    [plusC (l : ExprC) (r : ExprC)]
    [multC (l : ExprC) (r : ExprC)])

  (define (subtC [l : ExprC] [r : ExprC]) : ExprC
    (plusC l (multC (numC -1) r)))

  (define (negC [o : ExprC]) : ExprC
    (subtC (numC 0) o))

  (define-type Ans
    [numA (v : number)]
    ;; cloA is "closure Ans"
    [cloA (arg : symbol) (body : ExprC)
          (env : Env)])

  (define-type Binding
    [bind (name : symbol) (val : Ans)])

  (define-type-alias Env (listof Binding))
  (define mt-env empty)
  (define extend-env cons)

  (define (lookup [for : symbol] [env : Env]) : Ans
    (cond
      [(empty? env) (error 'lookup "name not found")]
      [(symbol=? for (bind-name (first env)))
       (bind-val (first env))]
      [else (lookup for (rest env))]))

  (define (ans+ (l : Ans) (r : Ans))
    (type-case
     Ans l
     [numA
      (l)
      (type-case
       Ans r
       [numA
        (r)
        (numA (+ l r))]
       [else
        (error 'ans+ "Given non-number")])]
     [else
      (error 'ans+ "Given non-number")]))
  (define (ans* (l : Ans) (r : Ans))
    (type-case
     Ans l
     [numA
      (l)
      (type-case
       Ans r
       [numA
        (r)
        (numA (* l r))]
       [else
        (error 'ans* "Given non-number")])]
     [else
      (error 'ans* "Given non-number")]))

  (define (interp [expr : ExprC] [env : Env]) : Ans
    (type-case
     ExprC expr
     [numC (n) (numA n)]
     [idC (n) (lookup n env)]
     [fdC
      (arg body)
      ;; allowed to see 'double (i.e. 'double is in env)
      (cloA arg body env)]
     [appC
      (f a)
      (local ([define fd
                ;; (get-fundef f fds) ;; => Implies that f is pre-known
                (interp f env)])
        (interp (cloA-body fd)
                (extend-env (bind (cloA-arg fd)
                                  (interp a env))
                            ;; Like before, this is wrong:
                            #;env
                            ;; Emacs Lisp
                            (cloA-env fd))))]
     [plusC (l r) (ans+ (interp l env) (interp r env))]
     [multC (l r) (ans* (interp l env) (interp r env))]))

  (test (interp (plusC (numC 10)
                       (appC (fdC 'x (plusC (idC 'x) (idC 'x)))
                             (numC 5)))
                mt-env)
        (numA 20))

  (test (interp (appC (fdC 'y
                           (plusC (appC (fdC 'x (plusC (idC 'x) (idC 'x)))
                                        (idC 'y))
                                  (appC (fdC 'x (plusC (idC 'x) (idC 'x)))
                                        (idC 'y))))
                      (numC 5))
                mt-env)
        (numA 20))

  (define (withC name named-expr body)
    (appC (fdC name body) named-expr))

  (test (interp (withC 'double (fdC 'x (plusC (idC 'x) (idC 'x)))
                       (appC (idC 'double)
                             (numC 10)))
                mt-env)
        (numA 20))
  (test (interp (appC (fdC 'double
                           (appC (idC 'double)
                                 (numC 10)))
                      (fdC 'x (plusC (idC 'x) (idC 'x))))
                mt-env)
        (numA 20))

  (test (interp (withC 'v (numC 20)
                       (idC 'v))
                mt-env)
        (numA 20))
  (test (interp (appC (fdC 'v
                           (idC 'v))
                      (numC 20))
                mt-env)
        (numA 20))

  (test (interp (withC 'double (fdC 'x (plusC (idC 'x) (idC 'x)))
                       (withC 'quad
                              (fdC 'y
                                   (plusC (appC (idC 'double)
                                                (idC 'y))
                                          (appC (idC 'double)
                                                (idC 'y))))
                              (appC (idC 'quad)
                                    (numC 5))))
                mt-env)
        (numA 20))

  (test (interp (withC 'double (fdC 'x (plusC (idC 'x) (idC 'x)))
                       (withC 'quad
                              (fdC 'y
                                   (plusC (appC (fdC 'x (plusC (idC 'x) (idC 'x)))
                                                (idC 'y))
                                          (appC (fdC 'x (plusC (idC 'x) (idC 'x)))
                                                (idC 'y))))
                              (appC (idC 'quad)
                                    (numC 5))))
                mt-env)
        (numA 20))


  (test (interp (withC 'quad
                       (withC 'double (fdC 'x (plusC (idC 'x) (idC 'x)))
                              (fdC 'y
                                   (plusC (appC (idC 'double)
                                                (idC 'y))
                                          (appC (idC 'double)
                                                (idC 'y)))))
                       (withC 'double (numC 5)
                              (appC (idC 'quad)
                                    (idC 'double))))
                mt-env)
        (numA 20))

  #;(test (interp (appC 'loop-forever (numC 5))
                mt-env
                (list (funDefC 'loop-forever 'x (appC 'loop-forever (idC 'x)))))
          42)

  ;; Comment this back in to run forever
  #;
  (test (interp (withC 'loop-forever
                       ;; (x -> nothing) -> (x -> nothing)
                       (fdC 'me
                            (fdC 'x (appC (appC (idC 'me) (idC 'me)) (idC 'x))))
                       (appC (appC (idC 'loop-forever) (idC 'loop-forever))
                             (numC 5)))
                mt-env)
        (numA 42))
  
  )
