#lang plai
; Changes before class:
;  - Using word "Env" instead of "Sub"
;  - Using 'env' rather than 'ds'
;  - 'with' has been removed from language, but is sugar for parser
;  - 'if0' added to language
;  - 'make-numV-op' no longer produces numVs and handles any arity Racket functions
(halt-on-errors #t)

(define-type CFWAE
  [num (n number?)]
  [add (lhs CFWAE?)
       (rhs CFWAE?)]
  [sub (lhs CFWAE?)
       (rhs CFWAE?)]
  [id (name symbol?)]
  [fun (param symbol?)
       (body CFWAE?)]
  [app (fun-expr CFWAE?)
       (arg-expr CFWAE?)]
  [if0 (test-expr CFWAE?)
       (true-expr CFWAE?)
       (false-expr CFWAE?)])

; <CFWAE> :== <number>
;      |   (+ <CFWAE> <CFWAE>)
;      |   (- <CFWAE> <CFWAE>)
;      |   <id>
;      |   (with [<id> <CFWAE>] <CFWAE>)
;      |   (fun (<id>) <CFWAE>)
;      |   (<CFWAE> <CFWAE>)
;      |   (if0 <CFWAE> <CFWAE> <CFWAE>)
; where <id> is symbol

; parse: Sexpr -> CFWAE
; Purpose: Parse an Sexpr into an CFWAE
(define (parse se)
  (cond [(number? se) (num se)]
        [(symbol? se) (id se)]
        [(and (list? se) (symbol? (first se)) (symbol=? 'with (first se)))
         (app (fun (first (second se))
                   (parse (third se)))
              (parse (second (second se))))]
        [(and (list? se) (symbol? (first se)) (symbol=? '+ (first se)))
         (add (parse (second se))
              (parse (third se)))]
        [(and (list? se) (symbol? (first se)) (symbol=? '- (first se)))
         (sub (parse (second se))
              (parse (third se)))]
        [(and (list? se) (symbol? (first se)) (symbol=? 'fun (first se)))
         (fun (first (second se))
              (parse (third se)))]
        [(and (list? se) (symbol? (first se)) (symbol=? 'if0 (first se)))
         (if0 (parse (second se))
              (parse (third se))
              (parse (fourth se)))]
        [(and (list? se))
         (app (parse (first se)) (parse (second se)))]))

(test (parse '(if0 0 1 2))
      (if0 (num 0) (num 1) (num 2)))

(test (parse '(f 1))
      (app (id 'f) (num 1)))
(test (parse '(fun (x) x))
      (fun 'x (id 'x)))

(test (parse '(+ 1 1))
      (add (num 1) (num 1)))
(test (parse '(- 1 2))
      (sub (num 1) (num 2)))
(test (parse '(with [x 5] (+ x x)))
      (app (fun 'x (add (id 'x) (id 'x)))
           (num 5)))

(define-type CFWAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body CFWAE?)
            (env Env?)]
  [promiseV (delayed-expr CFWAE?)
            (original-env Env?)
            (value-cache (box/c (or/c #f CFWAE-Value?)))])

(define-type Env
  [mtEnv] ; mt is "em" "tee"
  [anEnv (name symbol?)
         (value CFWAE-Value?)
         (rest Env?)])

(define env-ex0
  (mtEnv))
(define env-ex1
  (anEnv 'x (numV 1)
         (mtEnv)))
(define env-ex2
  (anEnv 'y (numV 2)
         env-ex1))

; lookup-Env : symbol Env -> number/#f
(define (lookup-Env name env)
  (type-case Env env
    [mtEnv ()
           #f]
    [anEnv (some-name some-value rest-env)
           (if (symbol=? name
                         some-name)
               some-value
               (lookup-Env name rest-env))]))

(test (lookup-Env 'x (mtEnv)) #f)
(test (lookup-Env 'x 
                  (anEnv 'x (numV 1)
                         (mtEnv))) 
      (numV 1))
(test (lookup-Env 'x 
                  (anEnv 'y (numV 2)
                         (anEnv 'x (numV 1)
                                (mtEnv)))) 
      (numV 1))
(test (lookup-Env 'x 
                  (anEnv 'x (numV 2)
                         (anEnv 'x (numV 1)
                                (mtEnv)))) 
      (numV 2))

(define (make-numV-op op)
  (lambda args
    (apply op
           (map (λ (c-v)
                  (type-case CFWAE-Value c-v
                    [numV (n)
                          n]
                    [else
                     (error 'interp "Not a number")]))
                args))))
(define numV+ (make-numV-op (compose numV +)))
(define numV- (make-numV-op (compose numV -)))
(define numV-zero? (make-numV-op zero?))

; interp : CFWAE Env -> CFWAE-Value
; Purpose: To compute the number represented by the CFWAE
(define (interp e env)
  (type-case CFWAE e
    [num (n) 
         (numV n)]
    [add (lhs rhs) 
         (numV+ (strict (interp lhs env))
                (strict (interp rhs env)))]
    [sub (lhs rhs)
         (numV- (strict (interp lhs env))
                (strict (interp rhs env)))]
    [if0 (test-e true-e false-e)
         (if (numV-zero? (strict (interp test-e env)))
             (interp true-e env)
             (interp false-e env))]
    [id (name)
        (local [(define names-value (lookup-Env name env))]
          (if names-value
              names-value
              (error 'interp "Unbound identifier: ~e" name)))]
    [fun (param body)
         (closureV param body env)]
    [app (fun-expr arg-expr)
         (local [(define the-fundef 
                   (strict (interp fun-expr env)))]
           (type-case CFWAE-Value the-fundef
             [closureV (param-name body-expr funs-env)
                       (interp body-expr
                               (anEnv param-name
                                      (delay arg-expr env)
                                      #;(interp arg-expr env)
                                      funs-env))]
             [numV (n)
              (error 'interp "Not a function")]
             [promiseV (e env cache)
                      (error 'interp "Impossible")]))]))

; delay : CFWAE Env -> promiseV
(define (delay e env)
  (promiseV e env (box #f)))

; strict : CFWAE-Value -> CFWAE-Value (except not a promiseV)
(define (strict cv)
  (type-case CFWAE-Value cv
    [promiseV (e env cache)
              (if (unbox cache)
                  (unbox cache)
                  (begin #;(printf "Evaluating ~a\n" e)
                         ; Uncomment that printf to see that we are lazy
                         (local [(define actual-v (strict (interp e env)))]
                           (begin (set-box! cache actual-v)
                                  actual-v))))]
    [else
     cv]))

; calc : CFWAE -> number
(define (calc e)
  (define v (strict (interp e (mtEnv))))
  (type-case CFWAE-Value v
    [numV (n)
          n]
    [else
     v]))

(test/exn (calc (parse 'x))
          "Unbound identifier")
(test (calc (parse '0))
      0)
(test (calc (parse '(+ 1 1)))
      2)
(test (calc (parse '(- 2 1)))
      1)
(test (calc (parse (list '- 2 1)))
      1)
(test (calc (parse (list '- 2 (list '- 2 1))))
      1)
(test (calc (parse '(- (+ 1 2) (- 8 9))))
      4)
(test (calc (parse '(with [x 5] (+ x x))))
      10)
(test (calc (parse '(with [x (+ 5 6)] (+ x x))))
      22)

(test (calc (parse '(with [x (+ 5 6)] (+ x x))))
      (calc (parse '(with [x 11] (+ x x)))))
(test (calc (parse '(with [x (+ 5 6)] (+ x x))))
      (calc (parse '(+ (+ 5 6)
                       (+ 5 6)))))
(test (calc (parse '(with [x (+ 5 6)] (+ x x))))
      (calc (parse '(+ 11 11))))

(test (calc (parse '(with [x (+ 5 6)]
                      (with [y (+ x 1)]
                        (+ x y)))))
      23)
(test (calc (parse '(with [x (+ 5 6)]
                      (with [x (+ x 1)]
                        (+ x x)))))
      24)

(test/exn (calc (parse '(double 5)))
          "Unbound identifier")
(test/exn (calc (parse '(with (double 1)
                          (double 5))))
          "Not a function")
(test (calc (parse '(with (double 
                           (fun (x)
                                (+ x x)))
                      (double 5))))
      10)
(test (calc (parse '((fun (x) (+ x x)) 5)))
      10)

(test (calc (parse '(with (x 1)
                      (with (y 2)
                        (with (z 3)
                          (+ x (+ y z)))))))
      6)

(test (calc (parse '(with [x 1]
                      (with [f (fun (y)
                                    (+ x y))]
                        (f 10)))))
      ; Sam
      11
      ; Joseph says with can't find fun defs
      #;"Unbound identifier")
(test/exn (calc (parse '(with [f (fun (y)
                                      (+ x y))]
                          (with [x 1]
                            (f 10)))))
          "Unbound identifier")

(test (calc (parse '(with [x 10]
                      (with [add10
                             (fun (y)
                                  (+ x y))]
                        (add10 5)))))
      15)
(test (calc (parse '(with [add10
                           (with [x 10]
                             (fun (y)
                                  (+ x y)))]
                      (add10 5))))
      15)
(test (calc (parse '(with [make-adder
                           (fun (x)
                                (fun (y)
                                     (+ x y)))]
                      (with [add10
                             (make-adder 10)]
                        (add10 5)))))
      15)
(test (calc (parse '(with [make-adder
                           (fun (x)
                                (fun (y)
                                     (+ x y)))]
                      (with [add10
                             (make-adder 10)]
                        (+ (add10 5)
                           (add10 6))))))
      31)
(test (calc (parse '(with [fake-adder
                           (fun (x)
                                (fun (y)
                                     (+ x x)))]
                      (with [add10
                             (fake-adder 10)]
                        (add10 5)))))
      20)
(test (calc (parse '(with [fake-adder
                           (fun (x)
                                (fun (y)
                                     (+ y y)))]
                      (with [add10
                             (fake-adder 10)]
                        (add10 5)))))
      10)

(test (calc (parse '(if0 0 1 2)))
      1)
(test (calc (parse '(if0 1 1 2)))
      2)

(test (calc (parse '(with [x (1 0)]
                      42)))
      ; If we are lazy
      42
      ; If we are eager
      #;"Not a function")

(test (calc (parse '(with [double (fun (x) (+ x x))]
                      (with [not-double/jk double]
                        (not-double/jk 5)))))
      10)
(test (calc (parse '(with [x 5]
                      (with [y x]
                        (+ y y)))))
      10)

(test (calc (parse '(with [x 5] x)))
      5)

(test (calc (parse '(with [omega
                           
                           ((fun (x) (x x))
                            (fun (x) (x x)))
                           ;=>
                           ;(x x) [x -> (fun (x) (x x))]
                           ;=>
                           ;((fun (x) (x x))
                           ; (fun (x) (x x)))
                           ]
                      42)))
      ; If we are lazy
      42
      ; If we are eager
      #;"Not a function")