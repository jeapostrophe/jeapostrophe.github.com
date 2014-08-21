#lang plai
; Changes before class:
; - removed rec
; - added binop
; - switched to match syntax
; - remove make-numV-op
; - language is named BCFAE
(halt-on-errors #t)

(define-type BCFAE
  [num (n number?)]
  [binop (op procedure?)
         (lhs BCFAE?)
         (rhs BCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?)
       (body BCFAE?)]
  [app (fun-expr BCFAE?)
       (arg-expr BCFAE?)]
  [if0 (test-expr BCFAE?)
       (true-expr BCFAE?)
       (false-expr BCFAE?)]
  [newbox (init-expr BCFAE?)]
  [openbox (box-expr BCFAE?)]
  [setbox (box-expr BCFAE?)
          (val-expr BCFAE?)]
  [seqn (first-expr BCFAE?)
        (second-expr BCFAE?)])

(define (with name named-expr body-expr)
  (app (fun name body-expr) named-expr))

; <BCFAE> :== <number>
;      |   (+ <BCFAE> <BCFAE>)
;      |   (- <BCFAE> <BCFAE>)
;      |   <id>
;      |   (with [<id> <BCFAE>] <BCFAE>)
;      |   (fun (<id>) <BCFAE>)
;      |   (<BCFAE> <BCFAE>)
;      |   (if0 <BCFAE> <BCFAE> <BCFAE>)
;      |   (newbox <BCFAE>)
;      |   (openbox <BCFAE>)
;      |   (setbox <BCFAE> <BCFAE>)
;      |   (seqn <BCFAE> <BCFAE>)
; where <id> is symbol

; parse: Sexpr -> BCFAE
; Purpose: Parse an Sexpr into an BCFAE
(define parse
  (match-lambda
    [(? number? n) (num n)]
    [(? symbol? name) (id name)]
    [`(openbox ,e) (openbox (parse e))]
    [`(newbox ,e) (newbox (parse e))]
    [`(setbox ,b ,e) (setbox (parse b) (parse e))]
    [`(seqn ,b ,e) (seqn (parse b) (parse e))]
    [`(with [,(? symbol? name) ,named-expr] ,body-expr)
     (with name (parse named-expr) (parse body-expr))]
    [`(+ ,lhs ,rhs) (binop + (parse lhs) (parse rhs))]
    [`(- ,lhs ,rhs) (binop - (parse lhs) (parse rhs))]
    [`(* ,lhs ,rhs) (binop * (parse lhs) (parse rhs))]
    [`(/ ,lhs ,rhs) (binop / (parse lhs) (parse rhs))]
    [`(fun (,(? symbol? param)) ,body) (fun param (parse body))]
    [`(if0 ,test ,true ,false) (if0 (parse test) (parse true) (parse false))]
    [`(,fun ,arg) (app (parse fun) (parse arg))]))

(test (parse '(if0 0 1 2))
      (if0 (num 0) (num 1) (num 2)))

(test (parse '(f 1))
      (app (id 'f) (num 1)))
(test (parse '(fun (x) x))
      (fun 'x (id 'x)))

(test (parse '(+ 1 1))
      (binop + (num 1) (num 1)))
(test (parse '(- 1 2))
      (binop - (num 1) (num 2)))
(test (parse '(with [x 5] (+ x x)))
      (app (fun 'x (binop + (id 'x) (id 'x)))
           (num 5)))

(define-type BCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body BCFAE?)
            (env Env?)]
  [boxV (addr number?)])

; An env is a mapping from symbol to addresses
(define-type Env
  [mtEnv] ; mt is "em" "tee"
  [anEnv (name symbol?)
         (addr number?)
         (rest Env?)])

; A store is a mapping from address/numbers to value
(define-type Store
  [mtStore] ; mt is "em" "tee"
  [aStore (addr number?)
          (value BCFAE-Value?)
          (rest Store?)])

(define env-ex0
  (mtEnv))
(define env-ex1
  (anEnv 'x 1
         (mtEnv)))
(define env-ex2
  (anEnv 'y 2
         env-ex1))

; lookup-Env : symbol Env -> number/#f
(define (lookup-Env name env)
  (type-case Env env
    [mtEnv ()
           #f]
    [anEnv (some-name some-addr rest-env)
           (if (symbol=? name
                         some-name)
               some-addr
               (lookup-Env name rest-env))]))

(test (lookup-Env 'x (mtEnv)) #f)
(test (lookup-Env 'x 
                  (anEnv 'x 1
                         (mtEnv))) 
      1)
(test (lookup-Env 'x 
                  (anEnv 'y 2
                         (anEnv 'x 1
                                (mtEnv)))) 
      1)
(test (lookup-Env 'x 
                  (anEnv 'x 2
                         (anEnv 'x 1
                                (mtEnv)))) 
      2)

; lookup-Store : addr Store -> number/#f
(define (lookup-Store addr store)
  (type-case Store store
    [mtStore ()
           #f]
    [aStore (some-addr some-value rest-store)
           (if (= addr some-addr)
               some-value
               (lookup-Store addr rest-store))]))

(test (lookup-Store 1 (mtStore)) #f)
(test (lookup-Store 1
                    (aStore 1 (numV 1)
                            (mtStore))) 
      (numV 1))
(test (lookup-Store 1
                    (aStore 2 (numV 2)
                            (aStore 1 (numV 1)
                                    (mtStore)))) 
      (numV 1))
(test (lookup-Store 2 
                    (aStore 2 (numV 2)
                            (aStore 1 (numV 1)
                                    (mtStore)))) 
      (numV 2))

; max-addr : Store -> addr
(define (max-addr st)
  (type-case Store st
    [mtStore () -1]
    [aStore (some-addr some-val some-st)
            (max some-addr (max-addr some-st))]))
; malloc : Store -> addr
(define (malloc st)
  (add1 (max-addr st)))

; interp : BCFAE Env Store -> BCFAE-Value
; Purpose: To compute the number represented by the BCFAE
(define (interp e env store)
  (type-case BCFAE e
    [num (n) 
         (numV n)]
    [binop (op lhs rhs) 
           (type-case BCFAE-Value (interp lhs env store)
             [numV (lhs-v)
                   (type-case BCFAE-Value (interp rhs env store)
                     [numV (rhs-v)
                           (numV (op lhs-v rhs-v))]
                     [else
                      (error 'interp "Not a number [rhs]")])]
             [else
              (error 'interp "Not a number [lhs]")])]
    [if0 (test-e true-e false-e)
         (type-case BCFAE-Value (interp test-e env store)
           [numV (test-v)
                 (if (zero? test-v)
                     (interp true-e env store)
                     (interp false-e env store))]
           [else 
            (error 'interp "Not a number")])]
    [id (name)
        (local [(define names-addr (lookup-Env name env))]
          (if names-addr
              (local [(define names-value (lookup-Store names-addr store))]
                (if names-value
                    names-value
                    (error 'interp "SEGFAULT ~e" names-addr)))
              (error 'interp "Unbound identifier: ~e" name)))]
    [fun (param body)
         (closureV param body env)]
    [app (fun-expr arg-expr)
         (local [(define the-fundef 
                   (interp fun-expr env store))]
           (type-case BCFAE-Value the-fundef
             [closureV (param-name body-expr funs-env)
                       (local [(define param-addr (malloc store))]
                         (interp body-expr
                                 (anEnv param-name
                                        param-addr
                                        funs-env)
                                 (aStore param-addr
                                         (interp arg-expr env store)
                                         store)))]
             [else
              (error 'interp "Not a function")]))]
    [else
     (numV 42)]))

; calc : BCFAE -> number
(define (calc e)
  (define v (interp e (mtEnv) (mtStore)))
  (type-case BCFAE-Value v
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
; XXX This test is broken. We're not passing the store as if it is global
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
(test (calc (parse '(with [x 0]
                      (if0 x 1 2))))
      1)

(test/exn (calc (parse '(with [x (0 1)]
                          42)))
          ; If we are eager...
          "Not a function"
          ; If we are lazy...
          #;42)

(test (calc (parse '(with [x 5] x)))
      5)

(test (calc (parse '(with [double (fun (x) (+ x x))]
                      (with [not-double/jk double]
                        (not-double/jk 5)))))
      10)
(test (calc (parse '(with [x 5]
                      (with [y x]
                        (+ y y)))))
      10)

(test/exn (calc (parse '(with [add-fac
                               (fun (x)
                                    (if0 x
                                         x
                                         (+ x (add-fac (- x 1)))))]
                          (add-fac 7))))
          "Unbound identifier")

; MakeEnv 'with 'bound-expr env = env
; MakeEnv 'with 'bound-body env = (anEnv bound-id bound-value env)

(test (calc (parse '(with [x 1] x)))
      1)

(test (calc (parse '(with [y 1]
                      (with [x y]
                        x))))
      1)

(test (calc (parse '(with [x 1]
                      (with [x x]
                        x))))
      1)

#;(test (calc (parse '(with [omega (fun (x) (x x))]
                        (with [Omega (omega omega)]
                          42))))
        51)


; λf. (λx. f (λy. x x y)) (λx. f (λy. x x y))
 
(test (calc (parse '(with [Y 
                           (fun (f)
                                ((fun (x) (f (fun (y) ((x x) y))))
                                 (fun (x) (f (fun (y) ((x x) y))))))]
                      (with [make-add-fac
                             (fun (add-fac)
                                  (fun (x)
                                       (if0 x
                                            x
                                            (+ x (add-fac (- x 1))))))]
                        (with [add-fac (Y make-add-fac)
                               #;(make-add-fac add-fac)]
                          (add-fac 7))))))
      (+ 7 6 5 4 3 2 1 0))


;;-----

#| Is windowing/gui programming inherently stateful?

; show-the-window : (event -> void) -> doesnt' return 
(show-the-window on-click-function)

; show-the-window : initial-state (state event -> state)
(show-the-window 0 +)

(define (show-the-window initial f)
  (define event (get-the-next-event))
  (show-the-window (f initial event) f))
|#

; Box examples

(test (calc (parse '(openbox (newbox 42))))
      42)
(test (calc (parse '(with [x (newbox 42)]
                      (openbox x))))
      42)
(test (calc (parse '(with [x (newbox 42)]
                      (+ (openbox x)
                         (openbox x)))))
      84)
(test (calc (parse '(with [x (newbox 42)]
                      (setbox x 43))))
      43)
(test (calc (parse '(with [x (newbox 42)]
                      (seqn (setbox x 43)
                            (openbox x)))))
      43)
; We're not like C, evaluate the left first
(test (calc (parse '(with [x (newbox 42)]
                      (+ (openbox x)
                         (seqn (setbox x 43)
                               (openbox x))))))
      (+ 42 43))
(test (calc (parse '(with [x (newbox 42)]
                      (+ (seqn (setbox x 43)
                               (openbox x))
                         (openbox x)))))
      (+ 43 43))
(test (calc (parse '(with [x (newbox 42)]
                      (with [f (fun (y) (openbox x))]
                        (f 1)))))
      42)
; Closures do not "close" or "capture" the values of boxes
(test (calc (parse '(with [x (newbox 42)]
                      (with [f (fun (y) (openbox x))]
                        (seqn (setbox x 43)
                              (f 1))))))
      43)
(test (calc (parse '(with [x (newbox 42)]
                      (with [z (newbox 1)]
                        (with [f (fun (y) (openbox z))]
                          (seqn (setbox z x)
                                (openbox (f 1))))))))
      42)
(test/exn (calc (parse '(openbox 42)))
          "Not a box")
(test/exn (calc (parse '(setbox 42 43)))
          "Not a box")
(test (calc (parse '(seqn 1 2)))
      2)


; x = 0
; f(++x, ++x)
; ->
; f(2, 1)
; NOT
; f(1, 2)