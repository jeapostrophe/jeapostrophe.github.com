#lang plai
(halt-on-errors #t)

(define-type RCFWAE
  [num (n number?)]
  [add (lhs RCFWAE?)
       (rhs RCFWAE?)]
  [sub (lhs RCFWAE?)
       (rhs RCFWAE?)]
  [id (name symbol?)]
  [fun (param symbol?)
       (body RCFWAE?)]
  [app (fun-expr RCFWAE?)
       (arg-expr RCFWAE?)]
  [if0 (test-expr RCFWAE?)
       (true-expr RCFWAE?)
       (false-expr RCFWAE?)]
  [rec (bound-id symbol?)
    (bound-expr RCFWAE?)
    (body-expr RCFWAE?)])

; <RCFWAE> :== <number>
;      |   (+ <RCFWAE> <RCFWAE>)
;      |   (- <RCFWAE> <RCFWAE>)
;      |   <id>
;      |   (with [<id> <RCFWAE>] <RCFWAE>)
;      |   (rec [<id> <RCFWAE>] <RCFWAE>)
;      |   (fun (<id>) <RCFWAE>)
;      |   (<RCFWAE> <RCFWAE>)
;      |   (if0 <RCFWAE> <RCFWAE> <RCFWAE>)
; where <id> is symbol

; parse: Sexpr -> RCFWAE
; Purpose: Parse an Sexpr into an RCFWAE
(define (parse se)
  (cond [(number? se) (num se)]
        [(symbol? se) (id se)]
        [(and (list? se) (symbol? (first se)) (symbol=? 'rec (first se)))
         (rec (first (second se))
           (parse (second (second se)))
           (parse (third se)))]
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
  
  
(test (parse '(rec [x 5] (+ x x)))
      (rec 'x (num 5) (add (id 'x) (id 'x))))

#;(define-type RCFWAE-Value
  [undefV]
  [numV (n number?)]
  [closureV (param symbol?)
            (body RCFWAE?)
            (env Env?)])

(define (numV n) n)
(define (closureV param body env)
  ; We only ever use closureV for...
  #;[closureV (param-name body-expr funs-env)
              (interp body-expr
                      (anEnv param-name
                             (interp arg-expr env)
                             funs-env))]
  (λ (arg)
    (interp body
            (anEnv param arg
                   env))))
(define (undefV) #f)

(define RCFWAE-Value?
  (or/c #f number? procedure?))

#;(define-type Env
  [mtEnv] ; mt is "em" "tee"
  [anEnv (name symbol?)
         (value RCFWAE-Value?)
         (rest Env?)]
  [a-Cyclic-Env (back-pointer (box/c Env?))])

; This is basically OO where the arguments to (*Env ...) are the fields
; And the closure is the code pointer + fields (in C++)
; This is called "Church Encoding" and it's awesome
(define (mtEnv)
  #;#f
  (λ (name) #f))
(define (anEnv some-name some-val some-env) 
  #;(if (symbol=? name
                  some-name)
        some-value
        (lookup-Env name rest-env))
  (λ (name)
    (if (symbol=? name some-name)
        some-val
        (lookup-Env name some-env))))
(define (a-Cyclic-Env back-pointer) 
  (λ (name)
    #;(lookup-Env name (unbox back-pointer))
    ((unbox back-pointer) name)))

(define Env?
  (-> symbol? (or/c #f RCFWAE-Value?)))

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
  (env name)
  #;(type-case Env env
    [mtEnv ()
           #f]
    [anEnv (some-name some-value rest-env)
           (if (symbol=? name
                         some-name)
               some-value
               (lookup-Env name rest-env))]
    [a-Cyclic-Env (back-pointer)
                  (lookup-Env name (unbox back-pointer))]))

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
(test (lookup-Env 'x 
                  (a-Cyclic-Env
                   (box (anEnv 'x (numV 2)
                               (anEnv 'x (numV 1)
                                      (mtEnv)))))) 
      (numV 2))

#;(define (make-numV-op op)
  (lambda args
    (apply op
           (map (λ (c-v)
                  (type-case RCFWAE-Value c-v
                    [numV (n)
                          n]
                    [else
                     (error 'interp "Not a number")]))
                args))))
; numVs are actually Racket numbers
(define numV+ + #;(make-numV-op (compose numV +)))
(define numV- - #;(make-numV-op (compose numV -)))
(define numV-zero? zero? #;(make-numV-op zero?))

; interp : RCFWAE Env -> RCFWAE-Value
; Purpose: To compute the number represented by the RCFWAE
(define (interp e env)
  (type-case RCFWAE e
    [num (n)
         #;n
         (numV n)]
    [add (lhs rhs)
         #;(+ lhs rhs)
         ; Because numV+ is an alias for +, we could replace it here
         (numV+ (interp lhs env)
                (interp rhs env))]
    [sub (lhs rhs)
         #;(- lhs rhs)
         (numV- (interp lhs env)
                (interp rhs env))]
    [if0 (test-e true-e false-e)
         #;(if (zero? test-e) true-e false-e)
         (if (numV-zero? (interp test-e env))
             (interp true-e env)
             (interp false-e env))]
    [id (name)
        #;name
        (local [(define names-value (lookup-Env name env))]
          (if names-value
              names-value
              (error 'interp "Unbound identifier: ~e" name)))]
    [fun (param body)
         #;(λ (param) body)
         (closureV param body env)]
    [app (fun-expr arg-expr)
         #;(fun-expr arg-expr)
         (local [(define the-fundef 
                   (interp fun-expr env))]
           (if (procedure? the-fundef)
               (the-fundef (interp arg-expr env))
               (error 'interp "Not a function"))
           #;(type-case RCFWAE-Value the-fundef
             [closureV (param-name body-expr funs-env)
                       (interp body-expr
                               (anEnv param-name
                                      (interp arg-expr env)
                                      funs-env))]
             [else
              (error 'interp "Not a function")]))]
    [rec (bound-id bound-expr body-expr)
      #;(local [(define bound-id bound-expr)] body-expr)
      #;(letrec ([bound-id bound-expr]) body-expr)
      (local [(define back-pointer
                (box (anEnv bound-id (undefV)
                            env)))
              (define not-yet-the-correct-env
                (a-Cyclic-Env back-pointer))
              (define bound-value
                (interp bound-expr not-yet-the-correct-env))
              (define the-correct-env 
                (anEnv bound-id bound-value
                       env))]
        (begin (set-box! back-pointer the-correct-env)
               ; Now the not-yet-the-correct-env is the-correct-env
               (interp body-expr the-correct-env)))]))

; calc : RCFWAE -> number
#;(define (calc e)
  (define v (interp e (mtEnv)))
  v
  #;(type-case RCFWAE-Value v
    [numV (n)
          n]
    [else
     v]))

(require (for-syntax syntax/parse racket))

#;(define (fun req0 req1 [opt1 "a"] #:keyword req-key #:keyword2 [opt-key "b"])
    ...)
#;(fun 1 2 #:keyword 3)
#;(fun 1 #:keyword 3 2)

(define-syntax (calc stx)
  (syntax-parse
   stx
   #:literals (parse quote)
   [(calc (parse (quote rcfwae:expr)))
    (syntax
     (compile-rcfwae rcfwae))]))
(define-syntax (with stx) (raise-syntax-error 'with "Used outside compiler" stx))
(define-syntax (compile-rcfwae stx)
  (syntax-parse
   stx
   #:literals (+ - fun with if0 rec)
   [(compile-rcfwae n:number)
    (syntax n)]
   [(compile-rcfwae (+ lhs:expr rhs:expr))
    (syntax 
     (+ (compile-rcfwae lhs)
        (compile-rcfwae rhs)))]
   [(compile-rcfwae (- lhs:expr rhs:expr))
    (syntax 
     (- (compile-rcfwae lhs)
        (compile-rcfwae rhs)))]
   [(compile-rcfwae name:id)
    (syntax name)]
   [(compile-rcfwae (fun (param:id) body:expr))
    (syntax (λ (param) (compile-rcfwae body)))]
   [(compile-rcfwae (with [name:id named:expr] body:expr))
    (syntax (compile-rcfwae ((fun (name) body) named)))]
   [(compile-rcfwae (if0 test:expr true:expr false:expr))
    (syntax (if (equal? (compile-rcfwae test) 0)
                (compile-rcfwae true)
                (compile-rcfwae false)))]
   [(compile-rcfwae (rec [name:id named:expr] body:expr))
    (syntax (letrec ([name (compile-rcfwae named)])
              (compile-rcfwae body)))]
   [(compile-rcfwae (fun-e:expr arg-e:expr))
    (syntax ((compile-rcfwae fun-e)
             (compile-rcfwae arg-e)))]))

#;(test/exn x
          "Unbound identifier")
#;(test/exn (calc (parse 'x))
          "Unbound identifier")
(test (calc (parse '0))
      0)
(test (calc (parse '(+ 1 1)))
      2)
(test (calc (parse '(- 2 1)))
      1)
#;(test (calc (parse (list '- 2 1)))
      1)
#;(test (calc (parse (list '- 2 (list '- 2 1))))
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

#;(test/exn (calc (parse '(double 5)))
          "Unbound identifier")
#;(test/exn (calc (parse '(with (double 1)
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
#;(test/exn (calc (parse '(with [f (fun (y)
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
(test (calc (parse '(with [x 0]
                      (if0 x 1 2))))
      1)

#;(test/exn (calc (parse '(with [x (0 1)]
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

#;(test/exn (calc (parse '(with [add-fac
                               (fun (x)
                                    (if0 x
                                         x
                                         (+ x (add-fac (- x 1)))))]
                          (add-fac 7))))
          "Unbound identifier")

; MakeEnv 'with 'bound-expr env = env
; MakeEnv 'with 'bound-body env = (anEnv bound-id bound-value env)

(test (calc (parse '(rec [add-fac
                          (fun (x)
                               (if0 x
                                    x
                                    (+ x (add-fac (- x 1)))))]
                      (add-fac 7))))
      (+ 7 6 5 4 3 2 1 0))

(test (calc (parse '(with [add-fac 5]
                      (rec [add-fac
                            (fun (x)
                                 (if0 x
                                      x
                                      (+ x (add-fac (- x 1)))))]
                        (add-fac 7)))))
      (+ 7 6 5 4 3 2 1 0))

(test (calc (parse '(rec [add-fac
                          (fun (x)
                               (if0 x
                                    x
                                    (+ x (add-fac (- x 1)))))]
                      (add-fac 1))))
      (+ 1 0))

; MakeEnv 'rec 'bound-expr env = (anEnv bound-id ... env)
; MakeEnv 'rec 'bound-body env = (anEnv bound-id bound-value env)

; Eventually we bottom out
#;(anEnv 'add-fac 
         (closureV 'x --definition-of-add-fac---
                   (anEnv 'add-fac
                          (closureV 'x ---definition-of-add-fac---
                                    (anEnv 'add-fac
                                           (closureV 'x ---definition-of-add-fac---
                                                     env)
                                           env))
                          env))
         env)

#;(define E
    (anEnv 'add-fac 
           (closureV 'x --definition-of-add-fac---
                     E)
           env))

#;(define E
    ((λ (E_0)
      (anEnv 'add-fac 
             (closureV 'x --definition-of-add-fac---
                       E_0)
             env))
     E))

; x = f x

(test (calc (parse '(with [x 1] x)))
      1)
(test (calc (parse '(rec [x 1] x)))
      1)
(test (calc (parse '(with [y 1]
                      (with [x y]
                        x))))
      1)
(test (calc (parse '(with [y 1]
                      (rec [x (fun (z) y)]
                        (x 2)))))
      1)
(test (calc (parse '(with [y 1]
                      (rec [x y]
                        x))))
      1)

(test (calc (parse '(with [x 1]
                      (with [x x]
                        x))))
      1)
#;(test/exn (calc (parse '(with [x 1]
                          (rec [x (+ 1 x)]
                            x))))
          "Not a number"
          #;"Unbound identifier"
          #;-infinite-loop-
          #;2)

; Infinite loop:
#;(test (calc (parse '(rec [loop
                            (fun (x)
                                 (loop x))]
                        (loop 1))))
        42)

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