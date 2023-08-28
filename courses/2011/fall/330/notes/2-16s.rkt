#lang plai
(halt-on-errors)

(define-type Binding
  [binding (name symbol?) (named-expr CFWAE?)])

(define-type CFWAE
  [num (n number?)]
  [binop (op procedure?) (lhs CFWAE?) (rhs CFWAE?)]
  [with (lob (listof Binding?)) (body CFWAE?)]
  [id (name symbol?)]
  [if0 (c CFWAE?) (t CFWAE?) (e CFWAE?)]
  [fun (args (listof symbol?)) (body CFWAE?)]
  [app (f CFWAE?) (args (listof CFWAE?))])

(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (value CFWAE-Value?) (env Env?)])

(define-type CFWAE-Value
  [numV (n number?)]
  [closureV (params (listof symbol?))
            (body CFWAE?)
            (env Env?)])

#|
CFWAE       =       number
|       (+ CFWAE CFWAE)
|       (- CFWAE CFWAE)
|       (* CFWAE CFWAE)
|       (/ CFWAE CFWAE)
|       id
|       (if0 CFWAE CFWAE CFWAE)
|       (with ([id CFWAE] ...) CFWAE)
|       (fun (id ...) CFWAE)
|       (CFWAE CFWAE ...)
|#

(define (safe-/ x y)
  (if (zero? y)
      (error '/ "Division by zero")
      (/ x y)))

(define symbol->op
  (match-lambda
   ['+ +]
   ['- -]
   ['* *]
   ['/ safe-/]
   [_ #f]))

(define (valid-symbol? x)
  (and (symbol? x)
       (not
        (ormap ;;(lambda (this-one)
         ;;  (equal? x this-one))
         ;; (curry f a ...) => (lambda (b ...) (f a ... b ...))
         (curry equal? x)
         '(+ - * / with fun if0)))))

(define (duplicates? l)
  (not
   (= (length l)
      (length (remove-duplicates l)))))

(define (identity-if-not-duplicates l)
  (if (duplicates? l)
      (error 'parse "Duplicated things")
      l))

                                        ; parse : expression -> CFWAE
                                        ; This procedure parses an expression into a CFWAE
(define (parse sexp)
  (match sexp
    [(? number? n)
     (num n)]
    [(list (app symbol->op
                (and (not #f)
                     op))
           lhs-se rhs-se)
     (binop op
            (parse lhs-se)
            (parse rhs-se))]

    [(? valid-symbol? s)
     (id s)]

    [(list 'if0 tst-se tru-se fal-se)
     (if0 (parse tst-se)
          (parse tru-se)
          (parse fal-se))]

    [(list 'with (list [list (? valid-symbol? name)
                             named-expr-se]
                       ...)
           body-se)
     (with (map binding
                (identity-if-not-duplicates name)
                (map parse named-expr-se))

           ;; (map f (list a ...)) = (list (f a) ...)
           ;; (map g (list a ...) (list b ...)) = (list (g a b) ...)
           (parse body-se))]

    [(list 'fun (list (? valid-symbol? arg) ...) body-se)
     (fun (identity-if-not-duplicates arg)
          (parse body-se))]

    [(list fun-se arg-se ...)
     (app (parse fun-se)
          (map parse arg-se))]

    [_
     (error 'parse "Invalid syntax")]))

(parse '4)
(parse 'x)
(test/exn (parse '+) "Invalid")
(parse '(+ 4 4))
(parse '(- 4 4))
(parse '(* 4 4))
(parse '(/ 4 4))
(parse '(if0 4 4 4))
(parse '(fun (x) 4))
(test/exn (parse '(fun (+) 4)) "Invalid")
(test/exn (parse '(fun (x x) 4)) "Duplicated")
(parse '(with ([x 4]) x))
(test/exn (parse '(with ([+ 4]) 4)) "Invalid")
(test/exn (parse '(with ([x 4] [x 5]) 4)) "Duplicated")
(parse '(4))
(parse '(4 4))
(test (parse 'x) (id 'x))
(test (parse '(fun () 1))
      (fun (list) (num 1)))
(test (parse '(fun () x))
      (fun (list) (id 'x)))

(define (lookup-binding n env)
  (type-case
   Env env
   [mtEnv ()
          (error 'interp "Unbound identifier")]
   [anEnv (name named-val more)
          (if (equal? name n)
              named-val
              (lookup-binding n more))]))

(define (lifty op . values)
  (define (pull-out-number v)
    (type-case
     CFWAE-Value v
     [numV (n) n]
     [else (error 'interp "Not a number")]))
  (define numbers
    (map pull-out-number values))
  ;; (apply f (list a ...)) => (f a ...)
  (apply op numbers))

                                        ; interp : CFWAE Env -> CFWAE-Value
                                        ; This procedure interprets the given CFWAE in the environment
                                        ; and produces a result in the form of a CFWAE-Value
(define (interp expr env)
  (type-case
   CFWAE expr
   [num (n) (numV n)]
   [fun (args body) (closureV args body env)]
   [id (s) (lookup-binding s env)]
   [binop (op lhs rhs)
          (numV
           (lifty
            op
            (interp lhs env)
            (interp rhs env)))]
   [if0 (tst tru fal)
        (if (lifty
             zero?
             (interp tst env))
            (interp tru env)
            (interp fal env))]
   [with (binds body)
         (interp body
                 (foldr
                  (lambda (this-bind env-so-far)
                    (anEnv
                     (binding-name this-bind)
                     (interp (binding-named-expr this-bind)
                             env)
                     env-so-far))
                  env
                  binds))]
   [app (fun args)
        (type-case
         CFWAE-Value (interp fun env)
         [closureV (arg-names body saved-env)
                   (if (= (length args)
                          (length arg-names))
                       (interp body
                               (foldr
                                (lambda (arg-name arg env-so-far)
                                  (anEnv
                                   arg-name
                                   (interp arg
                                           env)
                                   env-so-far))
                                env
                                arg-names
                                args))
                       (error 'interp "Wrong number of arguments"))]
         [else
          (error 'interp "Not a function")])]))

(define (interp* e)
  (define ans (interp e (mtEnv)))
  (type-case
   CFWAE-Value ans
   [numV (n) n]
   [else ans]))

(test (interp* (parse '4)) 4)
(test (interp* (parse '(+ 4 4))) 8)
(test/exn (interp* (parse '(+ 4 (fun (x) x)))) "Not a number")
(test (interp* (parse '(- 4 4))) 0)
(test/exn (interp* (parse '(- 4 (fun (x) x)))) "Not a number")
(test (interp* (parse '(* 4 4))) 16)
(test/exn (interp* (parse '(* 4 (fun (x) x)))) "Not a number")
(test (interp* (parse '(/ 4 4))) 1)
(test/exn (interp* (parse '(/ 4 (fun (x) x)))) "Not a number")
(test/exn (interp* (parse '(/ 4 0))) "Division by zero")
(test (interp* (parse '(with ([x 4] [z 1])
                             (with ([x 5] [y (+ x z)] [z 2])
                                   (+ x y)))))
      10)
;; (apply f a ... (list b ...)) => (f a ... b ...)
(test (interp* (parse '(if0 0 1 2))) 1)
(test (interp* (parse '(if0 1 1 2))) 2)
(test (interp* (parse '(if0 2 1 2))) 2)
(test/exn (interp* (parse '(if0 (fun (x) x) 1 2))) "Not a number")

(test (interp* (parse '(fun (x) x)))
      (closureV (list 'x) (id 'x) (mtEnv)))
(test (interp* (parse '((fun (x) x) 4)))
      4)
(test/exn (interp* (parse '((fun () x) 4)))
          "Wrong number of arguments")
(test (interp* (parse '(with ([x 4]
                              [z 1])
                             ((fun (x y z) (+ x y))
                              5
                              (+ x z)
                              2))))
      10)

