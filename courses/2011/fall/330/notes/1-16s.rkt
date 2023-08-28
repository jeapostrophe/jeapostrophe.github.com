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
CFWAE	 	=	 	number
 	 	|	 	(+ CFWAE CFWAE)
 	 	|	 	(- CFWAE CFWAE)
 	 	|	 	(* CFWAE CFWAE)
 	 	|	 	(/ CFWAE CFWAE)
 	 	|	 	id
 	 	|	 	(if0 CFWAE CFWAE CFWAE)
 	 	|	 	(with ([id CFWAE] ...) CFWAE)
 	 	|	 	(fun (id ...) CFWAE)
 	 	|	 	(CFWAE CFWAE ...)
|#

(define (valid-symbol? x)
  (and (symbol? x)
       (not (ormap (curry equal? x)
                   ;; Point-free style
                   ;; or pointless style
                   ;;(lambda (invalid) (equal? x invalid))
                   ;;(curry f a ...) => (lambda (b ...) (f a ... b ...))
                   '(+ - * / with fun if0)))))

(define (duplicates? l)
  (define-values
    (final-seen? any-dups?)
    (for/fold ([seen? (hash)]
               [any-dups? #f])
        ([e (in-list l)])
      (if (or any-dups? (hash-has-key? seen? e))
          (values seen?
                  #t)
          (values (hash-set seen? e #t)
                  #f))))
  any-dups?)

(define (simple-duplicates? l)
  (not (equal? l (remove-duplicates l))))

(define (complicated-duplicates? l)
  (ormap (lambda (x)
           (< 1 (count (curry equal? x) l)))
         l))

(define (identity-if-not-duplicates l)
  (if (empty? l)
      l
      (if (duplicates? l)
          (error 'parse "Duplicated ids")
          l)))

(define (safe-/ x y)
  (if (zero? y)
      (error 'interp "Division by zero")
      (/ x y)))

; parse : expression -> CFWAE
; This procedure parses an expression into a CFWAE
(define (parse sexp)
  (match sexp
    [(? number? n)
     (num n)]
    ;; =>
    ;;[(number? sexp)
    ;; (local [(define n sexp)]
    ;;   (num n))]
    [(list '+ (app parse lhs) (app parse rhs))
     (binop + lhs rhs)]
    ;; =>
    ;; [(and (list? sexp)
    ;;       (= 3 (length sexp))
    ;;       (equal? '+ (first sexp)))
    ;;  (local [(define lhs (parse (second sexp)))
    ;;          (define rhs (parse (third sexp)))]
    ;;         (binop + lhs rhs))]
    
    [(list '- (app parse lhs) (app parse rhs))
     (binop - lhs rhs)]
    [(list '* (app parse lhs) (app parse rhs))
     (binop * lhs rhs)]     
    [(list '/ (app parse lhs) (app parse rhs))
     (binop safe-/ lhs rhs)]

    [(? valid-symbol? the-sym)
     (id the-sym)]

    [(list 'if0 (app parse tst) (app parse tru) (app parse fal))
     (if0 tst tru fal)]

    [(list 'with (list (list (? valid-symbol? name)
                             (app parse named-expr))
                       ...)
           (app parse body))
     (with (map binding ;; name E -> Binding
                (identity-if-not-duplicates name) ;; (listof name)
                named-expr) ;; (listof E)                
           body)]
     ;; (map f (list a ...)) => (list (f a) ...)
     ;; (map f (list a ...) (list b ...)) => (list (f a b) ...)


    [(list 'fun (list (? valid-symbol? arg-name) ...)
           (app parse body))
     (fun (identity-if-not-duplicates arg-name)
          body)]

    [(list (app parse fun-expr)
           (app parse arg-expr)
           ...)
     (app fun-expr arg-expr)]

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
(test (identity-if-not-duplicates empty)
      empty)
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

(define (lifty op . argVs)
  (define argNs
    (map (lambda (argV)
           (type-case
            CFWAE-Value argV
            [numV (n) n]
            [else
             (error 'interp "Not a number")]))
         argVs))
  (apply op argNs))

; interp : CFWAE Env -> CFWAE-Value
; This procedure interprets the given CFWAE in the environment
; and produces a result in the form of a CFWAE-Value
(define (interp expr env)
  (type-case
   CFWAE expr
   [num (n)
        (numV n)]
   [binop (op lhs rhs)
          (numV
           (lifty
            op
            (interp lhs env)
            (interp rhs env)))]
   [with (binds body)
         (interp body
                 (foldr
                  (lambda (this-guy the-rest-of-the-new-env)
                    (anEnv
                     (binding-name this-guy)
                     (interp (binding-named-expr this-guy) env)
                     the-rest-of-the-new-env))
                  env
                  binds))]
   [id (n)
       (lookup-binding n env)]
   [if0 (tst tru fal)
        (if (lifty zero? (interp tst env))
            (interp tru env)
            (interp fal env))]
   [fun (args body)
        (closureV args body env)]
   [app (fun-expr arg-exprs)
        (type-case
         CFWAE-Value (interp fun-expr env)
         [closureV (args body saved-env)
                   (if (= (length args)
                          (length arg-exprs))
                       (interp
                        body
                        (foldr
                         (lambda (arg arg-expr the-rest-of-the-new-env)
                           (anEnv
                            arg
                            (interp arg-expr env)
                            the-rest-of-the-new-env))
                         env
                         args
                         arg-exprs))
                       (error 'interp "Not right number of arguments"))]
         [else
          (error 'interp "Not a function")])]))

(define (interp* e)
  (define ans
    (interp e (mtEnv)))
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
          "Not right number of arguments")
(test (interp* (parse '(with ([x 4]
                              [z 1])
                             ((fun (x y z) (+ x y))
                              5
                              (+ x z)
                              2))))
      10)
      
