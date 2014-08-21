#lang plai
(require (only-in racket
                  hash-ref hasheq))
(halt-on-errors #t)

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

; op-table : (hash/c symbol procedure?)
(define op-table
  (hasheq '+ +
          '- -
          '/ /
          '* *))

; lookup-op : symbol -> procedure/#f
(define (lookup-op t)
  (hash-ref op-table t #f))

(test (lookup-op '+) +)
(test (lookup-op '-) -)
(test (lookup-op '*) *)
(test (lookup-op '/) /)
(test (lookup-op '%) #f)

; valid-id? : symbol -> bool
(define (valid-id? s)
  (not 
   (or
    (lookup-op s)
    (symbol=? 'with s)
    (symbol=? 'fun s)
    (symbol=? 'if0 s))))

(test (valid-id? '+) #f)
(test (valid-id? '*) #f)
(test (valid-id? '/) #f)
(test (valid-id? '-) #f)
(test (valid-id? 'with) #f)
(test (valid-id? 'fun) #f)
(test (valid-id? 'if0) #f)
(test (valid-id? 'x) #t)

; no-duplicates? : los -> bool
(define (no-duplicates? los)
  (equal? los (remove-duplicates los)))

(test (no-duplicates? '(x y z)) #t)
(test (no-duplicates? '(x y z x)) #f)

; parse : sexpr -> CFWAE
(define (parse se)
  (cond
    ; CFWAE	 	=	 	number
    [(number? se)
     (num se)]
    ; 	 	|	 	(+ CFWAE CFWAE)    
    ; 	 	|	 	(- CFWAE CFWAE)
    ; 	 	|	 	(* CFWAE CFWAE)
    ; 	 	|	 	(/ CFWAE CFWAE)
    [(and (list? se) 
          (= (length se) 3)
          (symbol? (first se))
          (lookup-op (first se)))
     (binop (lookup-op (first se))
            (parse (second se))
            (parse (third se)))]
    ; 	 	|	 	(if0 CFWAE CFWAE CFWAE)
    [(and (list? se)
          (= 4 (length se))
          (symbol? (first se))
          (symbol=? 'if0 (first se)))
     (if0 (parse (second se))
          (parse (third se))
          (parse (fourth se)))]
    ; 	 	|	 	(with ([id CFWAE] ...) CFWAE)
    [(and (list? se)
          (= (length se) 3)
          (symbol? (first se))
          (symbol=? 'with (first se))
          (list? (second se))
          (andmap
           (λ (se) 
             (and (list? se)
                  (= (length se) 2)
                  (symbol? (first se))
                  (valid-id? (first se))))
           (second se))
          (no-duplicates?
           (map first (second se))))
     (local [(define (sexpr->binding se)
               (binding (first se)
                        (parse (second se))))]
       (with (map sexpr->binding (second se))
         (parse (third se))))]    
    ;	 	|	 	(fun (id ...) CFWAE)
    [(and (list? se)
          (= 3 (length se))
          (symbol? (first se))
          (symbol=? 'fun (first se))
          (list? (second se))
          (andmap (λ (e)
                    (and (symbol? e)
                         (valid-id? e)))
                  (second se))
          (no-duplicates? (second se)))
     (fun (second se) (parse (third se)))]    
    ; 	 	|	 	(CFWAE ...+)
    [(and (list? se)
          (not (empty? se)))
     (app (parse (first se))
          (map parse (rest se)))]
    ; 	 	|	 	id
    [(and (symbol? se)
          (valid-id? se))
     (id se)]
    [else
     (error 'parse "Not valid CFWAE syntax: ~e" se)]))

(test (parse '5) (num 5))

(test (parse '(+ 1 1)) (binop + (num 1) (num 1)))
(test (parse '(- 1 1)) (binop - (num 1) (num 1)))
(test (parse '(/ 1 1)) (binop / (num 1) (num 1)))
(test (parse '(* 1 1)) (binop * (num 1) (num 1)))

(test (parse '(% 1 1))
      (app (id '%) (list (num 1) (num 1))))
(test/exn (parse '(+ 1 1 1)) "Not valid")
(test/exn (parse '(+ 1)) "Not valid")

(test (parse '(with ([x 1] [y 1]) 1))
      (with (list (binding 'x (num 1))
                  (binding 'y (num 1)))
        (num 1)))
(test/exn (parse '(with ([x 1] [x 1]) 1))
          "Not valid")

(test/exn (parse '(with)) "Not valid")
(test/exn (parse '(with () 1 1)) "Not valid")
(test/exn (parse '(with 1 1)) "Not valid")
(test/exn (parse '(with (1) 1)) "Not valid")
(test/exn (parse '(with ((x)) 1)) "Not valid")
(test/exn (parse '(with ((x 1 1)) 1)) "Not valid")
(test/exn (parse '(with ((1 1)) 1)) "Not valid")
(test/exn (parse '(with ((+ 1)) 1)) "Not valid")
(test/exn (parse '(with ((- 1)) 1)) "Not valid")
(test/exn (parse '(with ((* 1)) 1)) "Not valid")
(test/exn (parse '(with ((/ 1)) 1)) "Not valid")
(test/exn (parse '(with ((with 1)) 1)) "Not valid")

(test (parse 'x) (id 'x))

(test/exn (parse '+) "Not valid")
(test/exn (parse '-) "Not valid")
(test/exn (parse '/) "Not valid")
(test/exn (parse '*) "Not valid")
(test/exn (parse 'with) "Not valid")
(test/exn (parse 'fun) "Not valid")
(test/exn (parse 'if0) "Not valid")

(test/exn (parse #t) "Not valid")

; XXX if0, fun, app

; interp : CFWAE Env -> CFWAE-Value
; This procedure interprets the given CFWAE in the environment
; and produces a result in the form of a CFWAE-Value
(define (interp expr env)
  (type-case CFWAE expr
    [num (n) (numV n)]
    [binop (op lhs rhs)
           (type-case CFWAE-Value (interp lhs env)
             [numV (lhs-v)
                   (type-case CFWAE-Value (interp rhs env)
                     [numV (rhs-v)
                           (if (and (equal? op /)
                                    (zero? rhs-v))
                               (error 'interp "Division by zero")
                               (numV (op lhs-v rhs-v)))]
                     [else
                      (error 'interp "Not a number")])]
             [else
              (error 'interp "Not a number")])]
    [if0 (test-e true-e false-e)
         (type-case CFWAE-Value (interp test-e env)
           [numV (test-v)
                 (if (zero? test-v)
                     (interp true-e env)
                     (interp false-e env))]
           [else
            (error 'interp "Not a number")])]
    [fun (as body)
         (closureV as body env)]
    [id (name)
        (lookup-env name env)]
    [with (lob body)
      (interp body 
              (foldr (λ (b rest-env)
                       (anEnv (binding-name b)
                              (interp (binding-named-expr b) env)
                              rest-env))
                     env
                     lob))]
    [app (f-expr loa-exprs)
         (type-case CFWAE-Value (interp f-expr env)
           [closureV (as body fun-env)
                     (interp body
                             (foldr (λ (a ae rest-env)
                                      (anEnv a
                                             (interp ae env)
                                             rest-env))
                                    fun-env
                                    as
                                    loa-exprs))]
           [else
            (error 'interp "Not a function")])]))

(define (lookup-env name env)
  (type-case Env env
    [mtEnv () (error 'interp "Unbound id")]
    [anEnv (some-name some-val inner-env)
           (if (symbol=? name some-name)
               some-val
               (lookup-env name inner-env))]))
  
; calc : CFWAE -> number
(define (calc e)
  (type-case CFWAE-Value (interp e (mtEnv))
    [numV (n) n]
    [closureV (a b e) (closureV a b e)]))

(test (calc (parse '5)) 5)
(test (calc (parse '(+ 1 1))) 2)
(test (calc (parse '(- 1 1))) 0)
(test (calc (parse '(* 1 1))) 1)
(test (calc (parse '(/ 1 1))) 1)
(test/exn (calc (parse 'x)) "Unbound")

(test (calc (parse '(with ([x 1] [y 2]) (+ x y))))
      3)
(test (calc (parse '(with ([x 1] [y 2])
                      (with ([x 3] [y x])
                        (+ x y)))))
      4)
(test (calc (parse '(with ([x (+ 1 0)] [y 2])
                      (with ([x 3] [y x])
                        (+ x y)))))
      4)

#;(test/exn (calc (parse '(with ([x 1] [x 2]) (+ x y))))
          "Duplicate")
(test/exn (calc (parse '(with ([x 1] [y x]) (+ x y))))
          "Unbound")

(test (calc (parse '(with ([Y
                            (fun (f)
                                 ((fun (x) (f (fun (y) ((x x) y))))
                                  (fun (x) (f (fun (y) ((x x) y))))))])
                      (with ([aac 
                              (Y (fun (aac)
                                      (fun (x)
                                           (if0 x
                                                0
                                                (+ x (aac (- x 1)))))))])
                        (aac 7)))))
      (+ 7 6 5 4 3 2 1 0))