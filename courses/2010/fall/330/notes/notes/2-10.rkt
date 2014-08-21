#lang plai
(halt-on-errors #t)

(define-type Binding
  [binding (name symbol?) (named-expr WAE?)])

(define-type WAE
  [num (n number?)]
  [binop (op procedure?)
         (lhs WAE?)
         (rhs WAE?)]
  [with (lob (listof Binding?)) (body WAE?)]
  [id (name symbol?)])

; op-table : listof (list symbol procedure)
(define op-table
  (list (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)))

; lookup-op : symbol -> procedure/#f
(define (lookup-op t)
  (define maybe-list (assoc t op-table))
  (if maybe-list
      (second maybe-list)
      #f))

(test (lookup-op '+) +)
(test (lookup-op '/) /)
(test (lookup-op '-) -)
(test (lookup-op '*) *)
(test (lookup-op 'harold) #f)

; valid-id? : sym -> bool
(define (valid-id? s)
  (not (or (symbol=? 'with s)
           (lookup-op s))))

(test (valid-id? '+) #f)
(test (valid-id? '/) #f)
(test (valid-id? '-) #f)
(test (valid-id? '*) #f)
(test (valid-id? 'with) #f)
(test (valid-id? 'x) #t)

; parse : sexpr -> WAE
(define (parse se)
  (cond
; WAE	 	=	 	number
    [(number? se)
     (num se)]     
; 	 	|	 	(+ WAE WAE)     
; 	 	|	 	(- WAE WAE)
; 	 	|	 	(* WAE WAE)
; 	 	|	 	(/ WAE WAE)
    [(and (list? se)
          (= 3 (length se))
          (symbol? (first se))
          (lookup-op (first se)))
     (binop (lookup-op (first se))
            (parse (second se))
            (parse (third se)))]
; 	 	|	 	(with ([id WAE] ...) WAE)
    [(local [(define (binding-sexp? bse)
               (and (list? bse)
                    (= 2 (length bse))
                    (symbol? (first bse))
                    (valid-id? (first bse))))]
       (and (list? se)
            (= 3 (length se))
            (symbol? (first se))
            (symbol=? 'with (first se))
            (list? (second se))
            (andmap binding-sexp?
                    (second se))))
     (local [(define (sexpr->binding bse) ; bse is shaped like [id WAE]
               (binding (first bse)
                        (parse (second bse))))]
       (with (map sexpr->binding (second se))
         (parse (third se))))]
; 	 	|	 	id
    [(and (symbol? se)
          (valid-id? se))
     (id se)]
    [else
     (error 'parse "Not valid WAE syntax: ~e" se)]))

(test (parse '5) (num 5))
(test (parse '(+ 1 1)) (binop + (num 1) (num 1)))
(test (parse '(* 1 1)) (binop * (num 1) (num 1)))
(test (parse '(- 1 1)) (binop - (num 1) (num 1)))
(test (parse '(/ 1 1)) (binop / (num 1) (num 1)))
(test/exn (parse '(1 1 1)) "Not valid")
(test/exn (parse '(% 1 1)) "Not valid")
(test/exn (parse '(+ 1)) "Not valid")
(test/exn (parse '(+ 1 1 1)) "Not valid")
(test (parse '(with ([x 1] [y 2]) 3))
      (with (list (binding 'x (num 1))
                  (binding 'y (num 2)))
        (num 3)))
; multi occurrnences are not parse errors
(test (parse '(with ([x 1] [x 2]) 3))
      (with (list (binding 'x (num 1))
                  (binding 'x (num 2)))
        (num 3)))
(test (parse '(with ([x 1]) 3))
      (with (list (binding 'x (num 1)))
        (num 3)))
(test (parse '(with () 3))
      (with (list)
        (num 3)))
(test/exn (parse '(with)) "Not valid")
(test/exn (parse '(with () 1 1)) "Not valid")
(test/exn (parse '(with 1 1)) "Not valid")
(test/exn (parse '(with (1) 1)) "Not valid")
(test/exn (parse '(with ([x]) 1)) "Not valid")
(test/exn (parse '(with ([x 1 1]) 1)) "Not valid")
(test/exn (parse '(with ([1 1]) 1)) "Not valid")
(test/exn (parse '(with ([+ 1]) 1)) "Not valid")
(test/exn (parse '(with ([- 1]) 1)) "Not valid")
(test/exn (parse '(with ([/ 1]) 1)) "Not valid")
(test/exn (parse '(with ([* 1]) 1)) "Not valid")
(test/exn (parse '(with ([with 1]) 1)) "Not valid")
(test (parse 'x) (id 'x))
(test/exn (parse '+) "Not valid")
(test/exn (parse '-) "Not valid")
(test/exn (parse '/) "Not valid")
(test/exn (parse '*) "Not valid")
(test/exn (parse 'with) "Not valid")
(test/exn (parse #t) "Not valid")

; lookup-binding : symbol listof(Binding) -> Binding/#f
(define (lookup-binding name lob)
  (cond [(empty? lob)
         #f]
        [else
         (if (symbol=? name (binding-name (first lob)))
             (first lob)
             (lookup-binding name (rest lob)))]))

(test (lookup-binding 'x empty) #f)
(test (lookup-binding 'x (list (binding 'x (num 1)))) 
      (binding 'x (num 1)))
(test (lookup-binding 'x (list (binding 't (num 1))
                               (binding 'x (num 1)))) 
      (binding 'x (num 1)))
(test (lookup-binding 'x (list (binding 'x (num 1))
                               (binding 'x (num 2)))) 
      (binding 'x (num 1)))

; subst* : listof(Binding) WAE -> WAE
(define (subst* lob e)
  (type-case WAE e
    [num (n)
         e]
    [binop (op lhs rhs)
           (binop op 
                  (subst* lob lhs)
                  (subst* lob rhs))]
    [id (name)
        (local [(define matching-binding
                  (lookup-binding name lob))]
          (if 
           ; name is one of the bindings in lob
           matching-binding
           ; return the named expr for that binding
           (binding-named-expr matching-binding)
           ; otherwise return the thing that came in
           e))]
    ; If lob = [x 1] [y 2]
    ; If nested-lob = [x 3]
    ; Then lob-without-things-bound-by-this-with = [y 2]
    [with (nested-lob body)
      (local [(define (not-in-nested-lob? b)
                (not (ormap (λ (nb)
                              (symbol=? (binding-name b)
                                        (binding-name nb)))
                            nested-lob)))
              (define lob-without-things-bound-by-this-with
                (filter not-in-nested-lob? lob))
              (define (subst*-in-binding b)
                (binding (binding-name b)
                         (subst* lob (binding-named-expr b))))
              (define nested-lob/substitutions
                (map subst*-in-binding nested-lob))]
        (with nested-lob/substitutions
          (subst* lob-without-things-bound-by-this-with body)))]))


(test (subst* (list (binding 'x (num 1))
                    (binding 'y (num 2)))
              (parse '(with ([x x]) (+ x y))))
      (parse '(with ([x 1]) (+ x 2))))
      