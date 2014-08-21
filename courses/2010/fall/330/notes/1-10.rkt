#lang plai
(require (only-in racket
                  hash-ref hasheq))
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
    (symbol=? 'with s))))

(test (valid-id? '+) #f)
(test (valid-id? '*) #f)
(test (valid-id? '/) #f)
(test (valid-id? '-) #f)
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
          (= (length se) 3)
          (symbol? (first se))
          (lookup-op (first se)))
     (binop (lookup-op (first se))
            (parse (second se))
            (parse (third se)))]
; 	 	|	 	(with ([id WAE] ...) WAE)
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
           (second se)))
     (local [(define (sexpr->binding se)
               (binding (first se)
                        (parse (second se))))]
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
(test (parse '(- 1 1)) (binop - (num 1) (num 1)))
(test (parse '(/ 1 1)) (binop / (num 1) (num 1)))
(test (parse '(* 1 1)) (binop * (num 1) (num 1)))

(test/exn (parse '(% 1 1)) "Not valid")
(test/exn (parse '(+ 1 1 1)) "Not valid")
(test/exn (parse '(+ 1)) "Not valid")

(test (parse '(with ([x 1] [y 1]) 1))
      (with (list (binding 'x (num 1))
                  (binding 'y (num 1)))
        (num 1)))
; Multiple occurences pass parse
(test (parse '(with ([x 1] [x 1]) 1))
      (with (list (binding 'x (num 1))
                  (binding 'x (num 1)))
        (num 1)))

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

(test/exn (parse #t) "Not valid")

; find-binding : symbol listof(Binding) -> Binding/#f
(define (find-binding name lob)
  (cond [(empty? lob)
         #f]
        [else
         (if (symbol=? name (binding-name (first lob)))
             (first lob)
             (find-binding name (rest lob)))]))

(test (find-binding 'x empty)
      #f)
(test (find-binding 'x (list (binding 'x (num 1))))
      (binding 'x (num 1)))
(test (find-binding 'x (list (binding 'y (num 1))
                             (binding 'x (num 1))))
      (binding 'x (num 1)))
(test (find-binding 'x (list (binding 'x (num 1))
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
                  (find-binding name lob))]
        (if matching-binding
            (binding-named-expr matching-binding)
            e))]
    [with (nested-lob body)
      (local [(define (subst*-in-binding b)
                (binding (binding-name b)
                         (subst* lob (binding-named-expr b))))
              (define (binding-does-not-occur-in-nested-lob? b)
                (andmap
                 (λ (nested-b)
                   (not (symbol=? (binding-name b)
                                  (binding-name nested-b))))
                 nested-lob))
              (define filtered-lob
                (filter 
                 binding-does-not-occur-in-nested-lob?
                 lob))]
        (with 
            (map subst*-in-binding nested-lob)
          (subst* filtered-lob body)))]))

(test (subst* (list (binding 'x (num 1))) (num 1))
      (num 1))
(test (subst* (list (binding 'x (num 1))) (id 'x))
      (num 1))
(test (subst* (list (binding 'x (num 1))) (id 'y))
      (id 'y))
(test (subst* (list (binding 'x (num 1))
                    (binding 'y (num 2))) (id 'y))
      (num 2))
(test (subst* (list (binding 'x (num 1))
                    (binding 'y (num 2)))
              (parse '(with ([x 3] [z x]) 
                        (+ x (+ y z)))))
      (parse '(with ([x 3] [z 1])
                (+ x (+ 2 z)))))
; XXX skip the rest

; duplicate-bindings? : lob -> bool
(define (duplicate-bindings? lob)
  (define bns (map binding-name lob))
  (not (equal? bns (remove-duplicates bns))))

; calc : WAE -> number
(define (calc e)
  (type-case WAE e
    [num (n) 
         n]
    [binop (op lhs rhs)
           (op (calc lhs) (calc rhs))]
    [with (lob body)
      (if (duplicate-bindings? lob)
          (error 'calc "Duplicate bindings")
          (local [(define (evaluate-binding b)
                    (binding (binding-name b)
                             (num (calc (binding-named-expr b)))))
                  (define evaluated-lob
                    (map evaluate-binding lob))]
            (calc (subst* evaluated-lob body))))]
    [id (name)
        (error 'calc "Unbound identifier: ~e" name)]))
      
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

(test/exn (calc (parse '(with ([x 1] [x 2]) (+ x y))))
      "Duplicate")
(test/exn (calc (parse '(with ([x 1] [y x]) (+ x y))))
      "Unbound")

