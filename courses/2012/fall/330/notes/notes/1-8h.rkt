#lang plai
(print-only-errors #t)

(define-type Binding
  [binding (name symbol?) (named-expr AE?)])

(define-type AE
  [num (n number?)]
  [binop (op procedure?)
         (lhs AE?)
         (rhs AE?)]
  [with (lob (listof Binding?)) 
        (body AE?)]
  [id (name symbol?)])

(define (add lhs rhs)
  (binop + lhs rhs))
(define (sub lhs rhs)
  (binop - lhs rhs))

;; <AE> := <real Racket number>
;;       | (+ <AE> <AE>)
;;       | (- <AE> <AE>)
;;       | (* <AE> <AE>)
;;       | (/ <AE> <AE>)
;;       | <id>
;;       | (with ([<id> <AE>] ...) <AE>)

;; where <id> is any Racket symbol, except +, -, *, /, and with

(define op-table
  (list (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)))

;; lookup-op : symbol op-table -> (or/c #f procedure)
(define (lookup-op/table op-sym op-table)
  (cond [(empty? op-table)
         #f]
        [(equal? op-sym (first (first op-table)))
         (second (first op-table))]
        [else
         (lookup-op/table op-sym (rest op-table))]))

;; lookup-op : symbol -> (or/c #f procedure)
(define (lookup-op op-sym)
  (lookup-op/table op-sym op-table))

(test (lookup-op '+) +)
(test (lookup-op '%) #f)

(define (valid-id? sym)
  (and (not (lookup-op sym))
       (not (equal? 'with sym))))

;; parse-binding : s-expression -> Binding
(define (parse-binding se)
  (cond
    [(and (list? se)
          (= 2 (length se))
          (symbol? (first se))
          (valid-id? (first se)))
     (binding (first se)
              (parse (second se)))]
    [else
     (error 'parse "Invalid syntax, dude")]))

;; parse :: s-expression -> AE
(define (parse se)
  (cond
    [(and (list? se)
          (= 3 (length se))
          (equal? 'with (first se))
          (list? (second se)))
     (with (map parse-binding (second se))
           (parse (third se)))]
    [(and (symbol? se)
          (valid-id? se))
     (id se)]
    [(number? se)
     (num se)]
    [(and (list? se)
          (= 3 (length se))
          (lookup-op (first se)))
     (binop (lookup-op (first se))
            (parse (second se))
            (parse (third se)))]
    [else
     (error 'parse "Invalid syntax, dude")]))

(test (parse '1)
      (num 1))
(test (parse '(+ 1 1))
      (add (num 1) (num 1)))
(test (parse '(- 1 1))
      (sub (num 1) (num 1)))
(test (parse 'x)
      (id 'x))
(test (parse '(with ([x 27]) x))
      (with (list (binding 'x (num 27))) (id 'x)))

(test/exn (parse "1")
          "Invalid syntax")

(define (subst-in-binding name named-thing a-binding)
  (type-case
   Binding a-binding
   [binding
    (inner-name inner-named-thing)
    (binding inner-name
             (subst name named-thing inner-named-thing))]))

(define (binding-same-as-this? name a-binding)
  (type-case
   Binding a-binding
   [binding
    (inner-name inner-named-thing)
    (equal? inner-name name)]))

;; subst : name named-thing place-to-look-for-names
;;      -> place-to-look-for-names/with-no-more-name-in-it
(define (subst name named-thing place-to-look-for-names)
  (type-case
   AE place-to-look-for-names
   [id (sym)
       (if (equal? name sym)
         named-thing
         place-to-look-for-names)]
   [with (inner-lob body)
         (with (map (λ (....)
                      (subst-in-binding name named-thing ....))
                    inner-lob)
               (if (ormap (curry binding-same-as-this? name)
                          inner-lob)
                 body
                 (subst name named-thing body)))]
   [binop (op lhs rhs)
        (binop op
               (subst name named-thing lhs)
               (subst name named-thing rhs))]   
   [num (n)
        place-to-look-for-names]))

(define (calc/binding b)
  (type-case 
   Binding b
   [binding
    (name named-thing)
    (binding name (num (calc named-thing)))]))

(define (subst/binding b body)
  (type-case 
   Binding b
   [binding
    (name named-thing)
    (subst name named-thing body)]))

(define (subst* lob body)
  (foldr subst/binding body lob))

;; calc : AE? -> number?
;; compute the meaning of the AE
(define (calc ae)
  (type-case
   AE ae
   [id
    (sym)
    (error 'calc "You has a bad identifier, bro")]
   [with
    (lob body)
    (calc (subst* (map calc/binding lob) body))]
   [num
    (the-most-excellent-number)
    the-most-excellent-number]
   [binop
    (op lhs rhs)
    (op (calc lhs)
        (calc rhs))]))

;; calc* : sexpr -> number?
(define (calc* se)
  (calc (parse se)))

(test (calc* '1)
      1)
(test (calc* '(+ 1 1))
      2)
(test (calc* '(- 0 1))
      -1)
(test (calc* '(* 18 2))
      36)
(test (calc* '(/ 4 2))
      2)

(test (calc* '(with ([x (+ 5 5)])
                    (+ x x)))
      20)
(test (calc* '(with ([y 7])
                    (with ([x y])
                          (+ x x))))
      14)
(test (calc* '(with ([x (+ 5 5)])
                    (with ([x 7])
                          (+ x x))))
      14)
(test (calc* '(with ([x (+ 5 5)])
                    (+ x (with ([x 7])
                               (+ x x)))))
      24)
(test (calc* '(with ([x 7])
                    (with ([y (+ 2 x)])
                          (+ y 3))))
      12)
(test (calc* '(with ([y 7])
                    ;; Shadowing
                    (with ([y (+ y 2)])
                          (+ y 3))))
      12)
(test (calc* '(with ([x (+ 5 5)])
                    7))
      7)

(test (calc* '(with ()
                    7))
      7)
(test (calc* '(with ([x (+ 5 5)] [y 0])
                    7))
      7)
(test (calc* '(with ([x (+ 5 5)] [y 2])
                    (+ x y)))
      12)
(test (calc* '(with ([x (+ 5 5)] [y 2])
                    (with ([y (+ x 1)])
                          (+ x y))))
      21)
(test (calc* '(with ([x (+ 5 5)] [y 2])
                    (with ([y (+ x 1)] [x (+ 10 9)])
                          (+ x y))))
      30)
