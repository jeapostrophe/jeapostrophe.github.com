#lang plai
(print-only-errors #t)

(define-type Binding
  [binding (name symbol?) (named-expr AE?)])

(define-type AE
  [num (n number?)]
  [binop (op procedure?)
         (lhs AE?)
         (rhs AE?)]
  [id (sym symbol?)]
  [with (lob (listof Binding?))
        (body AE?)])

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
;;       | (with ([<id> <AE>]) <AE>)

;; where <id> is any Racket symbol, except +, -, *, /, and with

;; parse-binding : s-expression -> Binding
(define (parse-binding se)
  (cond
    [(and (list? se)
       (= 2 (length se))
       (valid-id? (first se)))
     (binding (first se)
              (parse (second se)))]
    [else
     (error 'parse-binding "Invalid syntax")]))

(define op-table
  (list (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)))

(define (lookup-op/table op op-table)
  (cond [(empty? op-table)
         #f]
        [(equal? op (first (first op-table)))
         (second (first op-table))]
        [else
         (lookup-op/table op (rest op-table))]))

(define (lookup-op op)
  (lookup-op/table op op-table))

(test (lookup-op '+) +)
(test (lookup-op 'modulo) #f)

(define (valid-id? se)
  (and (symbol? se)
       (not (equal? se 'with))
       (not (lookup-op se))))

;; parse :: s-expression -> AE
(define (parse se)
  (cond
    [(and (list? se)
          (= 3 (length se))
          (equal? 'with (first se))
          (list? (second se)))
     (with (map parse-binding (second se))
           (parse (third se)))]
    [(valid-id? se)
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
(test/exn (parse '+)
          "Invalid syntax")
(test/exn (parse '(with ([+ 5]) (+ + +)))
          "Invalid syntax")

;; binding-same-as? : symbol binding -> boolean
(define (binding-same-as? name b)
  (equal? (binding-name b) name))

;; subst-in-binding : name named-thing binding -> binding
(define (subst-in-binding name named-thing b)
  (type-case 
   Binding b
   [binding
    (inner-name inner-named-thing)
    (binding inner-name
             (subst name named-thing inner-named-thing))]))

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
         (with (map (λ (∘∘∘∘)
                      (subst-in-binding name named-thing ∘∘∘∘))
                    inner-lob)
               (if (ormap ;; (curry binding-same-as? name)
                          ;; same as
                          (λ (x) (binding-same-as? name x))
                          inner-lob)
                 body
                 (subst name named-thing body)))]
   [binop (op lhs rhs)
        (binop op
               (subst name named-thing lhs)
               (subst name named-thing rhs))]
   [num (n)
        place-to-look-for-names]))

(define (subst/binding b body)
  (type-case
   Binding b
   [binding
    (name named-thing)
    (subst name named-thing body)]))

(define (calc-in-binding b)
  (type-case
   Binding b
   [binding
    (name named-thing)
    (binding name (num (calc named-thing)))]))

(define (subst* lob body)
  (foldr subst/binding body lob))

(define (member e l)
  (cond [(empty? l)
         false]
        [(equal? e (first l))
         true]
        [else
         (member e (rest l))]))

(define (contains-dups? l)
  (cond [(empty? l)
         false]
        [else
         (or (member (first l) (rest l))             
             (contains-dups? (rest l)))]))

(define (duplicated-ids? lob)
  (contains-dups? (map binding-name lob)))

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
    (if (duplicated-ids? lob)
      (error 'calc "dups")
      (calc (subst* (map calc-in-binding lob) body)))]
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
(test (calc* '(* 7 8))
      56)
(test (calc* '(/ 6 2))
      3)

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
(test (calc* '(with ([x (+ 5 5)] [y 7])
                    7))
      7)
(test (calc* '(with ([x (+ 5 5)] [y 7])
                    (+ x 7)))
      17)
(test (calc* '(with ([x (+ 5 5)] [y 7])
                    (+ x y)))
      17)
(test (calc* '(with ([x (+ 5 5)] [y 7])
                    (with ([x (+ y 5)] [y x])
                          (+ x y))))
      22)
(test/exn (calc* '(with ([x (+ 5 5)] [x 7])
                        (with ([x (+ y 5)] [y x])
                              (+ x y))))
          "dup")
