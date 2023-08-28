#lang plai
(halt-on-errors #t)

(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)]
  [id (sym symbol?)]
  [with (name symbol?)
        (named-thing AE?)
        (body AE?)])

;; <AE> := <real Racket number>
;;       | (+ <AE> <AE>)
;;       | (- <AE> <AE>)
;;       | <id>
;;       | (with [<id> <AE>] <AE>)
;; where <id> is a Racket symbol other than +, -, with

;; parse :: s-expression -> AE
(define (parse se)
  (cond
    [(and (list? se)
          (= 3 (length se))
          (equal? 'with (first se))
          (list? (second se))
          (= 2 (length (second se)))
          (symbol? (first (second se))))
     (with (first (second se))
           (parse (second (second se)))
           (parse (third se)))]
    [(symbol? se)
     (id se)]
    [(number? se)
     (num se)]
    [(and (list? se)
          (= 3 (length se))
          (equal? '+ (first se)))
     (add (parse (second se))
          (parse (third se)))]
    [(and (list? se)
          (= 3 (length se))
          (equal? '- (first se)))
     (sub (parse (second se))
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
(test (parse '(with [x 5] x))
      (with 'x (num 5) (id 'x)))

(test/exn (parse "1")
          "Invalid syntax")

;; Can you teach with to a 6th grader?

;; Everywhere you see an x in the bottom part (the body) take the
;; named thing and stick it where x was.

;; OR

;; Andrew's substitution: Find all "name"s and replace them with the
;; "named-thing" in the "body"

;; Bound instance : An identifier occurrence that refers to a previous
;; binding

;; Binding instance : An occurrence of an identifier that creates a
;; binding of that identifier to a value/expression

;; Unbound instance (Free): An identifier occurrence that does /not/ refer
;; to a previous binding.

;; Substitution': Find all "name"s that are not binding instances and
;; replace them with the "named-thing" in the "body"

;; Scope: The program text for which a binding instance is valid

;; Substitution'': Find all "name"s that are not binding instances and
;; replace them with the "named-thing" in the "body", unless they are
;; in a different scope.

;; Substitution''': Find all "name"s that are not binding instances
;; and replace them with the "named-thing" in the "body", unless they
;; are in a different scope that uses the same name.

;; Substition'''': Replace all free instances of an identifier with
;; its binding.

;; subst : name named-thing 
;;         a-place-to-look-for-the-name-and-replace-it-with-the-named-thing
;;      -> the-same-place-with-all-the-names-gone
(define (subst name named-thing 
               a-place-to-look-for-the-name-and-replace-it-with-the-named-thing)
  (type-case
   AE a-place-to-look-for-the-name-and-replace-it-with-the-named-thing
   [num
    (n)
    (num n)]
   [add
    (lhs rhs)
    (add (subst name named-thing lhs)
         (subst name named-thing rhs))]
   [sub
    (lhs rhs)
    (sub (subst name named-thing lhs)
         (subst name named-thing rhs))]
   [with
    (inner-name inner-named-thing body)
    (if (equal? inner-name name)
      (with inner-name
            (subst name named-thing inner-named-thing)
            body)
      (with inner-name
            (subst name named-thing inner-named-thing)
            (subst name named-thing body)))]
   [id
    (sym)
    (if (equal? sym name)
      named-thing
      (id sym))]))

;; calc : AE? -> number?
;; compute the meaning of the AE
(define (calc ae)
  (type-case 
   AE ae
   [num
    (the-most-excellent-number)
    the-most-excellent-number]
   [add
    (lhs rhs)
    (+ (calc lhs)
       (calc rhs))]
   [sub
    (lhs rhs)
    (- (calc lhs)
       (calc rhs))]
   [with
    (name named-thing body)
    ;; Inefficient -- Lazy*
    ;; (calc (subst name named-thing body))
    ;;   Efficient -- Eager
    (calc (subst name (num (calc named-thing)) body))]
   [id
    (sym)
    (error 'calc "You cannot has ~e" sym)]))

;; calc* : sexpr -> number?
(define (calc* se)
  (calc (parse se)))

(test (calc* '1)
      1)
(test (calc* '(+ 1 1))
      2)
(test (calc* '(- 0 1))
      -1)
(test (calc* '(+ (+ 5 5)
                 (+ 5 5)))
      20)

;; DWMFT -- Don't waste my finger's time!

;; DRY -- Don't repeat yourself

(test (calc* '(with [x (+ 5 5)]
                    (+ x x)))
      20)
(test (calc* '(with [x (+ 5 5)]
                    (+ x 
                       (with [x 7]
                             (+ x (+ x x))))))
      31)
(test (calc* '(with [x (+ 5 5)]
                    (with [y 7]
                          (+ y (+ x x)))))
      27)

(test (calc* '(with [x (+ 5 5)]
                    ;; Shadowing
                    (+ (with [x 7]
                             (+ x (+ x x)))
                       x)))
      31)

(test (calc* '(with [x (+ 5 5)]
                    (with [y x]
                          (+ y (+ x x)))))
      30)
(test (calc* '(with [x (+ 5 5)]
                    (with [y (+ 1 x)]
                          (+ y (+ y y)))))
      33)
;; Shadowing
(test (calc* '(with [x (+ 5 5)]
                    (with [x (+ 1 x)]
                          (+ x (+ x x)))))
      33)

(test (calc* '(with [x (+ 5 5)]
                    33))
      33)

;; Static Stack Distance (compilers)
;; De Brujin index (theoretical)

;; (test (calc* '(with (+ 5 5)
;;                     (with (+ 1 <0>)
;;                           (+ <0> (+ <1> <0>)))))
;;       32)
