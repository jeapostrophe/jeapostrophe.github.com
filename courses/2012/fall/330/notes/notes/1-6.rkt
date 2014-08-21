#lang plai
(halt-on-errors #t)

(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)]
  [id (sym symbol?)]
  [with (new-name symbol?)
        (named-thing AE?)
        (body AE?)])

;; <AE> := <real Racket number>
;;       | (+ <AE> <AE>)
;;       | (- <AE> <AE>)
;;       | <id>
;;       | (with [<id> <AE>] <AE>)

;; where <id> is any Racket symbol, except +, -, and with

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
(test (parse '(with [x 27] x))
      (with 'x (num 27) (id 'x)))

(test/exn (parse "1")
          "Invalid syntax")

;; f(x) = x + 27
;; Replace all Xs with 19
;; f(19) = 19 + 27 

;; Substitution : Replace all names with the named thing (Andrew's)

;; Substitution' : Replace all names, except the naming names, with
;; the named thing (Namingway's)

;; Binding instances -- The occurrences of identifiers that create the
;; names

;; Bound instances -- The occurrences of identifiers that refer to
;; binding instances

;; Scope -- The program text where bound instances refer to a
;; particular binding instance

;; Unbound instances -- The occurrences of identifiers that would
;; refer to binding instances, but don't because there is no
;; corresponding definition.

;; Substitution' (rephrase) : Replace all bound instances with
;; the named thing (Namingway's)

;; Substitution'' : Replace all bound instances in the scope of the
;; binding instance with the named thing (Jared)

;; Substitution''' : Replace all free identifiers in the scope of the
;; binding instance with the named thing.

;; subst : name named-thing place-to-look-for-names 
;;      -> place-to-look-for-names/with-no-more-name-in-it
(define (subst name named-thing place-to-look-for-names)
  (type-case 
   AE place-to-look-for-names
   [id (sym)
       (if (equal? name sym)
         named-thing
         place-to-look-for-names)]
   [with (inner-name inner-named-thing body)
         (if (equal? inner-name name)
           (with inner-name
                 (subst name named-thing inner-named-thing)
                 body)
           (with inner-name
                 (subst name named-thing inner-named-thing)
                 (subst name named-thing body)))]
   [add (lhs rhs)
        (add (subst name named-thing lhs)
             (subst name named-thing rhs))]
   [sub (lhs rhs)
        (sub (subst name named-thing lhs)
             (subst name named-thing rhs))]
   [num (n)
        place-to-look-for-names]))

;; calc : AE? -> number?
;; compute the meaning of the AE
(define (calc ae)
  (type-case 
   AE ae
   [id
    (sym)
    (error 'calc "You has a bad identifier, bro")]
   [with
    (name named-thing body)
    ;; Inefficient -- Lazy' semantics
    ;; (calc (subst name named-thing body))
    ;; Efficient -- Eager semantics
    (calc (subst name (num (calc named-thing)) body))]
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

(test (calc* '(with [x (+ 5 5)]
                    (+ x x)))
      20)
(test (calc* '(with [y 7]
                    (with [x y]
                          (+ x x))))
      14)
(test (calc* '(with [x (+ 5 5)]
                    (with [x 7]
                          (+ x x))))
      14)
(test (calc* '(with [x (+ 5 5)]
                    (+ x (with [x 7]
                               (+ x x)))))
      24)
(test (calc* '(with [x 7]
                    (with [y (+ 2 x)]
                          (+ y 3))))
      12)
(test (calc* '(with [y 7]
                    ;; Shadowing
                    (with [y (+ y 2)]
                          (+ y 3))))
      12)
(test (calc* '(with [x (+ 5 5)]
                    7))
      7)


;; Static Stack Distance (compilers) OR De Bruijn Index (theoretical)

;; (test (calc* '(with 7
;;                     ;; Shadowing
;;                     (with (+ <0> 2)
;;                           (+ <0> <1>))))
;;       16)
