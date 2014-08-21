#lang plai
(halt-on-errors #t)

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?)
       (rhs WAE?)]
  [sub (lhs WAE?)
       (rhs WAE?)]
  [with (name symbol?)
    (named-expr WAE?)
    (body WAE?)]
  [id (name symbol?)])

; <WAE> :==
;         | <number>
;         | (+ <WAE> <WAE>)
;         | (- <WAE> <WAE>)
;         | (with [<id> <WAE>] <WAE>)
;         | <id>
; where id is a symbol

; parse : Sexpr -> WAE
; Purpose: To accept WAE style Sexprs and turn them into WAEs
(define (parse se)
  (cond [(number? se) (num se)]
        [(symbol? se) (id se)]
        [(and (list? se) (symbol=? 'with (first se)))
         (with (first (second se))
           (parse (second (second se)))
           (parse (third se)))]
        [(and (list? se) (symbol=? '+ (first se)))
         (add (parse (second se))
              (parse (third se)))]
        [(and (list? se) (symbol=? '- (first se)))
         (sub (parse (second se))
              (parse (third se)))]))

(test (parse '(+ 1 2))
      (add (num 1) (num 2)))
(test (parse '(- 1 2))
      (sub (num 1) (num 2)))
(test (parse 'x)
      (id 'x))
(test (parse '(with [x (+ 1 1)] x))
      (with 'x (add (num 1) (num 1)) (id 'x)))

; subst : symbol WAE WAE -> WAE
; Bad Purpose 1: Replace name-to-subst with expr-to-subst inside subst-inside-expr

; Definition: a binding instance is the 'name' of a 'with'
; Bad Purpose 2: Replace name-to-subst, unless it is a binding instance, with expr-to-subst inside subst-inside-expr

; Bad Purpose 3: Replace name-to-subst, unless it is a binding instance or inside a with, with expr-to-subst inside subst-inside-expr

; Definition: scope is the syntactic region of the program source code where an identifier established by a with is bound
; Bad Purpose 4: Replace name-to-subst, unless it is a binding instance or inside a different scope, with expr-to-subst inside subst-inside-expr

; Purpose 5: Replace name-to-subst, unless it is a binding instance or inside a different scope of name-to-subst, with expr-to-subst inside subst-inside-expr 

; Definition: A free identifier is an identifer that is not inside its scope
; Definition: A bound identifier is an identifier that is inside its scope
; Purpose 5a: Replace free name-to-subst inside subst-inside-expr with expr-to-subst

(define (subst name-to-subst expr-to-subst subst-inside-expr)
  (type-case WAE subst-inside-expr
    [num (n) 
         subst-inside-expr]
    [add (lhs rhs)
         (add
          (subst name-to-subst expr-to-subst lhs)
          (subst name-to-subst expr-to-subst rhs))]
    [sub (lhs rhs)
         (sub
          (subst name-to-subst expr-to-subst lhs)
          (subst name-to-subst expr-to-subst rhs))]
    [id (name)
        (if (symbol=? name name-to-subst)
            expr-to-subst
            subst-inside-expr)]
    [with (name named-expr body-expr)
      (if (symbol=? name name-to-subst)
          (with name 
            (subst name-to-subst expr-to-subst
                   named-expr)
            body-expr)
          (with name 
            (subst name-to-subst expr-to-subst
                   named-expr)
            (subst name-to-subst expr-to-subst
                   body-expr)))]))

(test (subst 'x (num 1)
             (id 'x))
      (num 1))
(test (subst 'y (add (num 3) (num 2))
             (add (id 'y) (num 10)))
      (add (add (num 3) (num 2))
           (num 10)))
(test (subst 'x (num 1)
             (id 'y))
      (id 'y))
; This is an invalid test, but obeys purpose 1
; Therefore, purpose 1 is wrong
#;(test (subst 'x (num 5)
             (with 'x (num 3) 
               (num 1)))
      (with (num 5) (num 3) 
        (num 1)))
(test (subst 'x (num 5)
             (with 'x (num 3) 
               (num 1)))
      (with 'x (num 3) 
        (num 1)))
; This is a crazy test, but obeys purpose 2
; Therefore, purpose 2 is wrong
#;(test (subst 'x (num 1)
             (with 'x (num 3) 
               (id 'x)))
      (with 'x (num 3) 
        (num 1)))
(test (subst 'x (num 1)
             (with 'x (num 3) 
               (id 'x)))
      (with 'x (num 3) 
        (id 'x)))
; This is a good test, but disobeys purpose 3
; Therefore, purpose 3 is wrong
(test (subst 'x (num 1)
             (with 'y (num 3) 
               (id 'x)))
      (with 'y (num 3) 
        (num 1)))

(test (subst 'x (num 10)
             (parse '(with [y (+ x 1)]
                       (+ y y))))
      (parse '(with [y (+ 10 1)]
                (+ y y))))
(test (subst 'x (num 10)
             (parse '(with [x (+ x 1)]
                       (+ x x))))
      (parse '(with [x (+ 10 1)]
                (+ x x))))

; calc : WAE -> number
; Purpose: To determine the number computed/represented by the WAE
(define (calc e)
  (type-case WAE e
    [num (n) n]
    [add (lhs rhs) 
         (+ (calc lhs)
            (calc rhs))]
    [sub (lhs rhs)
         (- (calc lhs)
            (calc rhs))]
    [with (name named-expr body)
      (calc (subst name (num (calc named-expr)) body))]
    [id (name)
        (error 'calc "Unbound identifier: ~e" name)]))

(test (calc (parse '0))
      0)
(test (calc (parse '(+ 1 1)))
      2)
(test (calc (parse '(- 1 1)))
      0)
(test (calc (parse '(+ 1 (- 3 1))))
      3)
(test (calc (parse '(+ 1 (- 3 1))))
      (calc (parse '(+ 1 2))))
(test (calc (parse '(+ (- 3 1) 1)))
      3)
(test (calc (add (sub (num 3) (num 1)) (num 1)))
      3)

(test (calc (parse '(with [x (+ 5 5)] 
                      (+ x x))))
      (calc (parse '(+ 10 10))))
(test (calc (parse '(with [x (+ 5 5)] 
                      (+ x x))))
      20)
(test/exn (calc (parse 'x))
          "Unbound identifier")
(test (calc (parse '(with [x 10]
                      (with [y (+ x 1)]
                        (+ y y)))))
      22)
(test (calc (parse '(with [x 10]
                      (with [a (+ x 1)]
                        (+ a a)))))
      22)
(test (calc (parse '(with [x 10]
                      (with [x (+ x 1)]
                        (+ x x)))))
      22)

; Do we need names?
#;(with (+ 5 5)
    (+ <0> <0>))
#;(with (+ 5 5)
    (with (+ 2 <0>)
      (+ <1> <0>)))
; De Bruijn Index
