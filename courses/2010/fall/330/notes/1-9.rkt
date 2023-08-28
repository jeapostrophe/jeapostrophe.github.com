#lang plai
(halt-on-errors #t)

(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?)
       (rhs F1WAE?)]
  [sub (lhs F1WAE?)
       (rhs F1WAE?)]
  [with (name symbol?)
    (named-expr F1WAE?)
    (body F1WAE?)]
  [id (name symbol?)]
  [app (fun symbol?)
       (arg-expr F1WAE?)])

; <F1WAE> :==
;         | <number>
;         | (+ <F1WAE> <F1WAE>)
;         | (- <F1WAE> <F1WAE>)
;         | (with [<id> <F1WAE>] <F1WAE>)
;         | <id>
;         | (<id> <F1WAE>)
; where id is a symbol

(define-type FunDef
  [a-fundef (name symbol?)
            (param symbol?)
            (body F1WAE?)])

(define fundef:double
  (a-fundef 'double 'x
            (add (id 'x) (id 'x))))
(define fundef:add1
  (a-fundef 'add1 'x
            (add (id 'x) (num 1))))

; lookup-FunDef : symbol listof(FunDef) -> FunDef
(define (lookup-FunDef target lofd)
  (cond
    [(empty? lofd)
     (error 'interp "Undefined function: ~e" target)]
    [(symbol=? target (a-fundef-name (first lofd)))
     (first lofd)]
    [else
     (lookup-FunDef target (rest lofd))]))

(test/exn (lookup-FunDef 'double empty)
          "Undefined function")
(test (lookup-FunDef 'double (list fundef:double))
      fundef:double)
(test (lookup-FunDef 'double (list fundef:add1 fundef:double))
      fundef:double)


; parse : Sexpr -> F1WAE
; Purpose: To accept F1WAE style Sexprs and turn them into F1WAEs
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
              (parse (third se)))]
        [(and (list? se) (symbol? (first se)))
         (app (first se)
              (parse (second se)))]))

(test (parse '(add1 1))
      (app 'add1 (num 1)))

(test (parse '(+ 1 2))
      (add (num 1) (num 2)))
(test (parse '(- 1 2))
      (sub (num 1) (num 2)))
(test (parse 'x)
      (id 'x))
(test (parse '(with [x (+ 1 1)] x))
      (with 'x (add (num 1) (num 1)) (id 'x)))

; subst : symbol F1WAE F1WAE -> F1WAE
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
  (type-case F1WAE subst-inside-expr
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
                   body-expr)))]
    [app (fun-name arg-expr)
         (app fun-name 
              (subst name-to-subst expr-to-subst
                     arg-expr))]))

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

; interp : F1WAE listof(FunDef) -> number
; Purpose: To determine the number computed/represented by the F1WAE
(define (interp e defns)
  (type-case F1WAE e
    [num (n) n]
    [add (lhs rhs) 
         (+ (interp lhs defns)
            (interp rhs defns))]
    [sub (lhs rhs)
         (- (interp lhs defns)
            (interp rhs defns))]
    [with (name named-expr body)
      (interp (subst name (num (interp named-expr defns))
                   body) 
            defns)]
    [id (name)
        (error 'interp "Unbound identifier: ~e" name)]
    [app (fun-name arg-expr)
         (local [(define fun-def (lookup-FunDef fun-name defns))]
           (type-case FunDef fun-def
             [a-fundef (fun-name param-name body-expr)
                       #;(interp (subst param-name (num (interp arg-expr defns)) body-expr)
                               defns)
                       (interp (with param-name arg-expr body-expr)
                               defns)]))]))

; calc : F1WAE -> number
(define (calc e)
  (interp e empty))

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

(test (interp (parse '(add1 10))
              (list fundef:add1))
      11)
(test (interp (parse '(with (x 5)
                        (add1 x)))
              (list fundef:add1))
      6)
(test (interp (parse '(double (+ 10 5)))
              (list fundef:double))
      30)

(test/exn (interp (parse '(f 5))
                  (list
                   ; f(n) = n(n)
                   (a-fundef 'f 'n (parse '(n n)))))
          "Undefined function")
(test (interp (parse '(f 5))
              (list
               ; f(n) = n(n)
               (a-fundef 'f 'n (parse '(n n)))
               (a-fundef 'n 'n (parse '(+ n 2)))))
      7)
(test (interp (parse '(f 5))
              (list
               ; f(n) = g(n)
               (a-fundef 'f 'n (parse '(g n)))
               (a-fundef 'g 'x (parse '(+ x 2)))))
      7)
; Infinite loop:
#;(test (interp (parse '(f 1))
              (list (a-fundef 'f 'x (parse '(f x)))))
      42)
; Errors = halting
#;(test (interp (parse '(is-even? 6))
              (list (a-fundef 'is-even? 'x
                              (parse '(+ (/ 1 (- (/ x 2) 1))
                                         (is-even? (/ x 2)))))))
      "Divide by zero")

(test/exn (interp (parse '(with (y 4) (f 5)))
                  (list (a-fundef 'f 'x (parse '(+ x y)))))
          ; Sam wants
          #;9
          ; Sanity wants
          "Unbound identifier")

