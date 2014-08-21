#lang plai
; Warning this is meant to error.
(halt-on-errors #t)

(define-type FWAE
  [num (n number?)]
  [add (lhs FWAE?)
       (rhs FWAE?)]
  [sub (lhs FWAE?)
       (rhs FWAE?)]
  [with (name symbol?)
    (named-expr FWAE?)
    (body FWAE?)]
  [id (name symbol?)]
  [fun (param symbol?)
       (body FWAE?)]
  [app (fun-expr FWAE?)
       (arg-expr FWAE?)])

; <FWAE> :==
;         | <number>
;         | (+ <FWAE> <FWAE>)
;         | (- <FWAE> <FWAE>)
;         | (with [<id> <FWAE>] <FWAE>)
;         | <id>
;         | (fun (<id>) <FWAE>)
;         | (<FWAE> <FWAE>)
; where id is a symbol

; parse : Sexpr -> FWAE
; Purpose: To accept FWAE style Sexprs and turn them into FWAEs
(define (parse se)
  (cond [(number? se) (num se)]
        [(symbol? se) (id se)]
        [(and (list? se) (symbol? (first se)) (symbol=? 'with (first se)))
         (with (first (second se))
           (parse (second (second se)))
           (parse (third se)))]
        [(and (list? se) (symbol? (first se)) (symbol=? '+ (first se)))
         (add (parse (second se))
              (parse (third se)))]
        [(and (list? se) (symbol? (first se)) (symbol=? '- (first se)))
         (sub (parse (second se))
              (parse (third se)))]
        [(and (list? se) (symbol? (first se)) (symbol=? 'fun (first se)))
         (fun (first (second se))
              (parse (third se)))]
        [(and (list? se))
         (app (parse (first se))
              (parse (second se)))]))


(test (parse '(fun (x) (+ x x)))
      (fun 'x (add (id 'x) (id 'x))))
(test (parse '(add1 1))
      (app (id 'add1) (num 1)))

(test (parse '(+ 1 2))
      (add (num 1) (num 2)))
(test (parse '(- 1 2))
      (sub (num 1) (num 2)))
(test (parse 'x)
      (id 'x))
(test (parse '(with [x (+ 1 1)] x))
      (with 'x (add (num 1) (num 1)) (id 'x)))

; subst : symbol FWAE FWAE -> FWAE
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
  (type-case FWAE subst-inside-expr
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
    [app (fun-expr arg-expr)
         (app (subst name-to-subst expr-to-subst
                     fun-expr) 
              (subst name-to-subst expr-to-subst
                     arg-expr))]
    [fun (param-name body-expr)
         (if (symbol=? param-name name-to-subst)
             (fun param-name 
                  body-expr)
             (fun param-name
                  (subst name-to-subst expr-to-subst
                         body-expr)))]))

(test (subst 'x (num 1)
             (parse '(fun (y) (+ x y))))
      (parse '(fun (y) (+ 1 y))))
(test (subst 'x (num 1)
             (parse '(fun (x) (+ x x))))
      (parse '(fun (x) (+ x x))))

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

(define (num+ lhs rhs)
  (num (+ (num-n lhs) (num-n rhs))))
(define (num- lhs rhs)
  (num (- (num-n lhs) (num-n rhs))))

; interp : FWAE -> FWAE
; Purpose: To determine the number computed/represented by the FWAE
(define (interp e)
  (type-case FWAE e
    [num (n) e]
    [add (lhs rhs) 
         (num+ (interp lhs)
            (interp rhs))]
    [sub (lhs rhs)
         (num- (interp lhs)
            (interp rhs))]
    [with (name named-expr body)
      (interp (subst name (interp named-expr)
                   body))]
    [id (name)
        (error 'interp "Unbound identifier: ~e" name)]
    [app (fun-expr arg-expr)
         (local [(define evaled-fun-expr (interp fun-expr))]
           (type-case FWAE evaled-fun-expr
             [fun (param-name body-expr)
                       #;(interp (subst param-name (interp arg-expr) body-expr))
                       (interp (with param-name arg-expr body-expr))]
             [else
              (error 'interp "Not a function")]))]
    [fun (param-name body-expr)
         e]))

; calc : FWAE -> number/FWAE
(define (calc e)
  (type-case FWAE (interp e)
    [num (n) n]
    [else
     e]))

(test (calc (parse '(fun (x) x)))
      (parse '(fun (x) x)))

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

(test (calc (parse '((fun (x) (+ x 1)) 10)))
      11)
(test (calc (parse '(with (x 5)
                        ((fun (x) (+ x 1)) x))))
      6)
(test (calc (parse '((fun (x) (+ x x)) (+ 10 5))))
      30)

(test/exn (calc (parse '((fun (n) (n n)) 5)))
          "Not a function")
(test (calc (parse '((fun (n) ((fun (n) (+ n 2))
                                 n))
                       5)))
      7)
(test (calc (parse '((fun (n) ((fun (x) (+ x 2))
                                 n))
                       5)))
      7)

(test/exn (calc (parse '(with (f (fun (x) (+ x y)))
                            (with (y 4) (f 5)))))
          ; Sam wants
          #;9
          ; Sanity wants
          "Unbound identifier")

; double(5) => lookup the definition of 'double'
; and run (with double-arg 5 double-body)

; (with (x 5) (+ x x))
; f(x) = (+ x x); f(5)
; ((lambda (x) (+ x x)) 5)
(test (calc (parse '((fun (x) (+ x x)) 5)))
      10)
(test (calc (parse '(with [double
                           (fun (x) (+ x x))]
                      (double 5))))
      10)
(test (calc (parse '(with [y 10]
                      (with [f
                           (fun (x) (+ x y))]
                        (f 5)))))
      15)
(test/exn (calc (parse '(with [f
                               (fun (x) (+ x y))]
                          (with [y 10]
                            (f 5)))))
          "Unbound identifier")
(test (calc (parse '(with [make-adder
                           (fun (x)
                                (fun (y)
                                     (+ x y)))]
                      (with [add10 (make-adder 10)]
                        (add10 5)))))
      15)
(test (calc (parse '(with [make-adder
                           (fun (x)
                                (fun (y)
                                     (+ x y)))]
                      (with [add10 
                             (fun (y)
                                  (+ 10 y))]
                        (add10 5)))))
      15)
(test (calc (parse '(with [fake-adder
                           (fun (x)
                                (fun (x)
                                     (+ x x)))]
                      (with [fake10 (fake-adder 10)]
                        (fake10 5)))))
      10)
