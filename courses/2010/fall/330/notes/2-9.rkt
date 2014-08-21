#lang plai
(halt-on-errors #t)

(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?)
       (rhs F1WAE?)]
  [sub (lhs F1WAE?)
       (rhs F1WAE?)]
  [id (name symbol?)]
  [with (name symbol?)
    (named-expr F1WAE?)
    (body-expr F1WAE?)]
  [app (fun-name symbol?)
       (arg-expr F1WAE?)])

(define-type FunDef
  [a-fundef (fun-name symbol?)
            (param symbol?)
            (body-expr F1WAE?)])

; add1(x) = x + 1
(define fundef:add1
  (a-fundef 'add1 'x
            (add (id 'x) (num 1))))
(define fundef:double
  (a-fundef 'double 'x
            (add (id 'x) (id 'x))))

; <F1WAE> :== <number>
;      |   (+ <F1WAE> <F1WAE>)
;      |   (- <F1WAE> <F1WAE>)
;      |   <id>
;      |   (with [<id> <F1WAE>] <F1WAE>)
;      |   (<id> <F1WAE>)
; where <id> is symbol

; parse: Sexpr -> F1WAE
; Purpose: Parse an Sexpr into an F1WAE
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
         (app (first se) (parse (second se)))]))

(test (parse '(f 1))
      (app 'f (num 1)))

(test (parse '(+ 1 1))
      (add (num 1) (num 1)))
(test (parse '(- 1 2))
      (sub (num 1) (num 2)))
(test (parse '(with [x 5] (+ x x)))
      (with 'x (num 5)
        (add (id 'x) (id 'x))))

; subst : symbol F1WAE F1WAE -> F1WAE
; Purpose 1 (bad): Within plug-into replace every occurrence of id-to-subst with replace-with
; Purpose 2 (bad): Within plug-into replace every occurrence (not inside a with) of id-to-subst with replace-with

; A with binding is (with ////[id F1WAE]//// F1WAE) inside the ////s
; Purpose 3 (bad): Within plug-into replace every occurrence (not inside a with binding) of id-to-subst with replace-with

; Scope is the syntactic region in the program where an id makes sense
; A bound id is an identifier that is inside of an id-scope
; A free id is an identifier that is not
; Purpose 4: Within plug-into replace every free occurrence of id-to-subst with replace-with

(define (subst id-to-subst replace-with plug-into)
  (type-case F1WAE plug-into
    [num (n) plug-into]
    [add (lhs rhs)
         (add
          (subst id-to-subst replace-with lhs)
          (subst id-to-subst replace-with rhs))]
    [sub (lhs rhs)
         (sub
          (subst id-to-subst replace-with lhs)
          (subst id-to-subst replace-with rhs))]
    [id (name)
        (if (symbol=? id-to-subst name)
            replace-with
            plug-into)]
    [with (name named-expr body)
      (if (symbol=? id-to-subst name)
          (with name 
            (subst id-to-subst replace-with named-expr)
            body)
          (with name 
            (subst id-to-subst replace-with named-expr)
            (subst id-to-subst replace-with body)))]
    [app (fun-name arg-expr)
         (app fun-name (subst id-to-subst replace-with arg-expr))]))

(test (subst 'x (parse '5)
             (parse '(f x)))
      (parse '(f 5)))
             
(test (subst 'x (parse '5)
             (parse 'x))
      (parse '5))
(test (subst 'x (num 5)
             (id 'x))
      (num '5))
(test (subst 'x (parse '5)
             (parse '7))
      (parse '7))
(test (subst 'x (parse '5)
             (parse 'y))
      (parse 'y))
(test (subst 'x (parse '5)
             (parse '(+ x y)))
      (parse '(+ 5 y)))
(test (subst 'x (parse '5)
             (parse '(- x y)))
      (parse '(- 5 y)))
(test (subst 'x (parse '(+ 5 5))
             (parse '(- x y)))
      (parse '(- (+ 5 5) y)))
(test (subst 'x (parse '(with [y 3] y))
             (parse '(- x y)))
      (parse '(- (with [y 3] y) y)))

; This is a bad test case. Purpose 1 is wrong.
#;(test (subst 'x (parse '5)
             (parse '(with [x 1] 7)))
      (parse '(with [5 1] 7)))
(test (subst 'x (parse '5)
             (parse '(with [x 1] 7)))
      (parse '(with [x 1] 7)))

; This is a good test case. Purpose 2 is wrong.
(test (subst 'x (parse '5)
             (parse '(with [y 1] x)))
      (parse '(with [y 1] 5)))

; This is a bad test case. Purpose 3 is wrong
#;(test (subst 'x (parse '5)
             (parse '(with [x 1] x)))
      (parse '(with [x 1] 5)))
(test (subst 'x (parse '5)
             (parse '(with [x 1] x)))
      (parse '(with [x 1] x)))
(test (subst 'x (parse '5)
             (parse '(+ x (with [x 1] x))))
      (parse '(+ 5 (with [x 1] x))))

(test (subst 'x (parse '5)
             (parse '(+ x (with [y 1] x))))
      (parse '(+ 5 (with [y 1] 5))))

; lookup-FunDef : symbol listof(FunDef) -> FunDef
(define (lookup-FunDef fun-name defns)
  (cond
    [(empty? defns)
     (error 'interp "Undefined function: ~e" fun-name)]
    [(symbol=? fun-name (a-fundef-fun-name (first defns)))
     (first defns)]
    [else
     (lookup-FunDef fun-name (rest defns))]))

(test/exn (lookup-FunDef 'add1 empty) "Undefined function")
(test (lookup-FunDef 'add1 (list fundef:add1)) fundef:add1)  
      
; interp : F1WAE listof(FunDef) -> number
; Purpose: To compute the number represented by the F1WAE
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
         (local [(define the-fundef (lookup-FunDef fun-name defns))]
           (type-case FunDef the-fundef
             [a-fundef (fun-name param-name body-expr)
                       
                       (interp (with param-name arg-expr
                                 body-expr)
                               defns)
                       
                       #;(interp (subst param-name (num (interp arg-expr defns))
                                      body-expr)
                               defns)]))]))

; calc : F1WAE -> number
(define (calc e)
  (interp e empty))

(test/exn (calc (parse 'x))
          "Unbound identifier")
(test (calc (parse '0))
      0)
(test (calc (parse '(+ 1 1)))
      2)
(test (calc (parse '(- 2 1)))
      1)
(test (calc (parse (list '- 2 1)))
      1)
(test (calc (parse (list '- 2 (list '- 2 1))))
      1)
(test (calc (parse '(- (+ 1 2) (- 8 9))))
      4)
(test (calc (parse '(with [x 5] (+ x x))))
      10)
(test (calc (parse '(with [x (+ 5 6)] (+ x x))))
      22)

(test (calc (parse '(with [x (+ 5 6)] (+ x x))))
      (calc (parse '(with [x 11] (+ x x)))))
(test (calc (parse '(with [x (+ 5 6)] (+ x x))))
      (calc (parse '(+ (+ 5 6)
                       (+ 5 6)))))
(test (calc (parse '(with [x (+ 5 6)] (+ x x))))
      (calc (parse '(+ 11 11))))

(test (subst 'x (parse '(+ 5 6))
             (parse '(with [y (+ x 1)]
                       (+ x y))))
      (parse '(with [y (+ (+ 5 6) 1)]
                (+ (+ 5 6) y))))
(test (calc (parse '(with [x (+ 5 6)]
                      (with [y (+ x 1)]
                        (+ x y)))))
      23)
(test (subst 'x (parse '(+ 5 6))
             (parse '(with [x (+ x 1)]
                       (+ x x))))
      (parse '(with [x (+ (+ 5 6) 1)]
                (+ x x))))
(test (calc (parse '(with [x (+ 5 6)]
                      (with [x (+ x 1)]
                        (+ x x)))))
      24)

; If you ever want to double, then do this:
#;(with (x expr)
    (+ x x))
#;(double expr)

(test (interp (parse '(double 5))
              (list fundef:double))
      10)

; Infinite loops!
#;(test (interp (parse '(f 0))
              (list (a-fundef 'f 'x
                              (parse '(f x)))))
      42)

(test (interp (parse '(f 0))
              (list (a-fundef 'f 'x
                              (parse '(g (+ x 1))))
                    (a-fundef 'g 'x
                              (parse '(- x 32)))))
      -31)

(test/exn (interp (parse '(f 0))
              (list (a-fundef 'f 'n
                              (parse '(n n)))))
      "Undefined function")

(test/exn (interp (parse '(with (y 1) (f 0)))
                  (list (a-fundef 'f 'x (parse '(+ x y)))))
          "Unbound identifier")

(test (interp (parse '(double (+ 7 1)))
              (list fundef:double))
      16)