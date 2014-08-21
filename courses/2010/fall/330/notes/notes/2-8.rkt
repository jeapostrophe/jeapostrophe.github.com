#lang plai
(halt-on-errors #t)

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?)
       (rhs WAE?)]
  [sub (lhs WAE?)
       (rhs WAE?)]
  [id (name symbol?)]
  [with (name symbol?)
    (named-expr WAE?)
    (body-expr WAE?)])

; <WAE> :== <number>
;      |   (+ <WAE> <WAE>)
;      |   (- <WAE> <WAE>)
;      |   <id>
;      |   (with [<id> <WAE>] <WAE>)
; where <id> is symbol

; parse: Sexpr -> WAE
; Purpose: Parse an Sexpr into an WAE
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

(test (parse '(+ 1 1))
      (add (num 1) (num 1)))
(test (parse '(- 1 2))
      (sub (num 1) (num 2)))
(test (parse '(with [x 5] (+ x x)))
      (with 'x (num 5)
        (add (id 'x) (id 'x))))

; subst : symbol WAE WAE -> WAE
; Purpose 1 (bad): Within plug-into replace every occurrence of id-to-subst with replace-with
; Purpose 2 (bad): Within plug-into replace every occurrence (not inside a with) of id-to-subst with replace-with

; A with binding is (with ////[id wae]//// wae) inside the ////s
; Purpose 3 (bad): Within plug-into replace every occurrence (not inside a with binding) of id-to-subst with replace-with

; Scope is the syntactic region in the program where an id makes sense
; A bound id is an identifier that is inside of an id-scope
; A free id is an identifier that is not
; Purpose 4: Within plug-into replace every free occurrence of id-to-subst with replace-with

(define (subst id-to-subst replace-with plug-into)
  (type-case WAE plug-into
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
            (subst id-to-subst replace-with body)))]))

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
      
; calc : WAE -> number
; Purpose: To compute the number represented by the WAE
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

; Do we need names?
#;(with [x (+ 5 5)]
    (+ x x))
#;(with (+ 5 5)
    (+ <0> <0>))
#;(with (+ 5 5)
    (with (+ <0> 6)
      (+ <0> <1>)))
; De Bruijn Index