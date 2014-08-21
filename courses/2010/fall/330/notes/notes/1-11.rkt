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

(define-type DefrdSub
  [mtSub] ; mt is "em" "tee" hahahaha
  [aSub (name symbol?)
        (value number?)
        (restSub DefrdSub?)])

(define ds-ex0
  (mtSub))
(define ds-ex1
  (aSub 'x 1
        (mtSub)))
(define ds-ex2
  (aSub 'y 2
        ds-ex1))

; lookup-DefrdSub : symbol DefrdSub -> number/#f
(define (lookup-DefrdSub name ds)
  (type-case DefrdSub ds
    [mtSub ()
           #f]
    [aSub (some-name some-value rest-ds)
          (if (symbol=? name
                        some-name)
              some-value
              (lookup-DefrdSub name rest-ds))]))

(test (lookup-DefrdSub 'x ds-ex0) #f)
(test (lookup-DefrdSub 'x ds-ex1) 1)
(test (lookup-DefrdSub 'x ds-ex2) 1)
(test (lookup-DefrdSub 'y ds-ex1) #f)
(test (lookup-DefrdSub 'y ds-ex2) 2)
(test (lookup-DefrdSub 'x
                       (aSub 'x 1
                             (aSub 'x 0
                                   (mtSub))))
      1)
          

; interp/Defrd : F1WAE listof(FunDef) DefrdSub -> number
; Purpose: To determine the number computed/represented by the F1WAE
(define (interp/Defrd e defns ds)
  (type-case F1WAE e
    [num (n) n]
    [add (lhs rhs) 
         (+ (interp/Defrd lhs defns ds)
            (interp/Defrd rhs defns ds))]
    [sub (lhs rhs)
         (- (interp/Defrd lhs defns ds)
            (interp/Defrd rhs defns ds))]
    [with (name named-expr body)
      (interp/Defrd 
       body
       defns
       (aSub name
             (interp/Defrd named-expr defns ds)
             ds))]
    [id (name)
        (local [(define the-val-of-name (lookup-DefrdSub name ds))]
          (if the-val-of-name
              the-val-of-name 
              (error 'interp "Unbound identifier: ~e" name)))]
    [app (fun-name arg-expr)
         (local [(define fun-def (lookup-FunDef fun-name defns))]
           (type-case FunDef fun-def
             [a-fundef (fun-name param-name body-expr)
                       ; This expression
                       #;(interp/Defrd (with param-name arg-expr ; can refer to ds
                                       ; body can only refer to param-name
                                       ; (with x 5 (+ x y)) | ds = [y 4]
                                       body-expr)
                                     defns
                                     ds)
                       ; means this expresion:
                       #;(interp/Defrd 
                        body-expr
                        defns
                        (aSub param-name
                              (interp/Defrd arg-expr defns ds)
                              ds))
                       ; not:
                       (interp/Defrd 
                        body-expr
                        defns
                        (aSub param-name
                              (interp/Defrd arg-expr defns ds)
                              ; (The difference is right here, yo)
                              ; There are no more names that body-expr would have
                              ; had substituted
                              (mtSub)))]))]))

; interp : F1WAE listof(FunDefs) -> number
(define (interp e defns)
  (interp/Defrd e defns (mtSub)))

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
(test/exn (interp (parse '(with (y 5)
                            (add1 x)))
                  (list fundef:add1))
          "Unbound identifier")
(test/exn (interp (parse '(with (x 5)
                            (add1 y)))
                  (list fundef:add1))
          "Unbound identifier")
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

(test (calc (parse '(with (x 1)
                      (with (y 2)
                        (with (z 3)
                          (+ x (+ y z)))))))
      6)
; interp w/ subst is O(n^2) where n is the number of withs (or the program size)
#;(calc (parse '(with (x_0 e_0)
                  ....
                    (with (x_n e_n)
                      e_n+1)))
          ....)

; How efficient is this?
; Do we ever take stuff off?