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

(define-type DefrdSub
  [mtSub] ; mt is "em" "tee"
  [aSub (name symbol?)
        (value number?)
        (rest DefrdSub?)])

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

(test (lookup-DefrdSub 'x (mtSub)) #f)
(test (lookup-DefrdSub 'x 
                       (aSub 'x 1
                             (mtSub))) 
      1)
(test (lookup-DefrdSub 'x 
                       (aSub 'y 2
                             (aSub 'x 1
                                   (mtSub)))) 
      1)
(test (lookup-DefrdSub 'x 
                       (aSub 'x 2
                             (aSub 'x 1
                                   (mtSub)))) 
      2)

; interp/ds : F1WAE listof(FunDef) DefrdSub -> number
; Purpose: To compute the number represented by the F1WAE
(define (interp/ds e defns ds)
  (type-case F1WAE e
    [num (n) n]
    [add (lhs rhs) 
         (+ (interp/ds lhs defns ds)
            (interp/ds rhs defns ds))]
    [sub (lhs rhs)
         (- (interp/ds lhs defns ds)
            (interp/ds rhs defns ds))]
    [with (name named-expr body)
      (interp/ds
       body
       defns
       (aSub name 
             (interp/ds named-expr defns ds)
             ds))]
    [id (name)
        (local [(define names-value (lookup-DefrdSub name ds))]
          (if names-value
              names-value
              (error 'interp "Unbound identifier: ~e" name)))]
    [app (fun-name arg-expr)
         (local [(define the-fundef (lookup-FunDef fun-name defns))]
           (type-case FunDef the-fundef
             [a-fundef (fun-name param-name body-expr)
                       ; Thinking
                       #;(with (x 5)
                           (f (+ x 1)))
                       ;(f y) = (+ y 4)
                       ;(f 6)
                       
                       ; This expression
                       #;(interp/ds (with param-name arg-expr
                                      body-expr)
                                  defns
                                  ds)
                       ; means
                       #;(interp/ds body-expr
                                    defns
                                    (aSub param-name
                                          (interp/ds arg-expr defns ds)
                                          ds))
                       ; not
                       (interp/ds body-expr
                                    defns
                                    (aSub param-name
                                          (interp/ds arg-expr defns ds)
                                          ; The difference is here
                                          ; The body shouldn't see "ds"
                                          (mtSub)))
                       ; app is no longer a special case of with
                       ]))]))

; interp : F1WAE listof(FunDef) -> number
(define (interp e defns)
  (interp/ds e defns (mtSub)))

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

(test (calc (parse '(with [x (+ 5 6)]
                      (with [y (+ x 1)]
                        (+ x y)))))
      23)
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

(test (calc (parse '(with (x 1)
                      (with (y 2)
                        (with (z 3)
                          (+ x (+ y z)))))))
      6)

; Interp w/ subst has O(n^2)
#;'(with (x_0 e_0)
     ....
     (with (x_n e_n)
       e_n+1))


