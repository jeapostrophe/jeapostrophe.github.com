#lang plai
(halt-on-errors #t)

(define-type FWAE
  [num (n number?)]
  [add (lhs FWAE?)
       (rhs FWAE?)]
  [sub (lhs FWAE?)
       (rhs FWAE?)]
  #;[with (name symbol?)
    (named-expr FWAE?)
    (body-expr FWAE?)]
  [id (name symbol?)]
  [fun (param symbol?)
       (body FWAE?)]
  [app (fun-expr FWAE?)
       (arg-expr FWAE?)])

(define (with name named-expr body-expr)
  (app (fun name body-expr) named-expr))

; <FWAE> :== <number>
;      |   (+ <FWAE> <FWAE>)
;      |   (- <FWAE> <FWAE>)
;      |   <id>
;      |   (with [<id> <FWAE>] <FWAE>)
;      |   (fun (<id>) <FWAE>)
;      |   (<FWAE> <FWAE>)
; where <id> is symbol

; parse: Sexpr -> FWAE
; Purpose: Parse an Sexpr into an FWAE
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
         (app (parse (first se)) (parse (second se)))]))

(test (parse '(f 1))
      (app (id 'f) (num 1)))
(test (parse '(fun (x) x))
      (fun 'x (id 'x)))

(test (parse '(+ 1 1))
      (add (num 1) (num 1)))
(test (parse '(- 1 2))
      (sub (num 1) (num 2)))
(test (parse '(with [x 5] (+ x x)))
      (with 'x (num 5)
        (add (id 'x) (id 'x))))

(define-type FWAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
        (body FWAE?)
        (saved-subs DefrdSub?)])

(define-type DefrdSub
  [mtSub] ; mt is "em" "tee"
  [aSub (name symbol?)
        (value FWAE-Value?)
        (rest DefrdSub?)])

(define ds-ex0
  (mtSub))
(define ds-ex1
  (aSub 'x (numV 1)
        (mtSub)))
(define ds-ex2
  (aSub 'y (numV 2)
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
                       (aSub 'x (numV 1)
                             (mtSub))) 
      (numV 1))
(test (lookup-DefrdSub 'x 
                       (aSub 'y (numV 2)
                             (aSub 'x (numV 1)
                                   (mtSub)))) 
      (numV 1))
(test (lookup-DefrdSub 'x 
                       (aSub 'x (numV 2)
                             (aSub 'x (numV 1)
                                   (mtSub)))) 
      (numV 2))

(define (make-numV-op op)
  (lambda (lhs-v rhs-v)
    (type-case FWAE-Value lhs-v
      [numV (lhs)
            (type-case FWAE-Value rhs-v
              [numV (rhs)
                    (numV (op lhs rhs))]
              [else
               (error 'interp "Not a number")])]
      [else
       (error 'interp "Not a number")])))
(define numV+ (make-numV-op +))
(define numV- (make-numV-op -))         

; interp : FWAE DefrdSub -> FWAE-Value
; Purpose: To compute the number represented by the FWAE
(define (interp e ds)
  (type-case FWAE e
    [num (n) 
         (numV n)]
    [add (lhs rhs) 
         (numV+ (interp lhs ds)
                (interp rhs ds))]
    [sub (lhs rhs)
         (numV- (interp lhs ds)
                (interp rhs ds))]
    #;[with (name named-expr body)
      #;(interp
       body
       (aSub name 
             (interp named-expr ds)
             ds))
      ; (with [x 5] (+ x x)
      ; ==>
      ; ((fun (x) (+ x x)) 5)
      (interp (app (fun name body) named-expr)
              ds)]
    [id (name)
        (local [(define names-value (lookup-DefrdSub name ds))]
          (if names-value
              names-value
              (error 'interp "Unbound identifier: ~e" name)))]
    [fun (param body)
         (closureV param body ds)]
    [app (fun-expr arg-expr)
         (local [(define the-fundef 
                   (interp fun-expr ds))]
           (type-case FWAE-Value the-fundef
             [closureV (param-name body-expr funs-ds)
                       (interp body-expr
                               (aSub param-name
                                     (interp arg-expr ds)
                                     funs-ds))]
             [numV (n)
                   (error 'interp "Not a function")]))]))

#;(with (x 5)
  (with (y 6)
    (with (f (fun (z) (+ z 2)))
      (f 5))))
#;((fun (x)
      ((fun (y)
            ((fun (f)
                  (f 5))
             (fun (z) (+ z 2))))
       6))
 5)

; calc : FWAE -> number
(define (calc e)
  (define v (interp e (mtSub)))
  (type-case FWAE-Value v
    [numV (n)
          n]
    [else
     v]))

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

(test/exn (calc (parse '(double 5)))
      "Unbound identifier")
(test/exn (calc (parse '(with (double 1)
                          (double 5))))
          "Not a function")
(test (calc (parse '(with (double 
                           (fun (x)
                                (+ x x)))
                      (double 5))))
      10)
(test (calc (parse '((fun (x) (+ x x)) 5)))
      10)

(test (calc (parse '(with (x 1)
                      (with (y 2)
                        (with (z 3)
                          (+ x (+ y z)))))))
      6)

(test (calc (parse '(with [x 1]
                      (with [f (fun (y)
                                    (+ x y))]
                        (f 10)))))
      ; Sam
      11
      ; Joseph says with can't find fun defs
      #;"Unbound identifier")
(test/exn (calc (parse '(with [f (fun (y)
                                      (+ x y))]
                          (with [x 1]
                            (f 10)))))
          "Unbound identifier")

(test (calc (parse '(with [x 10]
                      (with [add10
                             (fun (y)
                                  (+ x y))]
                        (add10 5)))))
      15)
(test (calc (parse '(with [add10
                           (with [x 10]
                             (fun (y)
                                  (+ x y)))]
                      (add10 5))))
      15)
(test (calc (parse '(with [make-adder
                           (fun (x)
                                (fun (y)
                                     (+ x y)))]
                      (with [add10
                             (make-adder 10)]
                        (add10 5)))))
      15)
(test (calc (parse '(with [make-adder
                           (fun (x)
                                (fun (y)
                                     (+ x y)))]
                      (with [add10
                             (make-adder 10)]
                        (+ (add10 5)
                           (add10 6))))))
      31)
(test (calc (parse '(with [fake-adder
                           (fun (x)
                                (fun (y)
                                     (+ x x)))]
                      (with [add10
                             (fake-adder 10)]
                        (add10 5)))))
      20)
(test (calc (parse '(with [fake-adder
                           (fun (x)
                                (fun (y)
                                     (+ y y)))]
                      (with [add10
                             (fake-adder 10)]
                        (add10 5)))))
      10)

      
      