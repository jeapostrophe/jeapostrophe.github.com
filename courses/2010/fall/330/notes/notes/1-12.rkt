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
      (body FWAE?)]
  [id (name symbol?)]
  [fun (param symbol?)
       (body FWAE?)]
  [app (fun-expr FWAE?)
       (arg-expr FWAE?)])

(define (with name named-expr body)
  (app (fun name body) named-expr))

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

(define-type FWAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
               (body FWAE?)
               (ds-at-defn DefrdSub?)])

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

(define (num+ lhs rhs)
  (numV (+ (numV-n lhs) (numV-n rhs))))
(define (num- lhs rhs)
  (numV (- (numV-n lhs) (numV-n rhs))))

; interp : FWAE -> FWAE-Value
; Purpose: To determine the number computed/represented by the FWAE
(define (interp e ds)
  (type-case FWAE e
    [num (n) (numV n)]
    [add (lhs rhs) 
         (num+ (interp lhs ds)
               (interp rhs ds))]
    [sub (lhs rhs)
         (num- (interp lhs ds)
               (interp rhs ds))]
    #;[with (name named-expr body)
      
      #;(with name named-expr body)
      ; means
      (interp 
       (app (fun name body) named-expr)
       ds)
      
      #;(interp
       body
       (aSub name 
             (interp named-expr ds)
             ds))]
    [id (name)
        (local [(define names-value (lookup-DefrdSub name ds))]
          (if names-value
              names-value
              (error 'interp "Unbound identifier: ~e" name)))]
    [app (fun-expr arg-expr)
         (local [(define evaled-fun-expr (interp fun-expr ds))]
           (type-case FWAE-Value evaled-fun-expr
             [closureV (param-name body-expr funs-ds)
                          (interp body-expr ;(+ x y)
                                  (aSub param-name ; x
                                        (interp arg-expr ds) ; 5
                                        funs-ds))]
             [numV (n)
              (error 'interp "Not a function")]))]
    [fun (param-name body-expr)
         (closureV param-name body-expr ds)]))

; calc : FWAE -> number/closureV
(define (calc e)
  (type-case FWAE-Value (interp e (mtSub))
    [numV (n) 
          n]
    [closureV (param body ds)
                 (closureV param body ds)]))

(test (calc (parse '(fun (x) x)))
      (closureV 'x (id 'x) (mtSub)))

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