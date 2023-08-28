#lang plai
(print-only-errors #t)

(define-type AE
  [num (n number?)]
  [binop (op procedure?)
         (lhs AE?)
         (rhs AE?)]
  [id (sym symbol?)]
  [with (new-name symbol?)
        (named-thing AE?)
        (body AE?)]
  [app (fun AE?)
       (arg AE?)]
  [fun (param symbol?)
       (body AE?)])

(define (add lhs rhs)
  (binop + lhs rhs))
(define (sub lhs rhs)
  (binop - lhs rhs))

;; <AE> := <real Racket number>
;;       | (+ <AE> <AE>)
;;       | (- <AE> <AE>)
;;       | <id>
;;       | (with [<id> <AE>] <AE>)
;;       | (<AE> <AE>)
;;       | (fun (<id>) <AE>)

;; where <id> is any Racket symbol, except +, -, and with

;; parse :: s-expression -> AE
(define (parse se)
  (cond
    [(and (list? se)
          (= 2 (length se)))
     (app (parse (first se))
          (parse (second se)))]
    [(and (list? se)
          (= 3 (length se))
          (equal? 'fun (first se))
          (list? (second se))
          (= 1 (length (second se)))
          (symbol? (first (second se))))
     (fun (first (second se))
          (parse (third se)))]
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
(test (parse '(double 5))
      (app (id 'double) (num 5)))
(test (parse '(fun (x) (+ x x)))
      (fun 'x (add (id 'x) (id 'x))))

(test/exn (parse "1")
          "Invalid syntax")

;; "Environment"
(define-type DefrdSubst
  [mtSub]
  [andJustOneMoreThing
   (name symbol?)
   (named-value AEV?)
   (all-the-other-substs DefrdSubst?)])

(define-type AEV
  [numV
   (n number?)]
  [funV
   (param symbol?)
   (body AE?)
   (env DefrdSubst?)])

(define (lookup-id $ sym)
  (type-case
   DefrdSubst $
   [mtSub 
    ()
    (error 'calc "You has a bad identifier, bro: ~e" sym)]
   [andJustOneMoreThing
    (name named-value rest)
    (if (symbol=? name sym)
      named-value
      (lookup-id rest sym))]))

;; calc : AE? DefrdSubst -> number?
;; compute the meaning of the AE
(define (calc ae $)
  (type-case
   AE ae
   [fun
    (param body)
    ;; construct a closure:
    (funV param body $)]
   [app
    (fun arg)
    (type-case
     AEV (calc fun $)
     [funV
      (arg-name fun-body fun-$)
      (calc fun-body 
            (andJustOneMoreThing
             arg-name (calc arg $)
             ;; (mtSub) --> only top-level functions
             ;; $ --> dynamic scope
             ;; static scope:
             fun-$))]
     [else
      (error 'calc "Not a function")])]
   [id
    (sym)
    (lookup-id $ sym)]
   [with
    (name named-thing body)
    ;; (calc body 
    ;;       (andJustOneMoreThing
    ;;        name (calc named-thing $)
    ;;        $))
    (calc (app (fun name body) named-thing)
          $)]
   [num
    (n)
    (numV n)]
   [binop
    (op lhs rhs)
    (numV
     (op (numV-n* (calc lhs $))
         (numV-n* (calc rhs $))))]))

(define (numV-n* a)
  (if (numV? a)
    (numV-n a)
    (error 'calc "Not a number: ~e" a)))

;; calc* : sexpr -> number?
(define (calc* se)
  (define res (calc (parse se) (mtSub)))
  (type-case 
   AEV res
   [numV (n) n]
   [else res]))

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
(test (calc* '(with 
               [x (+ 5 5)]
               (with [x 7]
                     (+ x x))))
      14)
(test (calc* '(with [x (+ 5 5)]
                    (+ x (with [x 7]
                               (+ x x)))))
      24)
(test (calc* '(with [x (+ 5 5)]
                    (+ (with [x 7]
                               (+ x x))
                       x)))
      24)
(test (calc* '(with
               [x (+ 5 5)]
               (+ (with [x 7]
                        (+ x x))
                  (with [x 8]
                        (+ x x)))))
      (+ 14 16))
(test (calc* '(with [x 7]
                    (with [y (+ 2 x)]
                          (+ y 3))))
      12)
(test (calc* '(with [y 7]
                    (with [y (+ y 2)]
                          (+ y 3))))
      12)
(test (calc* '(with [x (+ 5 5)]
                    7))
      7)

(test (calc* '(with [x (+ 5 5)]
                    (+ x x)))
      20)
(test (calc* '(with [x (+ 5 6)]
                    (+ x x)))
      22)

(test (calc* '(with [x 5]
                     (with [y 5]
                           (+ (+ x x) y))))
      15)

(test (calc* '(with [x 5]
                     (with [x (+ 1 x)]
                           (+ (+ x x) x))))
      18)

(test/exn (calc* '(with [x 5]
                     (with [x (+ 1 x)]
                           (+ (+ x x) y))))
          "bro")

(test/exn (calc* '(with [x x]
                        5))
          "bro")


;; f(x) = x + 2
;; f(1)

;; ---->

;; (with [x ...] (+ x 2))
;; (with [x 1] (+ x 2))

;; ......

;; (with [x (+ 1 1)] (+ x 2))
;; --->
;; (f(x) = x + 2; f(1 + 1))

(test (calc* '(fun (x) (+ x x)))
      (funV 'x (add (id 'x) (id 'x)) (mtSub)))

(test (calc* '(+ 1 (+ 2 3)))
      6)

(test/exn (calc* '(+ 1 (fun (x) x)))
          "Not a number")

(test/exn (calc* '(5 1))
          "Not a function")

(test (calc* '(with [double (fun (x) (+ x x))]
                    (double 5)))
      10)

(test (calc* '(with [double (fun (x) (+ x x))]
                    (with [triple (fun (x) (+ x (double x)))]
                          (triple 5))))
      15)


(test (calc* '(with [double (fun (x) (+ x x))]
                    (with [triple (fun (x) (+ x (double x)))]
                          (with [double (fun (x) (- x 6))]
                                (triple 5)))))
      15)
