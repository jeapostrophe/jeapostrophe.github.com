#lang plai
(print-only-errors #t)

(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)]
  [id (sym symbol?)]
  [with (new-name symbol?)
        (named-thing AE?)
        (body AE?)]
  [app (fun symbol?)
       (arg AE?)])

;; <AE> := <real Racket number>
;;       | (+ <AE> <AE>)
;;       | (- <AE> <AE>)
;;       | <id>
;;       | (with [<id> <AE>] <AE>)
;;       | (<id> <AE>)

;; where <id> is any Racket symbol, except +, -, and with

;; parse :: s-expression -> AE
(define (parse se)
  (cond
    [(and (list? se)
          (= 2 (length se))
          (symbol? (first se))
          (not (member (first se) '(+ - with))))
     (app (first se)
          (parse (second se)))]
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
      (app 'double (num 5)))

(test/exn (parse "1")
          "Invalid syntax")

;; subst : symbol AE AE -> AE
(define (subst name named-thing body)
  (type-case
   AE body
   [app
    (fun arg)
    (app fun
         (subst name named-thing arg))]
   [id
    (sym)
    (if (equal? name sym)
      named-thing
      body)]
   [with
    (inner-name inner-named-thing inner-body)
    (if (equal? inner-name name)
      (with inner-name
            (subst name named-thing inner-named-thing)
            inner-body)
      (with inner-name
            (subst name named-thing inner-named-thing)
            (subst name named-thing inner-body)))]
   [add
    (lhs rhs)
    (add (subst name named-thing lhs)
         (subst name named-thing rhs))]
   [sub
    (lhs rhs)
    (sub (subst name named-thing lhs)
         (subst name named-thing rhs))]
   [num
    (n)
    body]))

;; C perspective... linker

;; First-order function (language doens't define functions, just uses
;; them)

(define-type FunDef
  [a-FunDef (name symbol?)
            (param symbol?)
            (body AE?)])
(define double-def
  (a-FunDef 'double
            'x
            (parse '(+ x x))))
(define triple
  (a-FunDef 'triple
            'x
            (parse '(+ x (double x)))))

;; lookup-fun : symbol lofds -> FunDef
(define (lookup-fun fun fs)
  (cond
    [(empty? fs)
     (error 'calc "SIGSEGV")]
    [(equal? fun (a-FunDef-name (first fs)))
     (first fs)]
    [else
     (lookup-fun fun (rest fs))]))

;; calc/funs : AE? lofds -> number?
;; compute the meaning of the AE
(define (calc/funs ae fs)
  (type-case
   AE ae
   [app
    (fun arg)
    (type-case
     FunDef (lookup-fun fun fs)
     [a-FunDef
      (fun-name param body)
      ;; (calc/funs (subst param (num (calc/funs arg fs)) body) fs)
      (calc/funs (with param arg body) fs)])]
   [id
    (sym)
    (error 'calc "You has a bad identifier, bro: ~e" sym)]
   [with
    (name named-thing body)
    ;; Inefficient -- Lazy' semantics
    ;; (calc (subst name named-thing body))
    ;; Efficient -- Eager semantics
    (calc/funs (subst name (num (calc/funs named-thing fs)) body) fs)]
   [num
    (n)
    n]
   [add
    (lhs rhs)
    (+ (calc/funs lhs fs)
       (calc/funs rhs fs))]
   [sub
    (lhs rhs)
    (- (calc/funs lhs fs)
       (calc/funs rhs fs))]))

;; calc* : sexpr -> number?
(define (calc* se)
  (calc/funs (parse se) empty))

;; calc*/funs : sexpr lofds -> number?
(define (calc*/funs se fs)
  (calc/funs (parse se) fs))

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
(test (calc* '(with [x (+ 5 5)]
                    (with [x 7]
                          (+ x x))))
      14)
(test (calc* '(with [x (+ 5 5)]
                    (+ x (with [x 7]
                               (+ x x)))))
      24)
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
(test (calc* '(with [x 6]
                    (+ x x)))
      12)

(test (calc*/funs '(double (+ 5 5))
                  (list double-def))
      20)
(test (calc*/funs '(double 6)
                  (list double-def))
      12)
(test (calc*/funs '(with [x 5] (double x))
                  (list double-def))
      10)

(test (calc*/funs '(triple (+ 5 5))
                  (list double-def triple))
      30)
(test (calc*/funs '(triple 6)
                  (list double-def triple))
      18)
(test (calc*/funs '(with [x 5] (triple x))
                  (list double-def triple))
      15)

;;(test (calc*/funs '(f 1)
;;                  (list (a-FunDef 'f 'x (parse '(f x)))))
;;      42)

(test (calc*/funs '(with [double 5] (double (triple double)))
                  (list double-def triple))
      30)

;; Two "namespaces" --- one for functions and one for identifiers
;; 2-Lisps (have two) [original, Common Lisp, Javascript] 
;;  and 1-Lisps (have one) [C, Clojure, Scheme, Racket]

;; Java... a 2-lisp

;; class Stupid {
;;               int x (float f) { return 0; }
;;               int main () {
;;                            float x = 3.0;
;;                            x(x);
;;                            }
;;               }

(test/exn
 (calc*/funs '(with [x 5]
                    (f 17))
             (list (a-FunDef 'f 'n (parse '(+ n x)))))
 "bro")
