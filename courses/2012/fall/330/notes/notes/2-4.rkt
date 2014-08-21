#lang plai

;; Language's semantics

;; semantics = meaning of programs

;; interpreter semantics (or a program in Racket that determines the
;; meaning of a program)

;; Arithmetic expression (basic math expression)

;; 1 + 2
;; 1.add(2)
;; add(1, 2)
;; 1 2 +
;; + 1 2
;; (+ 1 2)

;;   add
;;   / \
;;  1  2

;;   add
;;   / \
;;  1  add
;;    /  \
;;   3   4

;; 1 + (3 + 4)

(define-type AE
  ;; variants
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)])

(add (num 1) (num 2))

;; Error
;; (add 1 2)

(add-lhs (add (num 1) (num 2)))

(+ 1 2)

;; parser : concrete syntax -> tree syntax (abstract syntax)

;; read : concrete syntax -> list syntax

;; (read)
;; Type: (+ 1 2)
;; (read) returns => (list '+ 1 2)

;; (read)
;; Type: (+ 1 (+ 2 3))
;; (read) returns => (list '+ 1 (list '+ 2 3))

;; (read)
;; Type: (expt 1 (+ 2 3))
;; (read) returns => (list 'expt 1 (list '+ 2 3))

(quote something)
;; the same as:
;; (read) typing "something"
;; return 'something

;; (quote something) is the same 'something

(define q-ex '(+ 1 2))
(list? q-ex)
(first q-ex)
(second q-ex)
(third q-ex)

;; list syntax is also called "s-expression" or "sexpr"

;; BNF - Backus-Naur Form

;; <AE> := <real Racket number>
;;       | (list '+ <AE> <AE>)
;;       | (list '- <AE> <AE>)

;; <AE> := <real Racket number>
;;       | (+ <AE> <AE>)
;;       | (- <AE> <AE>)


;; parse : sexpr -> AE
(define (parse se)
  (cond
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
    [else (error 'parse "Bad syntax... MAN")]))

(halt-on-errors #t)

(test (parse '1)
      (num 1))
(test (parse '(+ 2 1))
      (add (num 2) (num 1)))
(test (parse '(- 1 2))
      (sub (num 1) (num 2)))

(test/exn (parse '(+ 2 1 2))
          "Bad syntax")

;; <foo> <bar> </foo> </bar>            <--- not well-formed
;; <form> <table> .... </table> </form> <--- not valid

;; DTD for XHTML

;; calc :: AE -> number?
(define (calc ae)
  ;; (cond
  ;;   [(num? ae)
  ;;    (num-n ae)]
  ;;   [(add? ae)
  ;;    (+ (calc (add-lhs ae))
  ;;       (calc (add-rhs ae)))]
  ;;   [(sub? ae)
  ;;    (- (calc (sub-lhs ae))
  ;;       (calc (sub-rhs ae)))])
  (type-case
   AE ae
   [num
    (the-most-excellent-number)
    the-most-excellent-number]
   [add
    (lhs rhs)
    (+ (calc lhs)
       (calc rhs))]
   [sub
    (lhs rhs)
    (- (calc lhs)
       (calc rhs))]))

(test (calc (num 1))
      1)
(test (calc (add (num 2) (num 7)))
      9)

(test (calc (parse '(+ 2 7)))
      9)
(test (calc (parse '(+ 2 9)))
      11)

;; calc* : sexpr -> number?
(define (calc* se)
  (calc (parse se)))

(test (calc* '11)
      11)
(test (calc* '(+ 2 7))
      9)
(test (calc* '(+ 2 9))
      11)
(test (calc* '(- 2 9))
      -7)
