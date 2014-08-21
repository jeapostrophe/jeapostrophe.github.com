#lang plai
(halt-on-errors #t)

;; 1 + 1
;; 1.add(1)
;; add(1,1)
;; 1 1 + (reverse polish notation)
;; + 1 1
;; (+ 1 1)

;; HONU

;; add... two args: 1 and 2
;; syntax ... is a really a tree, where nodes are functions (or
;; structures) and children are arguments

;;     add
;;     / \
;;    1  1

;;     add
;;     / \
;;    1  add
;;       / \
;;      1   1

(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)])

(add (num 1) (num 1))

;; Error
;; (add 1 1)

;; parsing!

;; parsing = tokenization + reading + parsing

;; (read)
;; Type: (+ 1 1)
;; read returns: (list '+ 1 1)

;; s-expression or sexpr
(define q-ex '(+ 1 1))
(first q-ex)
(second q-ex)
(third q-ex)

;; BNF - Backus-Naur FORM

;; <AE> := <real Racket number>
;;       | (+ <AE> <AE>)
;;       | (- <AE> <AE>)

;; parse :: s-expression -> AE
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
    [else
     (error 'parse "Invalid syntax, dude")]))

(test (parse '1)
      (num 1))
(test (parse '(+ 1 1))
      (add (num 1) (num 1)))
(test (parse '(- 1 1))
      (sub (num 1) (num 1)))

(test/exn (parse "1")
          "Invalid syntax")

(add (num 1) (num 1)) "means"
2

;; calc : AE? -> number?
;; compute the meaning of the AE
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
(test (calc (add (num 1) (num 1)))
      2)

;; calc* : sexpr -> number?
(define (calc* se)
  (calc (parse se)))

(test (calc* '1)
      1)
(test (calc* '(+ 1 1))
      2)
(test (calc* '(- 0 1))
      -1)

