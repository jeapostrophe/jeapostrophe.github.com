#lang plai

;; Language = "paradigms" + syntax + semantics + libraries/support for stuff + name + cultural things

;; Language = semantics

#|
1. a [5]
2. (vector-ref a 5)
3. a [5]
4. a [5]
|#

;; Qvickbasic

;; Brainf*ck

;; semantics
;;; compile to math = denotational semantics
;;; axiomatic semantics
;;; operational semantics = programing with math

;;; interpreter semantics

;; 1 + 1
;; + 1 1
;; 1 1 +

;(define-struct num (the-number))
;(define-struct add (the-lhs the-rhs))

;; abstract syntax
(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)])

;; abstract program
(add (num 1)
     (num 1))


;; concrete program
(define e '(+ 1 1))
e

;; concrete syntax
#|
AE = <number>
   | (+ <AE> <AE>)
|#

;; parse : concrete -> abstract
(define (parse c)
  (cond
   [(number? c)
    (num c)]
   [else
    (add (parse (second c))
         (parse (third c)))]))

(parse e)