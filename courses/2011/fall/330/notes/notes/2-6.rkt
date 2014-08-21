#lang plai

;; Language = actions + actors + acteds + 2 Nephi + syntax/grammar + implementation details + culture + libraries/stuff it does

;; Language = semantics

;; Which are the same:
;; 1. a[5]
;; 2. (vector-ref a 5)
;; 3. a[5]
;; 4. a[5]

;; Language = semantics

;; Semantics
;;; denotational = compile into math
;;; dictatorial fiat = Python and Ruby
;;; English specification = JavaScript
;;; axiomatic
;;; operational = write an interpreter in math

;;; interpreter = write an interpreter in an existing language (hopefully, a well behaved one)


;; concrete syntax (BNF = Backus-Naur Form)
; AE = <number>
;    | ( + <AE> <AE> )

(define e '(+ 1 1))
e

;; abstract syntax
;(define-struct num (n))
;(define-struct add (lhs rhs))
(define-type AE
  [num (n number?)]
  [add (lhs AE?) (rhs AE?)])

(define p 
  (add (num 1) (num 1)))
p

; parse : concrete -> program
(define (parse c)
  (cond
    [(number? c)
     (num c)]
    [(list? c)
     (add (parse (second c))
          (parse (third c)))]))

(parse e)

; interp : program -> meaning