#lang plai

(test (+ 1 1) 2)
(test (+ 1 2) 2)

#|
0. a [25]
1. a [25]
2. (vector-ref a 25)
3. a [25]

|#

; [[ (lambda (x) x) ]] = math function "f(x) = x".


(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)])

(define ex1
  (add (num 1) (num 1)))

; <AE> :== <number>
;      |   (+ <AE> <AE>)
;      |   (- <AE> <AE>)

; parse: Sexpr -> AE
; Purpose: Parse an Sexpr into an AE
(define (parse se)
  (cond [(number? se) (num se)]
        [(and (list? se) (symbol=? '+ (first se)))
         (add (parse (second se))
              (parse (third se)))]
        [(and (list? se) (symbol=? '- (first se)))
         (sub (parse (second se))
              (parse (third se)))]))

(test (parse '(+ 1 1))
      (add (num 1) (num 1)))
