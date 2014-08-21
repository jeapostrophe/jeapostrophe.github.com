#lang plai

(test (+ 1 1) 2)
(test (+ 1 1) 3)

#|
0. a [25]
1. a [25]
2. (vector-ref a 25)
3. a [25]
|#

#|
1 + 1
1 1 +
(+ 1 1)

one PLUS one

(add (num 1) (num 1))
|#

(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)])

(define ex1
  (add (num 1) (num 2)))

; <AE> :==
;         | <number>
;         | (+ <AE> <AE>)
;         | (- <AE> <AE>)

; parse : Sexpr -> AE
; Purpose: To accept AE style Sexprs and turn them into AEs
(define (parse se)
  (cond [(number? se) (num se)]
        [(and (list? se) (symbol=? '+ (first se)))
         (add (parse (second se))
              (parse (third se)))]
        [(and (list? se) (symbol=? '- (first se)))
         (sub (parse (second se))
              (parse (third se)))]))

(test (parse '(+ 1 2))
      (add (num 1) (num 2)))

#|
'(+ 1 2 3)

(require xml)
'(p "Hello World" (p "Bad"))
(xexpr->string '(a ([href "/"] [alt "Label"]) "Blah"))

'(a (@ [href "/"] [alt "Label"]) "Blah")
|#

#|

(f ∘ g) x = f ( g (x) )

|#