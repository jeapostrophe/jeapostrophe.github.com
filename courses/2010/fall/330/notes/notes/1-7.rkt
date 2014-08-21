#lang plai

(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)])

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
(test (parse '(- 1 2))
      (sub (num 1) (num 2)))

; calc : AE -> number
; Purpose: To determine the number computed/represented by the AE
(define (calc ae)
  #;(cond [(num? ae) (local [(define n (num-n ae))] 42)]
          [(add? ae) ...]
          [(sub? ae) ...])
  #;(type-case AE ae
      [num (n) ...]
      [add (lhs rhs) ...]
      [sub (lhs rhs) ...])
  #;(type-case AE ae
    [num (n) ...]
    [add (lhs rhs) 
         ... (calc lhs) ...
         ... (calc rhs) ...]
    [sub (lhs rhs)
         ... (calc lhs) ...
         ... (calc rhs) ...])
  (type-case AE ae
    [num (n) n]
    [add (lhs rhs) 
         (+ (calc lhs)
            (calc rhs))]
    [sub (lhs rhs)
         (- (calc lhs)
            (calc rhs))]))

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

; 2^n-addition : number -> AE
; Purpose: To create an AE that performs n levels of addition
(define (2^n-addition n)
  (cond [(zero? n)
         (num 1)]
        [else
         (local [(define one-less (2^n-addition (sub1 n)))]
           (add one-less one-less))]))

(test (2^n-addition 0)
      (num 1))
(test (2^n-addition 1)
      (add (num 1) (num 1)))
(test (2^n-addition 2)
      (add (add (num 1) (num 1))
           (add (num 1) (num 1))))

(test (calc (2^n-addition 10))
      (expt 2 10))

(require (for-syntax racket))
(define-syntax (racket-2^n-addition stx)
  (syntax-case stx ()
    [(racket-2^n-addition stx-of-a-number)
     (local [(define number (syntax->datum (syntax stx-of-a-number)))
             (define one-less-than-number (sub1 number))]
       (with-syntax ([stx-of-one-less-than-number one-less-than-number])
         (if (zero? number)
             (syntax 1)
             (syntax (+ (racket-2^n-addition stx-of-one-less-than-number)
                        (racket-2^n-addition stx-of-one-less-than-number))))))]))

(test (racket-2^n-addition 10)
      (expt 2 10))

