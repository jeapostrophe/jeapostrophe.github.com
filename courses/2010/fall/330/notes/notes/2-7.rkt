#lang plai

(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)])

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
(test (parse '(- 1 2))
      (sub (num 1) (num 2)))

; calc : AE -> number
; Purpose: To compute the number represented by the AE
(define (calc ae)
  #;(cond [(num? ae) (local [(define the-number (num-n ae))] 42)]
          [(add? ae) ...]
          [(sub? ae) ...])
  #;(type-case AE ae
    [num (the-number) 42]
    [add (left right) 42]
    [sub (left right) 42])
  #;(type-case AE ae
    [num (the-number) 
         ... the-number ...]
    [add (left right) 
         ... (calc left) ...
         ... (calc right) ...]
    [sub (left right)
         ... (calc left) ...
         ... (calc right) ...])
  (type-case AE ae
    [num (the-number) 
         the-number]
    [add (left right) 
         (+ (calc left)
            (calc right))]
    [sub (left right)
         (- (calc left)
            (calc right))]))

(test (calc (parse '0))
      0)
(test (calc (parse '(+ 1 1)))
      2)
(test (calc (parse '(- 2 1)))
      1)
(test (calc (parse (list '- 2 1)))
      1)
(test (calc (parse (list '- 2 (list '- 2 1))))
      1)
(test (calc (parse '(- (+ 1 2) (- 8 9))))
      4)

; 2^n-addition : number -> AE
; Purpose: To produce an AE that computes 2^n, with n layers of addition
(define (2^n-addition n)
  (cond [(zero? n)
         (num 1)]
        [else
         (local [(define one-less-than-n (sub1 n))
                 (define one-less-AE
                   (2^n-addition one-less-than-n))]
           (add one-less-AE one-less-AE))]))

(test (2^n-addition 0)
      (num 1))
(test (2^n-addition 1)
      (add (num 1) (num 1)))
(test (2^n-addition 2)
      (add (add (num 1) (num 1))
           (add (num 1) (num 1))))

(test (calc (2^n-addition 10))
      (expt 2 10))

;;;
(require (for-syntax racket/local))
(define-syntax (racket-2^n-addition stx)
  (syntax-case stx ()
    [(racket-2^n-addition stx-of-a-number)
     (local [(define number (syntax->datum (syntax stx-of-a-number)))
             (define one-less-than-the-number
               (sub1 number))]
       (with-syntax ([stx-of-one-less-than-the-number
                      one-less-than-the-number])
         (if (zero? number)
             (syntax 1)
             (syntax (+ (racket-2^n-addition stx-of-one-less-than-the-number)
                        (racket-2^n-addition stx-of-one-less-than-the-number))))))]))

(test (racket-2^n-addition 10)
      (expt 2 10))
