#lang racket

;; #define true false
;; #define assert(x) if ! x { blow up }

(define (! n)
  (if (zero? n)
      1
      (* n (! (sub1 n)))))

(! 10)
(! 10)
(! 30)

(define (time-this x)
  (define before (current-inexact-milliseconds))
  (define ans x)
  (define after (current-inexact-milliseconds))
  (printf "Took ~a ms\n" (- after before))
  ans)

(time-this (! 10))
(time-this (! 10))
(time-this (! 30))

(define (time-this2 x)
  (define before (current-inexact-milliseconds))
  (define ans (x))
  (define after (current-inexact-milliseconds))
  (printf "Took ~a ms\n" (- after before))
  ans)

(time-this2 (lambda () (! 10)))
(time-this2 (lambda () (! 10)))
(time-this2 (lambda () (! 30)))

(define-syntax-rule (time-this3 expr)
  (time-this2 (lambda () expr)))

(time-this3 (! 10))
(time-this3 (! 10))
(time-this3 (! 30))

(define-syntax-rule 
  (with ([id bound-expr]) body-expr)
  ((lambda (id) body-expr) bound-expr))

(with ([x 5]) (+ x x))

(with ([x 7])
      (+ x 
         (with ([x 5]) (+ x x))
         x))

(define-syntax with*
  (syntax-rules ()
    [(with* () body-expr)
     body-expr]
    [(with* ([id_0 bound-expr_0]
             [id_n bound-expr_n] ...)
            body-expr)
     (with ([id_0 bound-expr_0])
           (with* ([id_n bound-expr_n] ...)
                  body-expr))]))

(with* ([x 5]
        [y (+ x 1)]
        [z (* x y)])
       (list x y z))
(list 5 6 30)

(or #f 5)
(or 5 (error 'not-run))

(define-syntax-rule (my-or lhs rhs)
  (if lhs
      lhs
      rhs))

(my-or #f 5)
(my-or 5 (error 'not-run))
(my-or (printf "How many times do I print?\n")
       (error 'not-run))

(define-syntax-rule (my-or2 lhs rhs)
  (with ([ans lhs])
        (if ans
            ans
            rhs)))

(my-or2 #f 5)
(my-or2 5 (error 'not-run))
(my-or2 (printf "How many times do I print?\n")
       (error 'not-run))

(with ([ans 5])
      (my-or2 #f ans))

(with ([ans 5])
      (with ([ans #f])
        (if ans
            ans
            ans)))
