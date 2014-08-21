#lang racket

;; Macro system

;; Excel or C

;; CPP Macros

;;; #DEFINE true false
;;; #DEFINE NULL 0
;;; #DEFINE ASSERT(X) 
;;; #DEFINE SOMETHING_DUMB(X) {int ____y = X; y + 1;

;; Racket macros

(define (! n)
  (if (zero? n)
      1
      (* n (! (sub1 n)))))

(define (time-this x)
  (define before (current-inexact-milliseconds))
  (define ans x)
  (define after (current-inexact-milliseconds))
  (printf "Took ~a ms\n" (- after before))
  ans)

(time-this (! 10))
(time-this (! 20))

(define (time-this2 find-x)
  (let ([before (current-inexact-milliseconds)])
    (let ([ans (find-x)])
      (define after (current-inexact-milliseconds))
      (printf "Took ~a ms\n" (- after before))
      ans)))

(time-this2 (lambda () (! 10)))
(time-this2 (lambda () (! 10)))
(time-this2 (lambda () (! 30)))

;; #DEFINE timeThis3(computation) timeThis2(\\.computation)
(define-syntax-rule (time-this3 computation)
  (time-this2 (lambda () computation)))

(time-this3 (! 10))
(time-this3 (! 10))
(time-this3 (! 30))

(define-syntax-rule (with (id bound-expr) body-expr)
  ((lambda (id) body-expr) bound-expr))

(define-syntax-rule (my-let ((id bound-expr) ...) body-expr)
  ((lambda (id ...) body-expr) bound-expr ...))

(with (x 5)
      (+ x x))

(with (x 7)
      (+ x
         (with (x 5)
               (+ x x))
         x))

(define x 5)
(with (x 7)
      (+ x x))

(define-syntax withlike
  (syntax-rules ()
    [(withlike () body-expr)
     body-expr]
    [(withlike ([id0 bound-body0]
                [idn bound-bodyn]
                ...)
               body-expr)
     (with (id0 bound-body0)
           (withlike ([idn bound-bodyn]
                      ...)
                     body-expr))]))

(withlike ([x 7]
           [y (+ x 3)]
           [z (* 2 y)])
          (list x y z))
(list 7 10 20)

(define-syntax my-cond
  (syntax-rules (else)
    [(my-cond [else expr])
     expr]
    [(my-cond [test ans] more-clauses ...)
     (if test
         ans
         (my-cond more-clauses ...))]))

(my-cond
 [true 1]
 [(error 'false) 2]
 [else 3])

(my-cond
 [false 1]
 [true 2]
 [else 3])

(my-cond
 [false 1]
 [false 2]
 [else 3])

(or #f 4)
(or 5 (error #f))

(define-syntax-rule (my-or lhs rhs)
  (if lhs
      lhs
      rhs))

(my-or #f 4)
(my-or 5 (error #f))
(my-or (printf "How many times do I get printed?\n") #f)

;; Scheme-style macros and Lisp-style macros

;;; Hygeinic

(define-syntax-rule (my-or2 lhs rhs)
  (with (ans lhs)
    (if ans
        ans
        rhs)))

(my-or2 #f 4)
(my-or2 5 (error #f))
(my-or2 (printf "How many times do I get printed?\n") #f)

(with (ans 5)
      (with (ans #f)
            (if ans
                ans
                ans)))

(with (ans 5)
      (my-or2 #f ans))
