#lang racket
(require plot)

(define i 0)
(define-syntax-rule (++ x)
  (begin (set! x (add1 x)) x))

(define-syntax-rule (function* f)
  (function f 
            #:label (format "~a" 'f)
            #:color (++ i)))

(parameterize ([plot-y-ticks (time-ticks)])
  (plot (list 
         (function* (lambda (n) (* 4 n)))
         (function* (lambda (n) (+ 6 (* 2 n))))
         (function* (lambda (n) (* n n)))
         (function* (lambda (n) (* n n n)))
         (function* (lambda (n) (expt 2 n))))
        #:x-min 0
        #:x-max 2000))
