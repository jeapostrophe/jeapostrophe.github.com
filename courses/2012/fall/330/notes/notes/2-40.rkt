#lang racket

;; Languages have...
;; -- notation
;; -- natural data types
;; -- natural control constructs
;; -- code organization systems... functions, classes, modules,
;; -- automation... GC, type system, optimization

;; Domain-specific languages
;; -- langs customized for some domain
;; -- implement one...
;; --- natural path: parser -> interpreter/compiler ->
;;                          development tools + standard libraries
;;   ---- expensive!

;; Racket is designed to make DSLs
;; --- easy parser -> compiler to Racket -> embeded inside of Racket

;; First verse of the scriptures: Programming languages should not be
;; designed by piling feature ontop of feature, but by removing the
;; inconsistencies and restrictions that make new features appear
;; necessary.

(define (time-it* f)
  (define start (current-inexact-milliseconds))
  (displayln (f))
  (define end (current-inexact-milliseconds))
  (- end start))

(define-syntax-rule (time-it e)
  (time-it* (λ () e)))

;; #define time(e) ....

(time-it (expt 2 5000))
(time-it (expt 2 10000))

;; #define foo(x) { 6

;; #define bar(x) do { x } while (0);
;; if (cond) bar(int x = 1; y += x;)

(define-syntax-rule (with ([bound-id bound-body]) body)
  ((λ (bound-id) body) bound-body))

(with ([x 5]) (+ x x))

(with ([time-it* 2])
      (time-it (expt time-it* 6000)))

(define-syntax-rule (naive-with* ([bound-id bound-body] ...) body)
  ((λ (bound-id ...) body) bound-body ...))

(define-syntax with*
  (syntax-rules ()
    [(with* () body)
     body]
    [(with* ([bound-id_0 bound-body_0]
             [bound-id_n bound-body_n]
             ...)
            body)
     (with ([bound-id_0 bound-body_0])
           (with* ([bound-id_n bound-body_n]
                   ...)
                  body))]))

(with* ([x 5]
        [y 7]
        [z (+ x y)])
       (list x y z))

;; ((λ (x y z) (list x y z)) 5 7 (+ x y))

((λ (x)
   ((λ (y)
      ((λ (z)
         (list x y z))
       (+ x y)))
    7))
 5)

;; You won't need more than this for prolog

(define-syntax-rule (jor lhs rhs)
  (with ([lhs-v lhs])
        (if lhs-v lhs-v rhs)))

(jor #t (/ 1 0))

(jor (time-it (+ 1 1)) 7)

(with ([x 44])
      (jor #f x))

;; #define zog(x) { int ___dont_use_x = 0; x += ___dont_use_x; }
;; zog(zog(7))

(with ([lhs-v 44])
      (jor #f lhs-v))

(with ([lhs-v 44])
      (with ([lhs-v #f])
            (if lhs-v lhs-v lhs-v)))

;; Hygeine

(define-syntax (for stx)
  (printf "You gave m: ~v\n" stx)
  (define x (read))
  (printf "You gave m: ~v\n" x)  
  (syntax-case stx (from to do)
    [(for from start to end do body ...)
     (with-syntax ([it (datum->syntax stx 'it)])
       (syntax
         (let loop ([it start])
           (unless (= it end)
             body ...
             (loop (add1 it))))))]))

(define it 15)
(for from 7 to 10 do
     (displayln it))
