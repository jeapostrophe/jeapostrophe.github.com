#lang plai/mutator
(allocator-setup "1-25c.rkt" 60)

5
(cons 5 6)

(define (length l)
  (cond
    [(cons? l)
     (+ 1 (length (rest l)))]
    [else
     0]))

(length (cons 1 (cons 2 empty)))

(define x (cons 5 5))
(set-first! x x)
(set-rest! x x)