#lang racket/load

;; Plan:
;; - errors
;; - simplify alloc
;; - m&s
;; -- free list
;; -- odd heap size
;; -- marking
;; -- procedure-roots
;; -- cons roots
;; - s&c
;; -- semi-spaces
;; -- copying
;; -- cycles
;; -- procedure-roots
;; -- cons roots

(module errors plai/mutator
  (allocator-setup "2-32hc.rkt" 12)
  (print-only-errors #t)
  (halt-on-errors #t)

  (define x 1)
  (define y 2)

  ;; (first x)
  ;; (rest x)

  ;; (set-rest! x 3)
  ;; y

  (+ 1 (cons x y))

  )

;; (require 'errors)

(module alot plai/mutator
  (allocator-setup "2-32hc.rkt" 13)
  (print-only-errors #t)
  (halt-on-errors #t)

  1 (cons 2 3) 4 5
  6

  (define x (cons 1 (cons 2 (cons 3 4)))))

;; (require 'alot)

(module cycles plai/mutator
  (allocator-setup "2-32hc.rkt" 6)
  (print-only-errors #t)
  (halt-on-errors #t)

  (define x (cons 1 2))
  (set-rest! x x)

  3
  (first x))

(require 'cycles)

(module fake-root-cons plai/mutator
  (allocator-setup "2-32hc.rkt" 6)
  (print-only-errors #t)
  (halt-on-errors #t)

  3

  (define x (cons 1 2))

  (test/value=? (first x) 1)
  (test/value=? (rest x) 2))

(require 'fake-root-cons)

(module proc plai/mutator
  (allocator-setup "2-32hc.rkt" 6)
  (print-only-errors #t)
  (halt-on-errors #t)

  (define (f x)
    (λ () x))
  (define g (f 0))

  (test/value=? (g) 0)
  3
  (test/value=? (g) 0))

;;(require 'proc)

(module cycle-proc plai/mutator
  (allocator-setup "2-32hc.rkt" 6)
  (print-only-errors #t)
  (halt-on-errors #t)

  (define (f x)
    (λ () x))
  (define t (cons f f))
  (define g (f t))

  (set-first! t g)
  (set-rest! t g)
  (set! t g)

  (test/location=? (first (g)) g)
  (test/location=? (rest (g)) g)
  3
  (test/value=? (g) 0))

(require 'cycle-proc)
