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
  (allocator-setup "1-32hc.rkt" 20)
  (print-only-errors #t)
  (halt-on-errors #t)

  (first (cons (+ 1 2) 4))
  (first (cons (+ 1 2) 4))
  1

  (define x0 1)
  (define x1 1)
  (define x2 1)
  (define x3 1)
  (define x4 1)
  (define x5 1)
  (define x6 1)
  (define x7 1)
  (define x8 1)
  (define x9 1)
  6)

;; (require 'errors)

(module cyclic-cons plai/mutator
  (allocator-setup "1-32hc.rkt" 6)
  (print-only-errors #t)
  (halt-on-errors #t)

  (define x (cons 1 2))
  (set-rest! x x)

  3
  (first x))

(require 'cyclic-cons)

(module fake-roots-cons plai/mutator
  (allocator-setup "1-32hc.rkt" 10)
  (print-only-errors #t)
  (halt-on-errors #t)

  3 4

  (define x (cons 1 2))
  (test/value=? (first x) 1)
  (test/value=? (rest x) 2)

  5 6

  (test/value=? (first x) 1)
  (test/value=? (rest x) 2))

(require 'fake-roots-cons)

(module fake-roots-proc plai/mutator
  (allocator-setup "1-32hc.rkt" 6)
  (print-only-errors #t)
  (halt-on-errors #t)

  (define (f x)
    (λ () x))

  (define g (f 0))

  (test/value=? (g) 0)
  (test/value=? (g) 0)
  (test/value=? (g) 0)

  4

  (test/value=? (g) 0))

;; (require 'fake-roots-proc)

(module cycle-through-proc plai/mutator
  (allocator-setup "1-32hc.rkt" 6)
  (print-only-errors #t)
  (halt-on-errors #t)

  (define (f x)
    (λ () x))

  (define t (cons f f))

  (define g (f t))

  (set-first! t g)
  (set-rest! t g)
  (set! f g)
  (set! t g)

  0

  (test/location=? g (first (g)))
  (test/location=? g (rest (g))))

(require 'cycle-through-proc)
