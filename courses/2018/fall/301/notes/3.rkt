#lang racket/base
(define zero (λ (f) (λ (z) z)))
(define one (λ (f) (λ (z) (f z))))

(define plus
  (λ (x)
    (λ (y)
      (λ (f)
        (λ (z)
          ((y f) ((x f) z)))))))

(define two ((plus one) one))
(define four ((plus two) two))

four

((four add1) 0)