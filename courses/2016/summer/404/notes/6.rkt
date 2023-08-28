#lang racket/base
(require plot)

(define (insert-best-cost n) 1)
(define (insertion-sort-best-cost n)
  (if (<= n 1)
      1
      (+ (insertion-sort-best-cost (- n 1))
         (insert-best-cost n))))

(define (insert-worst-cost n) n)
(define (insertion-sort-worst-cost n)
  (if (<= n 1)
      1
      (+ (insertion-sort-worst-cost (- n 1))
         (insert-worst-cost n))))

(module+ test
  (plot-new-window? #t)
  #;(plot (list
           (function
            #:color 0
            #:label "best"
            (λ (n)
              (insertion-sort-best-cost
               (round n))))
           (function
            #:color 1
            #:label "worst"
            (λ (n)
              (insertion-sort-worst-cost
               (round n)))))
          #:x-min 0
          #:x-max 100
          #:y-min 0)

  (define (isort-cost/inversion size inv)
    ;; Average cost of all permutations of size `size` with `inv`
    ;; inversions
    (+ (* (- size inv) 1)
       (* size inv)))

  (plot3d (surface3d
           (λ (size inv)
             (isort-cost/inversion
              (round size)
              (round (min size inv)))))
          #:x-min 0
          #:x-max 100
          #:x-label "size"
          #:y-min 0
          #:y-max 100
          #:y-label "inversions"
          #:z-label "cost"))
