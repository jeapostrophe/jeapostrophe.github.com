#lang racket/base
(require plot)

(define (T n) (* n n))

(define (Q-sub-call-T q n)
  (+ (T q)
     (T (- n (+ q 1)))))
(define (maximize f i t)
  (for/fold ([maximum -inf.0])
            ([q (in-range i (+ t 1))])
    (max maximum (f q))))
(define (Q n)
  (maximize (λ (q) (Q-sub-call-T q n))
            0 (- n 1)))

(define (lg n) (/ (log n) (log 2)))
(define (best-body q n)
  (define (T n) (* n (lg n)))
  (+ (T q)
     (T (- n (+ q 1)))))

(module+ main
  (plot-new-window? #t)

  #;
  (plot (function (λ (x) (best-body x 100)))
        #:x-label "q"
        #:y-label "cost"
        #:x-min 0
        #:x-max 100)

  #;
  (plot (function (λ (x) (Q-sub-call-T x 100)))
        #:x-min 0
        #:x-max 100)

  #;
  (plot3d
   (surface3d
    (λ (x y)
      (Q-sub-call-T (min x y) y)))
   #:x-min 0
   #:y-min 0
   #:x-max 100
   #:y-max 100)
  
  #;
  (plot (list (function Q
                        #:color 0
                        #:label "Q")
              (function T
                        #:color 1
                        #:label "T"))
        #:x-min 0
        #:x-max 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (quicksort A p r)
  (when (< p (- r 1))
    (define q (partition A p r))
    (quicksort A p q)
    (quicksort A (+ q 1) r)))
(define (swap! A i j)
  (define Ai (vector-ref A i))
  (vector-set! A i (vector-ref A j))
  (vector-set! A j Ai))
(define (partition A p r)
  (define x (vector-ref A (- r 1)))
  (define i p)
  (for ([j (in-range p r)])
    (when (<= (vector-ref A j) x)
      (swap! A i j)
      (set! i (+ i 1))))
  (swap! A i (- r 1))
  i)

#;(define (insertion-sort A p r)
  (for ([j (in-range (+ p 1) r)])
    (define key (vector-ref A j))
    (for ([i (in-range (- j 1) (+ p 1))]
          #:stop-when (<= (vector-ref A i) key))
      (vector-set! A (+ i 1)
                   (vector-ref A i)))
    (vector-set! A (+ i 1) key)))

#;
(module+ test
  (require racket/list
           chk)
  (define N 4)
  (define t (build-list N add1))

  (define (quicksort-list l)
    (define A (list->vector l))
    (quicksort A 0 (- (vector-length A) 1))
    (vector->list A))

  (quicksort-list '(7 4 7 2 3))
  #;
  (for ([p (in-permutations t)])
    (define A (list->vector p))
    (quicksort A 0 (- N 1))
    (chk (vector->list A) t)))
