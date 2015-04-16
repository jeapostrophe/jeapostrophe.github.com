#lang racket/base
(require plot)

(define (sqr x)
  (* x x))

(module+ main
  (plot-new-window? #t))

(module+ main
  (define N 100)
  (define rand-points (build-vector N (λ (i) (random))))
  (define ((make-before floor) x)
    (vector-ref rand-points
                (max 0 (min (sub1 N) (inexact->exact (floor (* x N)))))))
  (define before (make-before floor))
  (define after (make-before ceiling))
  (define (lerp from to p)
    (+ (* (- 1 p) from)
       (* p to)))
  (define (f x)
    (lerp (before x) (after x) x))

  (define (temperature %)
    (- 1 %))

  (define (random-neighbor T s)
    (max 0 (min 1 (+ s (* T (- (random) 0.5))))))

  (define (P e ep T)
    (if (< ep e)
        1
        (exp (- (/ (- ep e) T)))))

  (define (simulated-annealing f s_0 k_max)
    (for/fold ([s s_0]) ([k (in-range k_max)])
      (printf "~v. ~v\n" k s)
      (define T (temperature (/ k k_max)))
      (define s_new (random-neighbor T s))
      (if (>= (P (f s) (f s_new) T)
              (random))
          s_new
          s)))

  (define sa-r
    (simulated-annealing f 0.5 2000))

  (plot (list (function f)
              (points (list (vector sa-r (f sa-r)))))
        #:x-min 0
        #:x-max 1))

(module+ mainA
  (define W 10)

  (define f
    (λ (x1 x2)
      (+ (* (+ 4 (* -2.1 (sqr x1)) (/ (expt x1 4) 3))
            (sqr x1))
         (* x1 x2)
         (* (+ -4 (* 4 (sqr x2)))
            (sqr x2)))))

  (f -1 -1)

  (plot3d (surface3d f)
          #:x-min (- W)
          #:x-max W
          #:y-min (- W)
          #:y-max W))
