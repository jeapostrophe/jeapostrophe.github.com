#lang racket/base
(require plot
         racket/file
         racket/math)

(module+ test
  (plot-new-window? #t)

  (define N 10000)
  (define m -5)
  (define M 5)
  (define Bs 100)

  (define (samples->hash m M Bs s)
    (define interval (/ (- M m) Bs))
    (for/fold ([h (hash)])
              ([e (in-list s)])
      (define i
        (+ m
           (* interval
              (round
               (/ (- e m) interval)))))
      (hash-update h i add1 0)))

  (define (samples->rects s N)
    (define interval (/ (- M m) Bs))
    (define vs
      (for/list ([(k v) (samples->hash m M Bs s)])
        (list (ivl (- k (* 1/2 interval))
                   (+ k (* 1/2 interval)))
              (ivl 0 (/ (/ v N) interval)))))
    (rectangles vs))

  (define (normal-pdf mu sig)
    (λ (x)
      (* (/ 1 (* sig (sqrt (* 2 pi))))
         (exp (- (/ (expt (- x mu) 2)
                    (* 2 (expt sig 2))))))))

  (define (erf x)
    (* (sign x)
       (/ 1 2)
       (expt
        (- 1 (exp (* -1 (/ 2 pi) (expt x 2))))
        (/ 1 2))))

  (define (sign x)
    (cond
     [(positive? x) +1.0]
     [(negative? x) -1.0]
     [else 0.0]))

  (define a 0.140012)
  (define (inv-erf x)
    (* (sign x)
       (sqrt (- (sqrt (- (expt (+ (/ 2 (* pi a))
                                  (/ (log (- 1 (expt x 2)))
                                     2))
                               2)
                         (/ (log (- 1 (expt x 2)))
                            a)))
                (+ (/ 2 (* pi a))
                   (/ (log (- 1 (expt x 2)))
                      2))))))

  (define (normal-cdf mu sig)
    (λ (x)
      (* (/ 1 2)
         (+ 1
            (erf (/ (- x mu)
                    (* sig (sqrt 2))))))))

  (define (inv-phi p)
    (* (sqrt 2) (inv-erf (- (* 2 p) 1))))

  (define (normal-inv-cdf mu sig)
    (λ (p)
      (+ mu (* sig (inv-phi p)))))
  
  (define (nonzero-random)
    (let ([u  (random)])
      (if (= u 0.0) (nonzero-random) u)))

  (define (normal-sample:box-muller mu sig)
    (λ ()
      (define u1 (nonzero-random))
      (define u2 (random))
      (define x (* (sqrt (* -2.0 (log u1)))
                   (sin (* (* 2.0 pi) u2))))
      (+ mu (* sig x))))
  
  (define (normal-sample:inverse mu sig)
    (λ ()
      ((normal-inv-cdf mu sig) (random))))

  (define (sample-n sample-one n)
    (for/list ([i (in-range n)])
      (sample-one)))
  
  (define given-points
    (file->value "17.rktd"))

  (define (normal mu sig)
    (list (samples->rects 
           #;given-points
           (sample-n (normal-sample:box-muller mu sig)
            #;(normal-sample:inverse mu sig)
            N)
           N)
          (function (normal-pdf mu sig)
                    #;(normal-cdf mu sig)
                    #:color 0 #:label (format "N(~a,~a)" mu sig))))

  (plot (list (normal 0 1))
        #:x-min m #:x-max M #:y-label "density"))
