#lang racket/base
(require plot
         math/bigfloat
         racket/pretty
         racket/file
         racket/list)

(define (newton-1d fp fpp x_0
                   #:tolerance [tol (bf 0.00001)]
                   #:count [count 1000])
  (define vp (fp x_0))
  (cond
   [(or (zero? count)
        (bf<= (bfabs vp) tol))
    empty]
   [else
    (define vpp (fpp x_0))
    (define x_1 (bf- x_0 (bf/ vp vpp)))
    (cons x_1
          (newton-1d fp fpp x_1
                     #:tolerance tol
                     #:count (sub1 count)))]))

(define (gradient-1d fp x_0 step
                     #:tolerance [tol (bf 0.00001)]
                     #:count [count 1000])
  (define v (fp x_0))
  (cond
   [(or (zero? count) 
        (bf<= (bfabs v) tol))
    empty]
   [else
    (define x_1 (bf- x_0 (bf* step v)))
    (cons x_1
          (gradient-1d fp x_1 step 
                       #:tolerance tol
                       #:count (sub1 count)))]))

(define (show-path t f xs)
  (define m (bigfloat->flonum (apply bfmax (map bfabs xs))))
  (pretty-print (cons t xs))
  (plot (list (function (λ (x) (bigfloat->flonum (f (bf x)))) #:label "f")
              (points (for/list ([x (in-list xs)])
                        (vector (bigfloat->flonum x) (bigfloat->flonum (f x))))))
        #:title t
        #:x-min (* -1 m)
        #:x-max (* +1 m)))

(module+ test
  (plot-new-window? #t)

  (when #f
    (define f (λ (x) (bf+ (bfexpt x (bf 2)) (bf* (bf 3) x))))
    (define fp (λ (x) (bf+ (bf* (bf 2) x) (bf 3))))
    (define fpp (λ (x) (bf 2)))
    (define x_0 (bf 50))
    (define nxs (cons x_0 (newton-1d fp fpp x_0)))
    (show-path "Newton's Method" f nxs)
    
    (define gxs (cons x_0 (gradient-1d fp x_0 (bf 0.01))))
    (show-path "Gradient" f gxs))
  
  (when #f
    (define k 40)
    (define f (λ (x) (bf+ (bfexpt x (bf 2)) (bf* (bf k) (bfsin x)))))
    (define fp (λ (x) (bf+ (bf* (bf 2) x) (bf* (bf k) (bfcos x)))))
    (define fpp (λ (x) (bf+ (bf 2) (bf* (bf (* -1 k)) (bfsin x)))))
    (define x_0 (bf 50))
    (define nxs (cons x_0 (newton-1d fp fpp x_0)))
    (show-path "Newton's Method" f nxs)
    
    (define gxs (cons x_0 (gradient-1d fp x_0 (bf 0.01))))
    (show-path "Gradient" f gxs))
  
  ;; y = mx + b
  ;; Error(m,b) = 1/N (sum i=1 (y(i) - (m * x_i + b))^2
  ;; dm = 2/n (sum -x_i * (y_i - (m * x_i + b)))
  ;; db = 2/n (sum -(y(i) - (m * x_i + b)))
  
  (when #f
    (define M (* 2 (random)))
    (define B (* 5 (random)))
    (define (noise x)
      (+ x (* 5 (- (random) 0.5))))
    (define (errf x)
      (noise (+ (* M x) B)))
    (define ps 
      (for/list ([i (in-range 100)])
        (define x (* 40 (random)))
        (vector x (errf x))))
    (plot (list (function (λ (x) (+ (* M x) B)))
                (points ps))))
  
  (when #t
    (define M  4.507450363924365)
    (define B 9.762366632191167)
    (define ps (file->value "15.rktd"))
    (define m 4.086403996388608)
    (define b 0.01912859164491673)
    (plot (list (function (λ (x) (+ (* M x) B))
                          #:color 0
                          #:label "generated")
                (function (λ (x) (+ (* m x) b))
                          #:color 1
                          #:label "estimate")
                (points ps
                        #:label "true"))))
  
  )
