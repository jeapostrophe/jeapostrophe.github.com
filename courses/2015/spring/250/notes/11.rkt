#lang racket/base
(require racket/match
         plot
         math/bigfloat)
(module+ main
  (plot-new-window? #t)
  ;; 128bit floats
  (bf-precision (* 2 53))
  ;; 64bit floats
  (bf-precision 53)
  )

;; Numeric differentiation
(module+ main-numdif
  #;#:
  (define f
    (λ (x)
      (bfsin x)))
  (define df
    (λ (x)
      (bfcos x)))
  
  (define f
    (λ (x)
      (bf+ (bf* (bf +6) (bfexpt x (bf 2)))
           (bf* (bf +4) (bfexpt x (bf 1)))
           (bf* (bf -8) (bfexpt x (bf 0))))))
  (define df
    (λ (x)
      (bf+ (bf* (bf +12) (bfexpt x (bf 1)))
           (bf* (bf  +4) (bfexpt x (bf 0))))))
  (define x_0
    (bf +25))

  ;; with 128-bit floats

  ;; 1 zooms
  ;; with h = 0.1
  ;; Actual df(x_0): 304
  ;; df-num-fwd(x_0): 304.599999999999999999999999999677
  ;; df-num-bwk(x_0): 303.399999999999999999999999999717

  ;; 9 zooms
  ;; with h = 0.0000000001
  ;; Actual df(x_0): 304
  ;; df-num-fwd(x_0): 304.000000000599999999167978877609
  ;; df-num-bwk(x_0): 303.999999999399999999384028380324

  ;; with 64-bit floats
  ;; h = 0.1
  ;; 1 zoom for fwd/bwk
  ;; 14 zooms for cen
  ;; Actual df(x_0): 304
  ;; df-num-fwd(x_0): 304.60000000000491
  ;; df-num-bwk(x_0): 303.400000000006
  ;; df-num-cen(x_0): 304.00000000000546

  (define h
    (bfexpt (bf 10) (bf -1)))
  (define (df-num-fwd x_0)
    (define (x ith)
      (bf+ x_0 (bf* ith h)))
    (bf/ (bf+ (bf* (bf -1) (f (x (bf 0))))
              (bf* (bf +1) (f (x (bf 1)))))
         h))
  (define (df-num-bwk x_0)
    (define (x ith)
      (bf+ x_0 (bf* ith h)))
    (bf/ (bf+ (bf* (bf -1) (f (x (bf -1))))
              (bf* (bf +1) (f (x (bf +0)))))
         h))
  (define (df-num-cen x_0)
    (define (x ith)
      (bf+ x_0 (bf* ith h)))
    (bf/ (bf+ (bf* (bf/ (bf -1)
                        (bf +2))
                   (f (x (bf -1))))
              (bf* (bf/ (bf +1)
                        (bf +2))
                   (f (x (bf +1)))))
         h))

  (printf "Actual df(x_0): ~a\n"
          (bigfloat->string (df x_0)))
  (printf "df-num-fwd(x_0): ~a\n"
          (bigfloat->string (df-num-fwd x_0)))
  (printf "df-num-bwk(x_0): ~a\n"
          (bigfloat->string (df-num-bwk x_0)))
  (printf "df-num-cen(x_0): ~a\n"
          (bigfloat->string (df-num-cen x_0)))

  (plot (list (function
               (compose1 bigfloat->real df bf)
               #:color 0
               #:label "actual")
              (function
               (compose1 bigfloat->real df-num-fwd bf)
               #:color 1
               #:label "forward")
              (function
               (compose1 bigfloat->real df-num-bwk bf)
               #:color 2
               #:label "backward")
              (function
               (compose1 bigfloat->real df-num-cen bf)
               #:color 3
               #:label "central"))
        #:x-min -50.0
        #:x-max +50.0)

  (when #f
    (plot (list (function
                 f
                 #:color 0
                 #:label "6x^2 + 4x - 8")
                (point-label
                 (vector x_0 (f x_0)))
                (function
                 (λ (x)
                   (- (* (df x) x)
                      (f x_0)))
                 #:color 1
                 #:label "df"
                 (- x_0 5)
                 (+ x_0 5)))
          #:x-min -50
          #:x-max +50)))

;; Automatic differentiation
(module+ main-auto
  (struct ad-rep (fx fpx))
  (define (ad fx)
    (ad-rep fx (bf 0)))
  (define (ad-input fx)
    (ad-rep fx (bf 1)))
  (define ad->fpx ad-rep-fpx)

  (define (ad+ l r)
    (match-define (ad-rep l-fx l-fpx) l)
    (match-define (ad-rep r-fx r-fpx) r)
    (ad-rep (bf+ l-fx r-fx)
            (bf+ l-fpx r-fpx)))
  (define (ad* l r)
    (match-define (ad-rep l-fx l-fpx) l)
    (match-define (ad-rep r-fx r-fpx) r)
    (ad-rep (bf* l-fx r-fx)
            (bf+ (bf* l-fx r-fpx)
                 (bf* l-fpx r-fx))))
  (define (adexpt u n)
    (match-define (ad-rep u-fx u-fpx) u)
    (ad-rep (bfexpt u-fx n)
            (bf* n
                 (bfexpt u-fx (bf- n (bf 1)))
                 u-fpx)))

  (define f
    (λ (x)
      (ad+
       (ad* (ad (bf +6)) (adexpt x (bf 2)))
       (ad+ (ad* (ad (bf +4)) 
                 (adexpt x (bf 1)))
            (ad* (ad (bf -8))
                 (adexpt x (bf 0)))))))
  (define df
    (λ (x)
      (bf+ (bf* (bf +12) (bfexpt x (bf 1)))
           (bf* (bf  +4) (bfexpt x (bf 0))))))

  (define x_0
    (bf +25))
  (printf "Actual df(x_0): ~a\n"
          (bigfloat->string (df x_0)))
  (printf "automatic(x_0): ~a\n"
          (bigfloat->string (ad->fpx (f (ad-input x_0)))))

  (plot (list (function
               (compose1 bigfloat->real df bf)
               #:color 0
               #:label "actual")
              (function
               (compose1 bigfloat->real ad->fpx f ad-input bf)
               #:color 1
               #:label "automatic"))
        #:x-min -50.0
        #:x-max +50.0)

  )
