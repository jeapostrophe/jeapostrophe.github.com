#lang racket/base
(require plot
         plot/utils
         math/bigfloat
         racket/format
         racket/list)

(module+ main
  (plot-new-window? #t)

  (define (plot-integral f actual-int a b N)
    (define actual
      (if actual-int
          (bf- (actual-int b) (actual-int a))
          #f))

    (define h (bf/ (bf- b a) (bf N)))

    (define samples
      (for/list ([i (in-range (add1 N))])
        (define x (bf+ a (bf* (bf i) h)))
        (list x (f x))))

    (define (all-but-last l)
      (reverse (rest (reverse l))))
    (define numeric
      (bf* h (bf+ (bf* (bf/ 1.bf 2.bf) (bf+ (second (first samples)) (second (last samples))))
                  (foldr bf+ 0.bf (map second (all-but-last (rest samples)))))))

    (define ((normal-lerp start end) x)
      (bf+ (bf* (bf- 1.bf x) start)
           (bf* x end)))
    (define ((lerp start end) x)
      ((normal-lerp (second start) (second end))
       (bf/ (bf- (bf x) (first start))
            (bf- (first end)
                 (first start)))))

    (define an-error
      (if actual
          (bfabs (bf- actual numeric))
          #f))

    (define (bf-> x)
      (bigfloat->real x))

    (printf "f is ~a\n" f)
    (printf "numeric integral = ~a\n"
            (bigfloat->string numeric))
    (when actual
      (printf "actual integral = ~a\n"
              (bigfloat->string actual))
      (printf "error = ~a\n"
              (bigfloat->string an-error)))
    (printf "\n")

    (plot (list*
           (function (compose bf-> f))
           (points (map (λ (l) (map bf-> l)) samples))
           (for/list ([left (in-list samples)]
                      [right (in-list (rest samples))]
                      [which (in-naturals)])
             (function-interval (λ (x) 0) (compose bf-> (lerp left right))
                                (bf-> (first left))
                                (bf-> (first right))
                                #:color (if (even? which) 1 2)
                                #:line1-style 'transparent)))
          #:x-min (bf-> a)
          #:x-max (bf-> b)))

  (bf-precision (round (* 1/2 53)))
  
  (plot-integral (λ (x) (bf* (bf 2) x))
                 (λ (x) (bfexpt x (bf 2)))
                 (bf 0) pi.bf
                 10)
  
  (plot-integral (λ (x) (bfsin x))
                 (λ (x) (bf* -1.bf (bfcos x)))
                 (bf/ pi.bf (bf 3)) (bf/ (bf* (bf 3) pi.bf) (bf 4))
                 10)
  (plot-integral (λ (x) (bf* (bflog x) (bfsin (bfexp (bf* -1.bf x)))))
                 #f
                 (bfexpt (bf 10) (bf -4)) (bf 4)
                 10))
