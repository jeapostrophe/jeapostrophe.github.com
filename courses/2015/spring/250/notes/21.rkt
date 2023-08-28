#lang racket/base
(require racket/vector
         racket/math)

(define i (sqrt -1))

(define (ditfft2 x off N s)
  (define N/2 (quotient N 2))
  (cond
    [(= N 1)
     (vector (vector-ref x off))]
    [else
     (define X
       (vector-append
        (ditfft2 x off N/2 (* 2 s))
        (ditfft2 x (+ off s) N/2 (* 2 s))))
     (for ([k (in-range N/2)])
       (define t (vector-ref X k))
       (define v (* (exp (* -2 pi i k (/ 1 N))) (vector-ref X (+ k N/2))))
       (vector-set! X k (+ t v))
       (vector-set! X (+ k N/2) (- t v)))
     X]))

(define (fft x)
  (ditfft2 x 0 (vector-length x) 1))

(define (sample-f N f)
  (build-vector N f))

(module+ test
  (require plot)
  (plot-new-window? #t)

  (define f
    (λ (t)
      (+ (* 0.75 (cos (* 2 pi t 0.25 1)))
         (* 0.25 (cos (* 2 pi t 0.10 1))))))

  ;; 64, should be more like 8192
  (define N 8192 #;(expt 2 6))

  (plot (list (function f))
        #:x-min 0
        #:x-max N
        #:title "Original Signal"))


(module+ test
  (define samps (sample-f N f))
  (plot (list (points (for/list ([s (in-vector samps)]
                                 [k (in-naturals)])
                        (vector k s))
                      #:color 1))
        #:x-min 0
        #:x-max N
        #:title "Sampled Signal"))

(module+ test
  (define fft-r (fft samps))

  (plot (list (points (for/list ([f (in-vector fft-r)]
                                 [k (in-naturals)])
                        (vector (/ k N)
                                (/ (sqrt (+ (sqr (real-part f))
                                            (sqr (imag-part f))))
                                   N)))
                      #:color 1))
        #:x-min 0
        #:x-max 1
        #:title "Frequency Amplitudes"))

;; Talk about period detection vs frequency analysis
