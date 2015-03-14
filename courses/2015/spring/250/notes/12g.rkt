#lang racket/base
(require plot)

(module+ main
  (plot-new-window? #t)
  (plot3d
   (surface3d
    (λ (x_1 x_2)
      (if (and (<= (+ (* 1 x_1) (* 2 x_2)) 4)
               (<= (+ (* 1 x_1) (* -1 x_2)) 1))
          (+ (* 3 x_1)
             (* 2 x_2))
          0)))
   #:x-min 0
   #:x-max 4
   #:y-min 0
   #:y-max 4
   #:z-min 0))
