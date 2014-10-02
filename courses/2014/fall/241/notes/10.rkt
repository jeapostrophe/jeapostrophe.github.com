#lang racket
(require data/heap)

(module+ test
  (define h (make-heap <=))
  (heap-add! h 13)
  (heap-add-all! h (list 10 9 74 3 1 4))
  (heap-add! h 130)
  (for ([e (in-heap h)])
    (displayln e))
  #;(error))

(struct paths (desc cost))
(define (paths<= x y)
  (<= (paths-cost x) (paths-cost y)))

(module+ main
  (define hp (make-heap paths<=))
  (heap-add-all! hp
                 (list (paths "Down the road"
                              10)
                       (paths "Spin around twice"
                              1)
                       (paths "Up the elevator"
                              7)))
  (for ([e (in-heap hp)])
    (displayln (paths-desc e))))
