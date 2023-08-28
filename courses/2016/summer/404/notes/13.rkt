#lang racket/base
(require racket/list
         racket/match)

(struct leaf ())
(struct branch (left val right))

(define mt (leaf))

(define (height bt)
  (match bt
    [(leaf) 0]
    [(branch l v r)
     (+ 1 (max (height l) (height r)))]))

(define (insert bt v)
  (match bt
    [(leaf) (branch bt v bt)]
    [(branch l tv r)
     (if (<= v tv)
         (branch (insert l v) tv r)
         (branch l tv (insert r v)))]))

(define (list->bt l)
  (for/fold ([bt mt])
            ([v (in-list l)])
    (insert bt v)))

(define (braun-insert bt v)
  (match bt
    [(leaf) (branch bt v bt)]
    [(branch l tv r)
     (branch r v (braun-insert l tv))]))

(define (list->bbt l)
  (for/fold ([bt mt])
            ([v (in-list l)])
    (braun-insert bt v)))

(require pict
         pict/tree-layout
         slideshow)
(define (circled p)
  (cc-superimpose
   p
   (circle (max (pict-width p)
                (pict-height p)))))
(define (bt->layout bt)
  (match bt
    [(leaf) #f]
    [(branch l v r)
     (tree-layout
      #:pict
      (circled
       (text (number->string v)))
      (bt->layout l)
      (bt->layout r))]))
(define (bt->pict bt)
  (binary-tidier (bt->layout bt)))
(define (displaybt bt)
  (slide
   (scale-to-fit
    (bt->pict bt)
    client-w
    client-h)))

(define (lg n)
  (/ (log n) (log 2)))

(module+ test
  (for ([n (in-range 100)])
    (define elems
      (for/list ([i (in-range n)]) i))
    (define bt
      (list->bbt (shuffle elems)))
    (displaybt bt)))

(module+ test-binary
  (for ([n (in-range 100)])
    (define elems
      (for/list ([i (in-range n)]) i))
    (define bt
      (list->bt (shuffle elems)))
    (displaybt bt)))

(module+ test-perms
  (define (average-height-of-tree-with-n-elements n)
    (define elems
      (for/list ([i (in-range n)]) i))

    (define-values (total count)
      (for/fold ([total 0]
                 [count 0])
                ([p (in-permutations elems)])
        (define bt (list->bt p))
        (values (+ total (height bt))
                (+ 1 count))))

    (printf "The average height for ~a elements is ~a, expected is ~a\n"
            n
            (real->decimal-string (/ total count))
            (real->decimal-string (lg n))))

  (for ([i (in-range 1 10)])
    (average-height-of-tree-with-n-elements i)))
