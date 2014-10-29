#lang racket/base
(require racket/match
         racket/list)

(module+ test
  (define (random-char)
    (integer->char (+ (char->integer #\a) (random 26))))
  (define (random-string len)
    (if (zero? len)
        ""
        (string-append (random-string (- len 1))
                       (string (random-char)))))
  (char->integer #\a)
  ;; Wassail
  (random-string 10))

(struct T ())
(struct T:0 T ())
(struct T:1 T (next))
(struct T:2 T (left key val right))

(define (insert T k v)
  (match T
    [(T:0)
     (T:2 T k v T)]
    [(T:2 L K V R)
     (cond
      [(= k K)
       (T:2 L K v R)]
      [(< k K)
       (T:2 (insert L k v) K V R)]
      [else
       (T:2 L K V (insert R k v))])]))

;;       AVL .... 1-2 Tree
;; Red-Black .... 2-3 Tree

(define (smart-insert T k v)
  (struct fake:L (K V))
  (struct fake:3 (left-tree K1 V1 middle-tree K2 V2 right-tree))
  ;; step1 is "find the place to insert"
  (define (step1 real-tree)
    (match real-tree
      [(T:0)
       (fake:L k v)]
      [(T:1 N)
       (fake:1 (step1 N))]
      [(T:2 L K V R)
       (cond
        [(= k K)
         (fake:2 L K v R)]
        [(< k K)
         (fake:2 (step1 L) K V R)]
        [else
         (fake:2 L K V (step1 R))])]))
  ;; step2 is "rebuild the tree around it"
  (define (step2 fake-tree)
    (match fake-tree
      [(fake:L k v)
       (T:2 (T:0) k v (T:0))]
      [(fake:3 L K1 V1 M K2 V2 R)
       (T:2 (T:2 L K1 V1 M) K2 V2 (T:1 R))]
      [t
       t]))
  ;; fake:1
  (define (fake:1 fake-tree)
    (match fake-tree
      [(fake:L k v)
       (T:2 (T:0) k v (T:0))]
      [(fake:3 L K1 V1 M K2 V2 R)
       (T:2 (T:2 L K1 V1 M) K2 V2 (T:1 R))]
      [t
       (T:1 t)]))
  (define (fake:2 L K V R)
    (match* (L K V R)
      [((fake:L k1 v1) k2 v2 t1)
       (fake:3 (T:0) k1 v1 (T:0) k2 v2 t1)]
      [((fake:3 t1 k1 v1 t2 k2 v2 t3) k3 v3 (T:1 t4))
       (T:2 (T:2 t1 k1 v1 t2) k2 v2 (T:2 t3 k3 v3 t4))]
      [((fake:3 t1 k1 v1 t2 k2 v2 t3) k3 v3 (? T:2? t4))
       (fake:3 (T:2 t1 k1 v1 t2) k2 v2 (T:1 t3) k3 v3 t4)]

      [(t1 k1 v1 (fake:L k2 v2))
       (fake:3 t1 k1 v1 (T:0) k2 v2 (T:0))]
      [((T:1 t1) k1 v1 (fake:3 t2 k2 v2 t3 k3 v3 t4))
       (T:2 (T:2 t1 k1 v1 t2) k2 v2 (T:2 t3 k3 v3 t4))]
      [((? T:2? t1) k1 v1 (fake:3 t2 k2 v2 t3 k3 v3 t4))
       (fake:3 t1 k1 v1 (T:1 t2) k2 v2 (T:2 t3 k3 v3 t4))]

      [(t1 k1 v1 t2)
       (T:2 t1 k1 v1 t2)]))
  (define after-step1 (step1 T))
  (step2 after-step1))

(maybe-rotate (insert t k v))

(define (insert-n insert T Ks)
  (if (empty? Ks)
      T
      (insert-n insert
                (insert T (first Ks) (first Ks))
                (rest Ks))))

(module+ main
  (require slideshow
           pict/tree-layout)

  (define (tree->layout t)
    (match t
      [(T:0) #f]
      [(T:1 N)
       (tree-layout (tree->layout N))]
      [(T:2 L K V R)
       (tree-layout #:pict (text (format "~a is ~a" K V))
                    (tree->layout L)
                    (tree->layout R))]))

  (define (show-tree t)
    (scale-to-fit
     (naive-layered
      (tree->layout t))
     client-w
     (* .2 client-h)))
  
  (define (smart->avl t)
    (match t
      [(T:0) t]
      [(T:1 N)
       (smart->avl N)]
      [(T:2 L K V R)
       (T:2 (smart->avl L) K V (smart->avl R))]))

  (for ([n (in-range 1 25)])
    (define the-list (build-list n add1))
    (define the-real-list
      (match 3
        [3 (reverse the-list)]
        [2 (list 8 4 12 2 6 10 14 1 3 5 7 9 11 13 15)]
        [1 (shuffle the-list)]
        [0 the-list]))
    (define smart-tree
      (insert-n smart-insert (T:0) the-real-list))
    (slide
     (t "Simple")
     (show-tree (insert-n insert (T:0) the-real-list))
     (t "1-2 Brother Tree")
     (show-tree smart-tree)
     (t "AVL")
     (show-tree (smart->avl smart-tree)))))
