#lang racket/base
(require racket/list
         racket/match)

;; Chapter 12

;; BSTs are the most important data structure discovered.

;; 12.1
;; - What's a sorting algorithm based on Inorder-Tree-Walk?

;; 12.1-2
;; - Look at the properties and think about what they mean
;; - Think about what this would mean for the heapsort algorithm

;; 12.1-4
;; - This should be very easy for you but don't just write the code
;; without thinking about it.

;; 12.2
;; - I find Min and Max to be cute.
;; - I really like the Successor and Predecessor algorithms

;; 12.2-4
;; - There's no magic here. Try to start with a very small tree and
;; experiment with the elements.
;; - You can also think about the information that is learned during a
;; search and show that it does not imply the conclusion.

;; 12.2-5
;; - The two sides are the same.
;; - Think about what successor and predecessor-ness means and where
;;they are in the tree

;; 12.3
;; - Some how this book can make super simple operations like
;; insertion look incredibly complicated. Here's a simpler version:

(struct T () #:transparent)
(struct T:0 T () #:transparent)
(struct T:2 T (left key val right) #:transparent)

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

(define (insert-n insert T Ks)
  (if (empty? Ks)
      T
      (insert-n insert
                (insert T (first Ks) (first Ks))
                (rest Ks))))

(module+ test
  (require slideshow
           pict/tree-layout)

  (define (tree->layout t)
    (match t
      [(T:0) #f]
      [(T:2 L K V R)
       (tree-layout #:pict (text (format "~a is ~a" K V))
                    (tree->layout L)
                    (tree->layout R))]))
  (define (render-tree T)
    (naive-layered (tree->layout T)))
  (define (show-tree #:label [label ""] T)
    (slide
     (t label)
     (scale-to-fit (render-tree T)
                   client-w
                   (* .8 client-h)))))

(module+ test
  (define T15
    (insert-n insert (T:0) (shuffle (for/list ([i (in-range 15)]) i))))

  (when #f
    (show-tree
     #:label "Counter example"
     (T:2 (T:0) 2 2 (T:2 (T:2 (T:0) 4 4 (T:0)) 5 5 (T:2 (T:0) 7 7 (T:0))))))

  (when #f
    (for/fold ([T (T:0)])
              ([i (in-range 25)])
      (define k (random 25))
      (define Tp (insert T k k))
      (show-tree
       #:label (format "Inserting ~a" k)
       Tp)
      Tp))

  (when #f
    (show-tree #:label "T15" T15)

    (show-tree #:label "Perfect order"
               (insert-n insert (T:0)
                         '(   4
                           2   6
                           1 3 5 7)))))

;; - Similarly, delete looks really complicated. Here's a simpler way
;; to do it. But what's the performance of this implementation? _Why_
;; is it obvious the performance would be worse?

(define (slow-delete T k)
  (match T
    [(T:0)
     T]
    [(T:2 L K V R)
     (cond
      [(= k K)
       (union L R)]
      [(< k K)
       (T:2 (slow-delete L k) K V R)]
      [else
       (T:2 L K V (slow-delete R k))])]))

(define (union X Y)
  (match X
    [(T:0)
     Y]
    [(T:2 L K V R)
     (insert (union (union L R) Y) K V)]))

(module+ test
  (when #f (show-tree (slow-delete T15 4))))

(define (fast-delete T k)
  (match T
    [(T:0)
     T]
    [(T:2 L K V R)
     (cond
      [(= k K)
       (fast-delete-this T)]
      [(< k K)
       (T:2 (fast-delete L k) K V R)]
      [else
       (T:2 L K V (fast-delete R k))])]))

;; Based on Fig 12.4
(define (fast-delete-this T)
  (match T
    ;; a
    [(T:2 (T:0) zk zv R)
     R]
    ;; b
    [(T:2 L zk zv (T:0))
     L]
    ;; c
    [(T:2 L zk zv (T:2 (T:0) yk yv X))
     (T:2 L yk yv X)]
    ;; d
    [(T:2 L zk zv (T:2 (T:2 (T:0) yk yv X) rk rv RR))
     (T:2 L yk yv (T:2 X rk rv RR))]))

(module+ test
  (when #f
    (show-tree (fast-delete T15 4))
    (show-tree (fast-delete T15 7))))

;; 12.3-2
;; - Do it a few times

;; 12.3-4
;; - I love this problem

(module+ test
  (let/ec break
    (for ([n (in-naturals 2)])
      (define l (build-list n add1))
      (for ([p-of-l (in-list (permutations l))])
        (define tr (insert-n insert (T:0) p-of-l))
        (for ([x (in-list p-of-l)])
          (for ([y (in-list p-of-l)]
                #:unless (= x y))
            (define t-x (fast-delete tr x))
            (define t-x-y (fast-delete t-x y))
            (define t-y (fast-delete tr y))
            (define t-y-x (fast-delete t-y x))
            (unless (equal? t-x-y t-y-x)
              (define (recty p)
                (cc-superimpose
                 (colorize
                  (filled-rectangle
                   (pict-width p)
                   (pict-height p))
                  "salmon")
                 p))
              (slide
               (t (format "Deleting ~a and ~a"
                          x y))
               (scale-to-fit
                (naive-layered
                 (tree-layout
                  #:pict (recty (render-tree tr))
                  (tree-layout
                   #:pict (recty (render-tree t-x))
                   #f
                   (tree-layout #:pict (recty (render-tree t-x-y))))
                  (tree-layout
                   #:pict (recty (render-tree t-y))
                   (tree-layout #:pict (recty (render-tree t-y-x)))
                   #f)))
                client-w client-h))
              (break))))))))
