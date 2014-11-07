#lang racket/base
(require racket/list
         racket/match)

;; Chapter 13
;; - The code in this chapter is so painful.
;; - Read this blog post and the papers it links to at the top:
;;   http://matt.might.net/articles/red-black-delete/
;; - In our reading, we'll focus on the theorems

;; 13.1
;; - Can we really store one more bit per node?
;; - REALLY understand this proof

;; 13.1-1
;; - This is to practice your understanding of the RB props

;; 13.1-3
;; - Where does blackness enter the props?

;; 13.1-4
;; - Write out each example
;; - What does this tell you about what RB trees are doing?

;; 13.1-5
;; - Think about the RB props and make some examples
;; - Think about the shortest path then modify it into the longest
;;   by considering when you can "add" nodes

;; 13.1-6
;; - Think about the last problem

;; 13.1-7
;; - Think about the last problem :)

;; 13.-2---
;; - You should read and skim this, but don't bother full grokking the
;; code. I find it very confusing and the guarantees of RB trees is
;; more important than the implementation
;; - Mainly, look at the pictures and the prose that describes them

(struct T () #:transparent)
(struct T:0 T () #:transparent)
(struct T:2 T (color left key val right) #:transparent)

(define (rb-insert T k v)
  (blacken (rb-insert/inner T k v)))

(define (rb-insert/inner T k v)
  (match T
    [(T:0)
     (T:2 'R T k v T)]
    [(T:2 C L K V R)
     (cond
      [(= k K)
       (T:2 C L K v R)]
      [(< k K)
       (balance C (rb-insert/inner L k v) K V R)]
      [else
       (balance C L K V (rb-insert/inner R k v))])]))

(define (blacken T)
  (match T
    [(T:2 _ L K V R)
     (T:2 'B L K V R)]))
(define (balance C L K V R)
  (match* (C L K V R)
    [('B (T:2 'R (T:2 'R a xK xV b) yK yV c) zK zV d)
     (T:2 'R (T:2 'B a xK xV b) yK yV (T:2 'B c zK zV d))]
    [('B (T:2 'R a xK xV (T:2 'R b yK yV c)) zK zV d)
     (T:2 'R (T:2 'B a xK xV b) yK yV (T:2 'B c zK zV d))]
    [('B a xK xV (T:2 'R (T:2 'R b yK yV c) zK zV d))
     (T:2 'R (T:2 'B a xK xV b) yK yV (T:2 'B c zK zV d))]
    [('B a xK xV (T:2 'R b yK yV (T:2 'R c zK zV d)))
     (T:2 'R (T:2 'B a xK xV b) yK yV (T:2 'B c zK zV d))]
    [(c a xK xV b)
     (T:2 c a xK xV b)]))

(module+ test
  (require slideshow
           pict/tree-layout)

  (define (tree->layout t)
    (match t
      [(T:0) 
       (tree-layout #:pict (colorize (filled-rectangle 15 15) "black"))]
      [(T:2 C L K V R)
       (tree-layout #:pict
                    (cc-superimpose
                     (colorize (disk 45)
                               (match C
                                 ['B "black"]
                                 ['R "red"]))
                     (colorize (text (format "~a" K))
                               "white"))
                    (tree->layout L)
                    (tree->layout R))]))
  (define (2-3tree->layout t)
    (match t
      [(T:0) 
       (tree-layout #:pict (colorize (filled-rectangle 15 15) "black"))]
      [(T:2 C (and L (or (T:0) (T:2 'B _ _ _ _))) K V (and R (or (T:0) (T:2 'B _ _ _ _))))
       (tree-layout #:pict
                    (cc-superimpose
                     (colorize (disk 45)
                               (match C
                                 ['B "black"]
                                 ['R "red"]))
                     (colorize (text (format "~a" K))
                               "white"))
                    (tree->layout L)
                    (tree->layout R))]
      [(T:2 C (and L (T:2 'R LL LK LV LR)) K V (and R (or (T:0) (T:2 'B _ _ _ _))))
       (tree-layout #:pict
                    (cc-superimpose
                     (colorize (disk 45)
                               (match C
                                 ['B "black"]
                                 ['R "red"]))
                     (colorize (text (format "~a ~a" LK K))
                               "white"))
                    (tree->layout LL)
                    (tree->layout LR)
                    (tree->layout R))]
      [(T:2 C (and L (or (T:0) (T:2 'B _ _ _ _))) K V (and R (T:2 'R RL RK RV RR)))
       (tree-layout #:pict
                    (cc-superimpose
                     (colorize (disk 45)
                               (match C
                                 ['B "black"]
                                 ['R "red"]))
                     (colorize (text (format "~a ~a" K RK))
                               "white"))
                    (tree->layout L)
                    (tree->layout RL)
                    (tree->layout RR))]
      [(T:2 C (and L (T:2 'R LL LK LV LR)) K V (and R (T:2 'R RL RK RV RR)))
       (tree-layout #:pict
                    (cc-superimpose
                     (colorize (disk 45)
                               (match C
                                 ['B "black"]
                                 ['R "red"]))
                     (colorize (text (format "~a ~a ~a" LK K RK))
                               "white"))
                    (tree->layout LL)
                    (tree->layout LR)
                    (tree->layout RL)
                    (tree->layout RR))]))
  (define (show-tree T)
    (scale-to-fit (naive-layered (tree->layout T)) client-w (* 0.4 client-h))))

(module+ test
  (define K 6)
  (define eL (for/list ([i (in-range (expt 2 K))]) i))
  (define L 
    (match 0
      [0 (shuffle eL)]
      [1 eL]
      [2 (reverse eL)]))
  (for/fold ([T (T:0)])
            ([i (in-list L)])
    (define Tp (rb-insert T i i))
    (slide
     (t (format "insert ~a" i))
     (show-tree T)
     (t "becomes")
     (show-tree Tp))
    Tp))
