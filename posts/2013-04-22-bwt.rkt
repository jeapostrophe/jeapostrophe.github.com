#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/unit
                     racket/match
                     racket/list
                     data/heap)
          "../post.rkt")

@title{Naive Burrows-Wheeler Transform}
@categories["Racket" "Algorithms" "Compression"]

XXX

@(the-jump)

XXX http://www.hpl.hp.com/techreports/Compaq-DEC/SRC-RR-124.html

XXX

If you'd like to use this code at home, you should put it in this
order:

@chunk[<*>
       (require rackunit
                racket/format
                racket/function
                racket/match
                racket/list)

       (let ()
         (define (string-last s)
           (string-ref s (sub1 (string-length s))))

         (define (rotation-start-at s i)
           (string-append (substring s i)
                          (substring s 0 i)))

         (define (rotations s)
           (for/list ([i (in-range (string-length s))])
             (rotation-start-at s i)))

         (define (encode s)
           (define rotation-table
             (sort (rotations s)
                   string<?))
           (list->string
            (map string-last
                 rotation-table)))

         (define (string-cons c s)
           (string-append (string c) s))

         (define (decode s)
           (define len (string-length s))
           (define sl (string->list s))
           (define rotation-table
             (for/fold ([t (make-list len "")])
                 ([i (in-range len)])
               (sort (map string-cons sl t) string<?)))
           (findf (λ (s) (char=? #\~ (string-last s)))
                  rotation-table))

         (define-syntax-rule (round-trip in out)
           (begin
             (check-equal? (decode (encode in)) in)
             (check-equal? (encode in) out)
             (check-equal? (decode out) in)))

         (round-trip
          "^BANANA~"
          "BNN^AA~A")

         (round-trip
          "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES~"
          "TEXYDST.E.IXIXIXXSSMPPS.B..E.~.UESFXDIIOIIITS"))

       (let ()
         (struct dll (prev data next) #:prefab)

         (define (list->dll l)
           (define top-prev
             (make-placeholder #f))
           (define top-next
             (make-placeholder #f))
           (define top 
             (dll top-prev #f top-next))
           (define the-first
             (let loop ([last top] [l l])
               (cond
                 [(empty? l)
                  (placeholder-set! top-prev last)
                  top]
                 [else
                  (define this-next (make-placeholder #f))
                  (define this (dll last (first l) this-next))
                  (define next (loop this (rest l)))
                  (placeholder-set! this-next next)
                  this])))
           (placeholder-set! top-next the-first)
           (dll-next (make-reader-graph top)))

         (define (dll-suffixes n dll)
           (cond
             [(zero? n)
              empty]
             [else
              (cons dll (dll-suffixes (sub1 n) (dll-next dll)))]))

         (define (safe-< x y)
           (match* (x y)
             [(#f _) #t]
             [(_ #f) #f]
             [(x y) (< x y)]))

         (define (dll<? n x y)
           (cond
             [(zero? n)
              #f]
             ;; Compare
             [(equal? (dll-data x) (dll-data y))
              (dll<? (sub1 n) (dll-next x) (dll-next y))]
             [else
              (safe-<  (dll-data x) (dll-data y))]))

         (define (dll->list n dll)
           (cond
             [(zero? n)
              empty]
             [else
              (cons (dll-data dll)
                    (dll->list (sub1 n) (dll-next dll)))]))

         (define (dll-last dll)
           (dll-data (dll-prev dll)))         

         ;; O(n^2)
         (define (encode l)
           (define n (add1 (length l)))
           ;; O(n)
           (define dll (list->dll l))
           ;; O(n)
           (define rotations (dll-suffixes n dll))
           ;; O(n^2)
           (define sorted-rotations
             (sort rotations (curry dll<? n)))
           ;; O(n)
           (map dll-last
                sorted-rotations))

         (define (list<? x y)
           (cond
             [(or (empty? x) (empty? y)) #f]
             [(equal? (first x) (first y))
              (list<? (rest x) (rest y))]
             [else
              (safe-< (first x) (first y))]))

         ;; O(n^3)
         (define (decode l)
           (define n (length l))
           ;; O(n^3)
           (define rotation-table
             (for/fold ([t 
                         ;; O(n)
                         (make-list n empty)])
                 ([i (in-range n)])
               ;; O(n^2)
               (sort 
                ;; O(n)
                (map cons l t)
                list<?)))           
           ;; O(n)
           (filter (λ (x) x)
                   (findf (λ (s) (not (last s)))
                          rotation-table)))

         (define N 100)
         (for ([i (in-range N)])
           (define in (build-list i (λ (i) (random 10))))
           (define out (encode in))
           (define re-in (decode out))
           ;; (printf "~a -> ~a -> ~a\n" in out re-in)
           (check-equal? re-in in)))

       ]

@(the-end)
