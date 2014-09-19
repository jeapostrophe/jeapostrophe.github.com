#lang racket/base
(require racket/bool
         racket/match)

(struct leaf () #:transparent)
(struct branch (left here right) #:transparent)

(define (empty)
  (leaf))
(define (cardinality t)
  (match t
    [(leaf)
     0]
    [(branch l h r)
     (+ 1 (cardinality l) (cardinality r))]))
(define (isEmptyHuh t)
  (match t
    [(leaf)
     true]
    [(branch l h r)
     false]))
(define (member t elt)
  (match t
    [(leaf)
     false]
    [(branch l h r)
     (cond
       [(= elt h)
        true]
       [(< elt h)
        (member l elt)]
       [else
        (member r elt)])]))
(define (add t elt)
  (match t
    [(leaf)
     (branch t elt t)]
    [(branch l h r)
     (cond
       [(= elt h)
        t]
       [(< elt h)
        (branch (add l elt) h r)]
       [else
        (branch l h (add r elt))])]))
(define (remove t elt)
  (match t
    [(leaf)
     t]
    [(branch l h r)
     (cond
       [(= elt h)
        (union l r)]
       [(< elt h)
        (branch (remove l elt) h r)]
       [else
        (branch l h (remove r elt))])]))
(define (union t u)
  (match t
    [(leaf)
     u]
    [(branch l h r)
     (add (union u (union l r)) h)]))
(define (inter t u)
  (match t
    [(leaf)
     t]
    [(branch l h r)
     (if (member u h)
       (branch (inter l u) h (inter r u))
       (inter (union l r) u))]))
(define (diff t u)
  (match t
    [(leaf)
     u]
    [(branch l h r)
     (diff l (diff r (remove u h)))]))
(define (equal t u)
  (and (subset t u)
       (subset u t)))
(define (subset t u)
  (match t
    [(leaf)
     true]
    [(branch l h r)
     (and (member u h)
          (subset l u)
          (subset r u))]))

(module+ test
  (define COUNT 100)

  (define (set)
    (if (zero? (random 2))
      (empty)
      (add (set) (int))))
  (define (int)
    (random COUNT))

  (define-syntax-rule (forall ([var gen] ...) prop ...)
    (for ([i (in-range COUNT)])
      (define var (gen))
      ...
      (unless prop
        (printf "Property ~v fails when\n" 'prop)
        (printf "\t~a = ~v\n"
                'var var)
        ...)
      ...))

  (define (<-> x y)
    (and (-> x y) (-> y x)))
  (define (-> x y)
    (or (not x) y))

  (forall
   ([t set] [u set] [x int] [y int])
   (<-> (member (add t x) y)
        (or (= x y) (member t y)))
   (<-> (member (union t u) x)
        (or (member t x)
            (member u x)))
   (or (= (cardinality (add t x))
          (cardinality t))
       (= (cardinality (add t x))
          (+ 1 (cardinality t))))
   (isEmptyHuh (empty))
   (not (isEmptyHuh (add t x)))
   (<-> (isEmptyHuh t)
        (zero? (cardinality t)))
   (-> (not (member t x))
       (equal (remove (add t x) x) t))))

