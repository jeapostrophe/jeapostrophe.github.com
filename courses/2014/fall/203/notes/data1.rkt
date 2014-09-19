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
     ;; Good
     (diff l (diff r (remove u h)))
     ;; An error
     #;
     (define up (remove u h))

     ;; T = L u R + {h}
     ;; U - T = U - (L u R + {h})

     ;; Suppose that U = L u R

     ;; (u ((U - {h}) - L)
     ;;    ((U - {h}) - R))

     ;; (u ((L u R - {h}) - L)
     ;;    ((L u R - {h}) - R))

     ;; (u (R - {h})
     ;;    (L - {h}))

     ;; (R u L) - {h}

     ;; (L u R) - {h}

     ;; U - {h}

     ;; up
     #;
     (union (diff l up)
            (diff r up))
     ]))
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
  (define COUNT 1000)

  (define (set)
    (if (zero? (random 2))
      (empty)
      (add (set) (int))))
  (define (int)
    (random 10))

  (define-syntax-rule (forall ([var gen] ...) prop ...)
    (for ([i (in-range COUNT)])
      (define var (gen))
      ...
      (cond
        [prop
         (printf ".")]
        [else
         (printf "Property ~v fails when\n" 'prop)
         (printf "\t~a = ~v\n"
                 'var var)
         ...
         (exit 1)])
      ...))

  (define (<-> x y)
    (and (-> x y) (-> y x)))
  (define (-> x y)
    (or (not x) y))

  (forall
   ([t set] [u set] [v set] [x int] [y int])
   (<-> (member (add t x) y)
        (or (= x y) (member t y)))
   (-> (isEmptyHuh t)
       (not (member t x)))
   (-> (subset t u)
       (<= (cardinality t)
           (cardinality u)))
   (-> (equal t u)
       (= (cardinality t)
          (cardinality u)))
   ;; -> isn't exactly if
   #;
   (-> (not (isEmptyHuh t))
       (not (-> (= (cardinality t)
                   (cardinality u))
                (equal t u))))
   (-> (= (cardinality t)
          (cardinality u))
       (<-> (equal t u)
            (isEmptyHuh (diff t u))))

   (-> (and (equal u (union t v))
            (not (equal t v)))
       (equal u
              (union (diff t u) (diff v u))))

   (equal (union (inter t u) 
                 (union (diff t u) 
                        (diff u t)))
          (union t u))
   
   ;; (<-> (member (union t u) x)
   ;;      (or (member t x)
   ;;          (member u x)))
   ;; (or (= (cardinality (add t x))
   ;;        (cardinality t))
   ;;     (= (cardinality (add t x))
   ;;        (+ 1 (cardinality t))))
   ;; (isEmptyHuh (empty))
   ;; (not (isEmptyHuh (add t x)))
   ;; (<-> (isEmptyHuh t)
   ;;      (zero? (cardinality t)))
   ;; (-> (not (member t x))
   ;;     (equal (remove (add t x) x) t))
   ))
