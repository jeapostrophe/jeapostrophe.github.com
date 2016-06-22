#lang lazy

(define (f x)
  (printf "f is running on ~a\n" x)
  (* x x))
(define (g x y)
  y)
(module+ test
  (g (f 4) (f 5)))


;; make-finlist : Num x (Num -> Val) -> List Val
(define (make-finlist n f)
  (if (zero? n)
      empty
      (cons (f n)
            (make-finlist (sub1 n) f))))
(module+ test
  (make-finlist 10 number->string))

(define (make-finlist-up n f)
  (make-finlist-up-helper 0 n f))
(define (make-finlist-up-helper i n f)
  (if (= i n)
      empty
      (cons (f i)
            (make-finlist-up-helper
             (add1 i) n f))))
(module+ test
  (make-finlist-up 10 number->string))

(define (make-inflist f)
  (make-inflist-helper 0 f))
(define (make-inflist-helper i f)
  (cons (f i)
        (make-inflist-helper
         (add1 i) f)))
(module+ test
  (make-inflist number->string))

;;; Infinite Lists
(define nats (make-inflist (λ (i) i)))
(define evens (map (λ (i) (* 2 i)) nats))
(define odds (map add1 evens))
(module+ test
  (list-ref odds 17))

;;       Fibs = 1 1 2 3  5  8 13
;; Rest(Fibs) = 1 2 3 5  8 13
;;          + = 2 3 5 8 13
;; R(R(Fibs)) =
(define fibs
  (cons 1
        (cons 1
              (map + fibs (rest fibs)))))
(module+ test
  (list-ref fibs 170))

;; Primes
(define (take-while ? l)
  (if (? (first l))
      (cons (first l)
            (take-while ? (rest l)))
      empty))
(define (divides n p)
  (zero? (modulo n p)))
(define (prime? n)
  (andmap (λ (p) (not (divides n p)))
          (take-while (λ (p) (<= (* p p) n))
                      primes)))
(define primes
  (cons 2 (filter prime? (rest odds))))
(module+ test
  (list-ref primes 17)
  (list-ref primes 170)
  (list-ref primes 1700))

;; ....

(define (build-table n m f)
  (build-list
   n
   (lambda (i) 
     (build-list
      m
      (lambda (j) (f i j))))))

(define (lookup-table tb i j)
  (cond
    [(= -1 i) 0]
    [(= -1 j) 0]
    [else
     (list-ref (list-ref tb i) j)]))

(define (lcs-length s1 s2)
  (define ls1 (string-length s1))
  (define ls2 (string-length s2))
  ;; Table : Num x Num -> Num
  (define tb
    (build-table
     ls1 ls2
     (λ (i j)
       (if (char=? (string-ref s1 i)
                   (string-ref s2 j))
           (+ 1 (lookup-table tb (sub1 i) (sub1 j)))
           (max (lookup-table tb (sub1 i) j)
                (lookup-table tb i (sub1 j)))))))
  (lookup-table tb (sub1 ls1) (sub1 ls2)))

(module+ test
  (lcs-length "artist" "artsy"))

(define (lookup-table^ tb i j)
  (cond
    [(= -1 i) '()]
    [(= -1 j) '()]
    [else
     (list-ref (list-ref tb i) j)]))

(define (argmax f x y)
  (if (< (f x) (f y))
      y
      x))

(define (lcs s1 s2)
  (define ls1 (string-length s1))
  (define ls2 (string-length s2))
  ;; Table : Num x Num -> List(Chars)
  (define tb
    (build-table
     ls1 ls2
     (λ (i j)
       (printf "Evaluated on (~a,~a)\n" i j)
       (if (char=? (string-ref s1 i)
                   (string-ref s2 j))
           (cons (string-ref s1 i) (lookup-table^ tb (sub1 i) (sub1 j)))
           (argmax
            length
            (lookup-table^ tb (sub1 i) j)
            (lookup-table^ tb i (sub1 j)))))))
  (list->string
   (!!list
    (reverse
     (lookup-table^ tb (sub1 ls1) (sub1 ls2))))))

(module+ test
  (lcs "artist" "artsy"))
