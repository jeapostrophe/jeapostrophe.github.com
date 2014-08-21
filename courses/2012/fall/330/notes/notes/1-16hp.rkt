#lang lazy

(define print-only-errors #t)
(define (test l r)
  (if (equal? l r)
    (if print-only-errors
      (void)
      (printf "Test Passed~n"))
    (printf "Test Failed.~nActual:   ~S ~nExpected: ~S~n" l r)))

(define (take-while p l)
  (cond
    [(empty? l)
     empty]
    [(p (first l))
     (cons (first l)
           (take-while p (rest l)))]
    [else
     empty]))

(define nats
  (cons 0 (map add1 nats)))

(define (build-infinite-list f)
  (map f nats))

(define ((divides n) p)
  (zero? (modulo n p)))

(define (prime?-like n possible-factors)
  (cond
    [(< n 2)
     #f]
    [(= n 2)
     #t]
    [else
     (not 
      (ormap (divides n)
             (take-while (λ (p)
                           (<= p (integer-sqrt n)))
                         possible-factors)))]))

(define (prime? n)
  (prime?-like n (rest (rest nats))))

(define primes
  (filter prime? nats))

(!!list (take 10 primes))

(define (prime?/fast n)
  (prime?-like n primes/fast))

(define primes/fast
  (filter prime?/fast nats))

(!!list (take 10 primes/fast))

(define (build-vector num f)
  (apply vector (build-list num f)))

(define (build-table r c f)
  (build-vector
   r
   (λ (r)
     (build-vector
      c
      (λ (c)
        (f r c))))))

(define (lcs-length s1 s2)
  (define s1l
    (string-length s1))
  (define s2l
    (string-length s2))
  (define (table-ref i j)
    (if (and (<= 0 i (sub1 s1l))
             (<= 0 j (sub1 s2l)))
      (vector-ref (vector-ref ze-table i) j)
      0))
  (define ze-table
    (build-table 
     s1l
     s2l
     (λ (s1i s2i)
       (if (char=? (string-ref s1 s1i)
                   (string-ref s2 s2i))
         (+ 1 (table-ref (sub1 s1i) (sub1 s2i)))
         (max (table-ref (sub1 s1i)      s2i)
              (table-ref       s1i (sub1 s2i)))))))
  (table-ref (sub1 s1l) (sub1 s2l)))

(test (lcs-length "Artist" "Artsy")
      4)

