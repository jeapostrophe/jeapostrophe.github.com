#lang lazy

(define print-only-errors #t)
(define (test l r)
  (if (equal? l r)
    (if print-only-errors
      (void)
      (printf "Test Passed~n"))
    (printf "Test Failed.~nActual:   ~S ~nExpected: ~S~n" l r)))

;; take-while : (a -> bool) (listof a) -> (listof a)
(define (take-while ? l)
  (cond
    [(empty? l)
     empty]
    [(? (first l))
     (cons (first l)
           (take-while ? (rest l)))]
    [else
     empty]))

(test (take-while odd? (list 1 2 3 4))
      (list 1))

(define nats
  (cons 0 (map add1 nats)))

(test (take-while odd? nats)
      empty)
(test (take 10 (take-while number? nats))
      (list 0 1 2 3 4 5 6 7 8 9))

;; build-infinite-list : (number -> a) -> (list a)
(define (build-infinite-list f)
  (map f nats))

(define (check-bil f i)
  (test (list-ref (build-infinite-list f) i)
        (f i)))

(check-bil add1 (random 1000))
(check-bil number->string (random 1000))

;; divides : number number -> bool
(define (divides n f)
  (zero? (modulo n f)))

;; prime?-like : epi (listof number) -> bool
(define (prime?-like n possible-factors)
  (cond
    [(<= n 1)
     #f]
    ;;[(= n 2)
    ;; #t]
    [else
     (not
      (ormap (λ (x) (divides n x))
             (take-while
              (λ (x) (<= x (integer-sqrt n)))
              possible-factors)))]))

;; prime? : epi -> bool
(define (prime? n)
  (prime?-like n (rest (rest nats))))

(test (prime? 1)
      #f)
(test (prime? 2)
      #t)
(test (prime? 3)
      #t)
(test (prime? 4)
      #f)
(test (prime? 1993)
      #t)

;; primes : (listof epi)
(define primes
  (filter prime? (rest nats)))

(test (take 10 primes)
      (list 2 3 5 7 11 13 17 19 23 29))

;; prime?/fast : epi -> bool
(define (prime?/fast n)
  (prime?-like n primes/fast))

;; primes/fast : (listof epi)
(define primes/fast
  (cons 2 (filter prime?/fast
                  (list-tail nats 3))))

;; "normal recursion" --- is INDUCTIVE
;; whereas this recursion --- is CO-INDUCTIVE

(test (prime?/fast 1)
      #f)
(test (prime?/fast 2)
      #t)
(test (prime?/fast 3)
      #t)
(test (prime?/fast 4)
      #f)
(test (prime?/fast 1993)
      #t)

(test (take 10 primes/fast)
      (list 2 3 5 7 11 13 17 19 23 29))

;; servers are co-inductive
(define (web-server initial-state read-request handler)
  (define (loop web-state)
    (define req
      (read-request))
    (define new-state
      (handler web-state req))
    (loop new-state))
  (loop initial-state))

(define (build-vector num f)
  (apply vector (build-list num f)))

(define (build-table rows cols f)
  (build-vector rows (λ (r) (build-vector cols (λ (c) (f r c))))))

(define (check-bt r c f)
  (define i (random r))
  (define j (random c))
  (test (vector-ref (vector-ref (build-table r c f) i) j)
        (f i j)))

(check-bt 10 10 -)
(check-bt 10 10 cons)

;; lcs-length : string string -> number
(define (lcs-length s1 s2)
  (define s1len
    (string-length s1))
  (define s2len
    (string-length s2))
  (define (table-ref tab i j)
    (if (or (negative? i)
            (negative? j))
      0
      (vector-ref (vector-ref tab i) j)))
  (define ze-table
    (build-table
     s1len
     s2len
     (λ (s1-i s2-j)
       (cond
         [(char=? (string-ref s1 s1-i)
                  (string-ref s2 s2-j))
          (+ 1 (table-ref ze-table
                          (sub1 s1-i)
                          (sub1 s2-j)))]
         [else
          (max (table-ref ze-table
                          s1-i
                          (sub1 s2-j))
               (table-ref ze-table
                          (sub1 s1-i)
                          s2-j))]))))
  (table-ref ze-table
             (sub1 s1len)
             (sub1 s2len)))

(test (lcs-length "Artist" "Artsy")
      4)
(test (lcs-length "(define (prime? n)
  (prime?-like n (rest (rest nats))))"
                  "(define (prime?/fast n)
  (prime?-like n primes/fast))")
      43)

;; lcs : string string -> string
(define (lcs s1 s2)
  (define s1len
    (string-length s1))
  (define s2len
    (string-length s2))
  (define (table-ref tab i j)
    (if (or (negative? i)
            (negative? j))
      ""
      (vector-ref (vector-ref tab i) j)))
  (define ze-table
    (build-table
     s1len
     s2len
     (λ (s1-i s2-j)
       (cond
         [(char=? (string-ref s1 s1-i)
                  (string-ref s2 s2-j))
          (string-append
           (table-ref ze-table
                      (sub1 s1-i)
                      (sub1 s2-j))
           (string (string-ref s1 s1-i)))]
         [else
          (string-max
           (table-ref ze-table
                      s1-i
                      (sub1 s2-j))
           (table-ref ze-table
                      (sub1 s1-i)
                      s2-j))]))))
  (table-ref ze-table
             (sub1 s1len)
             (sub1 s2len)))

(define (string-max s1 s2)
  (if (> (string-length s1)
         (string-length s2))
    s1
    s2))

(test (lcs "Artist" "Artsy")
      "Arts")
(test (lcs "(define (prime? n)
  (prime?-like n (rest (rest nats))))"
                  "(define (prime?/fast n)
  (prime?-like n primes/fast))")
      "(define (prime? n)\n  (prime?-like n resst))")

;; 401R ... 430
;; Crazy language.... Coq

(+ 1 "string")
(rest empty)
