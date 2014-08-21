#lang lazy

(define print-only-errors #t)
(define (test l r)
  (if (equal? l r)
    (if print-only-errors
      (void)
      (printf "Test Passed~n"))
    (printf "Test Failed.~nActual:   ~S ~nExpected: ~S~n" l r)))

(define (take-while ? l)
  (cond
    [(empty? l)
     empty]
    [(? (first l))
     (cons (first l)
           (take-while ? (rest l)))]
    [else
     ;; filter would be .... (take-while ? (rest l))
     empty]))

(test (take-while odd? (list 1 2 3 4 5))
      (list 1))
(test (take-while even? (list 1 2 3 4 5))
      empty)
(test (take-while even? empty)
      empty)

(define nats
  (cons 0 (map add1 nats)))

(test (take-while even? nats)
      (list 0))

(test (take 10 (take-while number? nats))
      (list 0 1 2 3 4 5 6 7 8 9))

(define (build-infinite-list f)
  (map f nats))

(define (check-bil f)
  (for ([n (in-range 100)])
    (define i (random 1000))
    (test (list-ref (build-infinite-list f) i)
          (f i))))

(check-bil add1)
(check-bil number->string)
(check-bil list)

(define (divides n x)
  (zero? (modulo n x)))

(define (prime?-like n possible-factors)
  (not
   (or (= n 1)
       (ormap (λ (x) (divides n x))
              (take-while
               (λ (x) (<= x (integer-sqrt n)))
               possible-factors)))))

(define (prime? n)
  (prime?-like n (rest (rest nats))))

(test (prime? 1)
      #f)
(test (prime? 2)
      #t)
(test (prime? 3)
      #t)
(test (prime? 6)
      #f)
(test (prime? 1993)
      #t)

(define primes
  (filter prime? (rest nats)))

(test (take 10 primes)
      (list 2 3 5 7 11 13 17 19 23 29))

(define (prime?/fast n)
  (prime?-like n primes/fast))

(define primes/fast
  (cons 2 (filter prime?/fast
                  (list-tail nats 3))))

;; "normal function recursion"... your base case is at the end... INDUCTION
;; "lazy data recursion"... your base case is at the beginning... CO-INDUCTION

(test (prime?/fast 1)
      #f)
(test (prime?/fast 2)
      #t)
(test (prime?/fast 3)
      #t)
(test (prime?/fast 6)
      #f)
(test (prime?/fast 1993)
      #t)

(test (take 10 primes/fast)
      (list 2 3 5 7 11 13 17 19 23 29))

(define (web-server initial-state read-request handle)
  (define (loop current-state)
    (define req (read-request))
    (define new-state
      (handle current-state req))
    (loop new-state))
  (loop initial-state))

(define (unfold f a)
  (cons a (unfold f (f a))))

(define (web-server-2.0 initial-state read-request handle)
  (unfold initial-state
          (λ (last-state)
            (handle last-state (read-request)))))

;; Coq
;; 401R... 430

;; "primes"
;; (time
;;  (! (list-ref primes 10000)))

;; "primes/fast"
;; (time
;;  (! (list-ref primes/fast 10000)))

(define (build-vector num f)
  (apply vector (build-list num f)))

(define (build-table rows cols f)
  (build-vector rows
                (λ (r)
                  (build-vector cols
                                (λ (c)
                                  (f r c))))))

(define (check-table f)
  (for ([n (in-range 100)])
    (define rows (random 100))
    (define cols (random 100))
    (define i (random rows))
    (define j (random cols))
    (test (vector-ref (vector-ref (build-table rows cols f) i) j)
          (f i j))))

(check-table +)
(check-table cons)
(check-table -)

(define (lcs-length s1 s2)
  (define s1-len
    (string-length s1))
  (define s2-len
    (string-length s2))
  (define (table-ref i j)
    (if (or (negative? i) (negative? j))
      0
      (vector-ref (vector-ref ze-table i) j)))
  (define ze-table
    (build-table
     s1-len
     s2-len
     (λ (s1-i s2-j)
       (if (char=? (string-ref s1 s1-i)
                   (string-ref s2 s2-j))
         (+ 1 (table-ref (sub1 s1-i) (sub1 s2-j)))
         (max (table-ref       s1-i  (sub1 s2-j))
              (table-ref (sub1 s1-i)       s2-j))))))
  (table-ref (sub1 s1-len)
             (sub1 s2-len)))

(test (lcs-length "Artist" "Artsy")
      4)
(test (lcs-length "The artist is very artsy."
                  "An artsy artist oftens arts.")
      16)

(define (lcs s1 s2)
  (define s1-len
    (string-length s1))
  (define s2-len
    (string-length s2))
  (define (table-ref i j)
    (if (or (negative? i) (negative? j))
      ""
      (vector-ref (vector-ref ze-table i) j)))
  (define ze-table
    (build-table
     s1-len
     s2-len
     (λ (s1-i s2-j)
       (if (char=? (string-ref s1 s1-i)
                   (string-ref s2 s2-j))
         (string-append
          (table-ref (sub1 s1-i) (sub1 s2-j))
          (string (string-ref s1 s1-i)))
         (string-max
          (table-ref       s1-i  (sub1 s2-j))
          (table-ref (sub1 s1-i)       s2-j))))))
  (table-ref (sub1 s1-len)
             (sub1 s2-len)))

(define (string-max s1 s2)
  (if (> (string-length s1)
         (string-length s2))
    s1
    s2))

(test (lcs "Artist" "Artsy")
      "Arts")
(test (lcs "The artist is very artsy."
           "An artsy artist oftens arts.")
      " artstis e arts.")
(test (lcs "Jay McCarthy" "Benjamin Hansen")
      "a a")
