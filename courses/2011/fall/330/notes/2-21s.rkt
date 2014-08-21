#lang lazy

(define print-only-errors #f)
(define (test l r)
  (if (equal? l r)
      (if print-only-errors
          (void)
          (printf "Test Passed~n"))
      (printf "Test Failed.~nActual:   ~S ~nExpected: ~S~n" l r)))

(test (+ 1 1)
      2)

;; take-while : (A -> Bool) (List A) -> (List A)
(define (take-while ? l)
  (cond
   [(empty? l)
    empty]
   [else
    (if (? (first l))
        (cons 
         (first l)
         (take-while ? (rest l)))
        empty)]))

(test (take-while odd? (list 1 3 4))
      (list 1 3))
(test (take-while (lambda (n) (< n 5)) (list 1 2 3 4 5 1 2))
      (list 1 2 3 4))

(define ones
  (cons 1 ones))

(test (take 10 (take-while odd? ones))
      (list 1 1 1 1 1
            1 1 1 1 1))

(define nats
  ;; 0, 1, 2, 3
  (cons 0 (map add1 nats)))

;; build-infinite-list : (nat -> A) -> (List A)
(define (build-infinite-list f)
  ;; (f 0), (f 1), (f 2), (f 3), ...
  (map f nats))

(define (test-build-infinite-list f i)
  (define r (random i))
  (test
   (list-ref (build-infinite-list f) r)
   (f r))
  (test
   (list-ref (build-infinite-list f) i)
   (f i)))

(test-build-infinite-list add1 10)
(test-build-infinite-list number->string 10)
(test-build-infinite-list + 99)
(test-build-infinite-list sub1 77)

;; things-between-2-and-its-sqrt-from-these-guys : nat (List nat) -> (List nat)
(define (things-between-2-and-its-sqrt-from-these-guys
         n from)
  (define sn (sqrt n))
  (take-while (lambda (m) (<= 2 m sn))
              from))

(define exists
  ormap)

(define (divisble-by-any-of-these? n these)
  (exists (lambda (m)
            (zero? (remainder n m)))
          these))

;; prime? : Nat -> Bool
(define (prime? n)
  (cond
   [(<= n 1)
    #f]
   [else
    ;; Prime means only divisible by 1 and itself
    ;; Prime means not divisble by the things inbetween
    ;; Prime means not divisble by the things in between 2 and its sqrt
    (not
     (divisble-by-any-of-these?
      n
      (things-between-2-and-its-sqrt-from-these-guys
       n
       ;; 0, 1, 2, ...
       ;;nats
       ;; 1, 2, ...
       ;;(rest nats)
       ;; 2, ...
       (rest (rest nats))
       )))]))

"prime?"
(test (prime? 0) #f)
(test (prime? 1) #f)
(test (prime? 2) #t)
(test (prime? 3) #t)
(test (prime? 514) #f)
(test (prime? 521) #t)
(test (prime? 101) #t)

;; primes : List Nat
(define primes
  (filter prime? nats))

"primes"
(test (take 9 primes)
      (list 2 3 5 7 11 13 17 19 23))
(test
 (take 170 primes)
 (list 2      3      5      7     11     13     17     19     23     29 
       31     37     41     43     47     53     59     61     67     71 
       73     79     83     89     97    101    103    107    109    113 
       127    131    137    139    149    151    157    163    167    173 
       179    181    191    193    197    199    211    223    227    229 
       233    239    241    251    257    263    269    271    277    281 
       283    293    307    311    313    317    331    337    347    349 
       353    359    367    373    379    383    389    397    401    409 
       419    421    431    433    439    443    449    457    461    463 
       467    479    487    491    499    503    509    521    523    541 
       547    557    563    569    571    577    587    593    599    601 
       607    613    617    619    631    641    643    647    653    659 
       661    673    677    683    691    701    709    719    727    733 
       739    743    751    757    761    769    773    787    797    809 
       811    821    823    827    829    839    853    857    859    863 
       877    881    883    887    907    911    919    929    937    941 
       947    953    967    971    977    983    991    997   1009   1013 ))

;; prime?/fast : Nat -> Bool
(define (prime?/fast n)
  (cond
   [(<= n 1)
    #f]
   [(= n 2)
    #t]
   [else
    ;; Prime means only divisible by 1 and itself
    ;; Prime means not divisble by the things inbetween
    ;; Prime means not divisble by the things in between 2 and its sqrt
    (not
     (divisble-by-any-of-these?
      n
      (things-between-2-and-its-sqrt-from-these-guys
       n
       ;; 0, 1, 2, ...
       ;;nats
       ;; 1, 2, ...
       ;;(rest nats)
       ;; 2, 3, 5, ...
       primes/fast
       )))]))

;; primes/fast : List Nat
(define primes/fast
  (filter prime?/fast nats))

"prime?/fast"
(test (prime?/fast 0) #f)
(test (prime?/fast 1) #f)
(test (prime?/fast 2) #t)
(test (prime?/fast 3) #t)
(test (prime?/fast 514) #f)
(test (prime?/fast 521) #t)
(test (prime?/fast 101) #t)

"primes/fast"
(test (take 9 primes/fast)
      (list 2 3 5 7 11 13 17 19 23))
(test
 (take 170 primes/fast)
 (list 2      3      5      7     11     13     17     19     23     29 
       31     37     41     43     47     53     59     61     67     71 
       73     79     83     89     97    101    103    107    109    113 
       127    131    137    139    149    151    157    163    167    173 
       179    181    191    193    197    199    211    223    227    229 
       233    239    241    251    257    263    269    271    277    281 
       283    293    307    311    313    317    331    337    347    349 
       353    359    367    373    379    383    389    397    401    409 
       419    421    431    433    439    443    449    457    461    463 
       467    479    487    491    499    503    509    521    523    541 
       547    557    563    569    571    577    587    593    599    601 
       607    613    617    619    631    641    643    647    653    659 
       661    673    677    683    691    701    709    719    727    733 
       739    743    751    757    761    769    773    787    797    809 
       811    821    823    827    829    839    853    857    859    863 
       877    881    883    887    907    911    919    929    937    941 
       947    953    967    971    977    983    991    997   1009   1013 ))

"Speed Trial #1"
"Slow?"
(time (! (list-ref primes 500)))
"Fast?"
(time (! (list-ref primes/fast 500)))

"Speed Trial #2"
"Slow?"
(time (! (list-ref primes 800)))
"Fast?"
(time (! (list-ref primes/fast 800)))

;; build-vector : nat (nat -> a) -> (vector a)
(define (build-vector num f)
  (apply vector (build-list num f)))

(test (vector-ref (build-vector 10 add1) 5)
      6)
(test (build-vector 5 (lambda (j) (+ 5 j)))
      (vector (+ 5 0)
              (+ 5 1)
              (+ 5 2)
              (+ 5 3)
              (+ 5 4)))

;; build-table : nat nat (nat nat -> a) -> (vector (vector a))
(define (build-table rows cols f)
  (build-vector
   rows
   (lambda (i)
     (build-vector
      cols
      (lambda (j)
        (f i j))))))

(test (build-table 3 3 +)
      (vector (vector (+ 0 0) (+ 0 1) (+ 0 2))
              (vector (+ 1 0) (+ 1 1) (+ 1 2))
              (vector (+ 2 0) (+ 2 1) (+ 2 2))))

(test (vector-ref (vector-ref (build-table 3 3 /) 2) 1)
      (/ 2 1))

(define (table-ref tbl i j)
  (if (and (<= 0 i (sub1 (vector-length tbl)))
           (<= 0 j (sub1 (vector-length (vector-ref tbl 0)))))
      (vector-ref (vector-ref tbl i) j)
      #f))

(test (table-ref (build-table 3 3 /) 2 1)
      (/ 2 1))
(test (table-ref (build-table 3 3 /) 99 99)
      #f)

;; lcs-length : string string -> nat
(define (lcs-length s1 s2)
  (define (table-ref* tbl i j)
    (or (table-ref tbl i j) 0))
  (define rows
    (string-length s1))
  (define cols
    (string-length s2))
  (define tbl
    (build-table
     rows cols
     ;; (lcs-length (string s_0 ... s_i) (string t_0 ... t_j))
     (lambda (i j)
       (if (char=? (string-ref s1 i) (string-ref s2 j))
           (add1 (table-ref* tbl (sub1 i) (sub1 j)))
           (max (table-ref* tbl i (sub1 j))
                (table-ref* tbl (sub1 i) j))))))
  (table-ref tbl (sub1 rows) (sub1 cols)))

(test (lcs-length "Artist" "Artsy")
      4)
(test (lcs-length "xxxxxxA" "yyyyyyA")      
      (add1 (lcs-length "xxxxxx" "yyyyyy")))
(test (lcs-length "xxxxxxA" "yyyyyyB")
      (max (lcs-length "xxxxxxA" "yyyyyy")
           (lcs-length "xxxxxx" "yyyyyyB")))
(test (lcs-length "Pikachu eats brains"
                  "Raichu eats Pikachu")
      11)

(define (string-max s1 s2)
  (if ((string-length s1) . < . (string-length s2))
      s2
      s1))

;; lcs : string string -> string
(define (lcs s1 s2)
  (define (table-ref* tbl i j)
    (or (table-ref tbl i j) ""))
  (define rows
    (string-length s1))
  (define cols
    (string-length s2))
  (define tbl
    (build-table
     rows cols
     ;; (lcs-length (string s_0 ... s_i) (string t_0 ... t_j))
     (lambda (i j)
       (if (char=? (string-ref s1 i) (string-ref s2 j))
           (string-append (table-ref* tbl (sub1 i) (sub1 j))
                          (string (string-ref s1 i)))
           (string-max (table-ref* tbl i (sub1 j))
                       (table-ref* tbl (sub1 i) j))))))
  (table-ref tbl (sub1 rows) (sub1 cols)))

(test (lcs "Artist" "Artsy")
      "Arts")
(lcs "Pikachu eats brains"
     "Raichu eats Pikachu")

