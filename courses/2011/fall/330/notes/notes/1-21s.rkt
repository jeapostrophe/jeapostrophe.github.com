#lang lazy

(define print-only-errors #f)
(define (test l r)
  (if (equal? l r)
      (if print-only-errors
          (void)
          (printf "Test Passed~n"))
      (printf "Test Failed.~nActual:   ~S ~nExpected: ~S~n" l r)))

;; take-while : (A -> Bool) (List A) -> (List A)
(define (take-while ? l)
  (cond
   [(empty? l)
    empty]
   [else
    (if (? (first l))    
        (cons (first l)
              (take-while ? (rest l)))
        empty)]))
    
(test (take-while (lambda (n) (< n 5)) (list 1 2 3 4 5 1 2))
      (list 1 2 3 4))

(define ones
  (cons 1 ones))

(test (take 10 (take-while (lambda (n) (= n 1)) ones))
      (list 1 1 1 1 1 1 1 1 1 1))

(define nats
  ;; 0, 1, 2, 3, 4, 5, 6, ...
  (cons 0 (map add1 nats)))

;; build-infinite-list : (nat -> A) -> (List A)
(define (build-infinite-list f)
  ;; (f 0), (f 1), (f 2), (f 3), (f 4), (f 5), (f 6), ...
  (map f nats))

(test (list-ref (build-infinite-list add1) 60)
      61)
(test (list-ref (build-infinite-list number->string) 60)
      "60")

(define exists ormap)

;; divisble-by-other-things? : nat (List nat) -> bool
(define (divisble-by-other-things? n factors)
  (exists (lambda (m) (zero? (remainder n m)))
          factors))

;; not-divisible-by-these-guys : nat (List nat) -> bool
(define (not-divisible-by-these-guys n factors)
  (not
   (divisble-by-other-things?
    n
    (take-while (lambda (m) (m . <= . (sqrt n)))
                factors))))

;; prime? : nat -> bool
(define (prime? n)
  ;; Prime means only divisible by 1 or itself
  ;; Prime means not divisible by other things
  (cond
   [(<= n 1)
    #f]
   [else
    (not-divisible-by-these-guys
     n
     ;; 0, 1, 2, ...
     ;;nats
     ;; 1, 2, ...
     ;;(rest nats)
     ;; 2, ...
     (rest (rest nats)))]))

(test (prime? 0) #f)
(test (prime? 1) #f)
(test (prime? 3) #t)
(test (prime? 2) #t)
(test (prime? 4) #f)
(test (prime? 1361) #t)
(test (prime? 561) #f)
(test (prime? 1009) #t)
(test (prime? 25) #f)

;; primes : (List nat)
(define primes
  (filter prime? nats))

(test (take 10 primes)
      (list 2 3 5 7 11 13 17 19 23 29))

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
       947    953    967    971    977    983    991    997   1009   1013))

;; prime?/fast : nat -> bool
(define (prime?/fast n)
  ;; Prime means only divisible by 1 or itself
  ;; Prime means not divisible by other things
  (cond
   [(<= n 1)
    #f]
   [(= n 2)
    #t]
   [else
    (not-divisible-by-these-guys
     n
     ;; 0, 1, 2, ...
     ;;nats
     ;; 1, 2, ...
     ;;(rest nats)
     ;; 2, ...
     primes/fast)]))

;; primes/fast : (List nat)
(define primes/fast
  (filter prime?/fast nats))

(test (prime?/fast 0) #f)
(test (prime?/fast 1) #f)
(test (prime?/fast 3) #t)
(test (prime?/fast 2) #t)
(test (prime?/fast 4) #f)
(test (prime?/fast 1361) #t)
(test (prime?/fast 561) #f)
(test (prime?/fast 1009) #t)
(test (prime?/fast 25) #f)

(test (take 10 primes/fast)
      (list 2 3 5 7 11 13 17 19 23 29))

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
       947    953    967    971    977    983    991    997   1009   1013))

#|
27 = 3^3
54 = 2^1 * 3^3 * 5^0 * 7^0

j = 13
a = 1
y = 25

2^13 * 3^1 * 5^25 = jay
|#

"First trial"
"Primes"
(time (! (list-ref primes 500)))
"Primes Fast"
(time (! (list-ref primes/fast 500)))

"Second trial"
"Primes"
(time (! (list-ref primes 500)))
"Primes Fast"
(time (! (list-ref primes/fast 500)))

"Primes"
(time (! (list-ref primes 800)))
"Primes Fast"
(time (! (list-ref primes/fast 800)))

;; build-vector : nat (nat -> A) -> (vector A)
(define (build-vector num f)
  (apply vector (build-list num f)))

(test (vector-ref (build-vector 10 add1) 9)
      10)
(test (vector-ref (build-vector 10 number->string) 9)
      "9")

;; build-table : nat nat (nat nat -> A) -> (vector (vector A))
(define (build-table rows cols f)
  (build-vector
   rows
   (lambda (i)
     (build-vector
      cols
      (lambda (j) (f i j))))))

(test (vector-ref (build-table 10 5 +) 5)
      (vector (+ 5 0)
              (+ 5 1)
              (+ 5 2)
              (+ 5 3)
              (+ 5 4)))

(test (vector-ref (vector-ref (build-table 10 10 +) 5) 4)
      (+ 5 4))
(test (vector-ref (vector-ref (build-table 10 10 *) 5) 4)
      (* 5 4))
(test (vector-ref (vector-ref (build-table 10 10 /) 5) 4)
      (/ 5 4))

;; table-ref : (V (V A)) nat nat -> A
(define (table-ref tbl i j)
  (cond
   [(or (< i 0) (< j 0))
    0]
   [else
    (vector-ref (vector-ref tbl i) j)]))

(test (table-ref (build-table 10 10 /) 5 4)
      (/ 5 4))
(test (table-ref (build-table 10 10 /) 5 -4)
      0)

;; lcs-length : string string -> nat
(define (lcs-length s1 s2)
  (define rows
    (string-length s1))
  (define cols
    (string-length s2))
  (define tbl
    (build-table
     rows cols
     (lambda (i j)
       (define s1_i (string-ref s1 i))
       (define s2_j (string-ref s2 j))
       (if (char=? s1_i s2_j)
           (add1 (table-ref tbl (sub1 i) (sub1 j)))
           (max (table-ref tbl i (sub1 j))
                (table-ref tbl (sub1 i) j))))))
  (table-ref tbl (sub1 rows) (sub1 cols)))

(test (lcs-length "xA" "yB")
      ;;(lcs-length "x" "y")      
      (max (lcs-length "xA" "y")
           (lcs-length "x" "yB")))
(test (lcs-length "xA" "yA")
      (add1 (lcs-length "x" "y")))

(test (lcs-length "Awesome sauce" "Cutiepie")
      2)
(test (lcs-length "Jay" "Cutiepie")
      0)
(test (lcs-length "Artist" "Artsy")
      4)

;; table-ref* : (V (V A)) nat nat -> A
(define (table-ref* tbl i j)
  (cond
   [(or (< i 0) (< j 0))
    ""]
   [else
    (vector-ref (vector-ref tbl i) j)]))

(test (table-ref* (build-table 10 10 /) 5 4)
      (/ 5 4))
(test (table-ref* (build-table 10 10 /) 5 -4)
      "")

;; funny-max : string string -> string
(define (funny-max s1 s2)
  (if ((string-length s1) . > . (string-length s2))
      s1
      s2))

(test (funny-max "Foo" "Barz") "Barz")
(test (funny-max "Foo" "B") "Foo")

;; lcs : string string -> nat
(define (lcs s1 s2)
  (define rows
    (string-length s1))
  (define cols
    (string-length s2))
  (define tbl
    (build-table
     rows cols
     (lambda (i j)
       (define s1_i (string-ref s1 i))
       (define s2_j (string-ref s2 j))
       (if (char=? s1_i s2_j)
           (string-append (table-ref* tbl (sub1 i) (sub1 j))
                          (string s1_i))
           (funny-max
            (table-ref* tbl i (sub1 j))
            (table-ref* tbl (sub1 i) j))))))
  (table-ref* tbl (sub1 rows) (sub1 cols)))

(test (lcs "xA" "yB")
      "")
(test (lcs "xA" "yA")
      "A")

(test (lcs "Awesome sauce" "Cutiepie")
      (or "ee"
          "ue"))
(test (lcs "Jay" "Cutiepie")
      "")
(test (lcs "Artist" "Artsy")
      "Arts")

(lcs "Awesome sauce"
     "Cutiepie")
(lcs "Jay McCarthy the doctor who doesn't help people"
     "Wayne Robison, sits in the front row")
