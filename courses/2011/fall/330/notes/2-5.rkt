;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 1-5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; check-temps1 : LoN -> boolean
(define (check-temps1 a-lon)
  (cond
   [(empty? a-lon)
    #t]
   [else
    ;;(+ 1 2 3) ; => 1 + 2 + 3
    (and
     (<= 5 (first a-lon) 95) ; 5 <= thing <= 95
     (check-temps1 (rest a-lon)))]))

;; check-temps : LoN num num -> boolean
(define (check-temps a-lon lo hi)
  (cond
   [(empty? a-lon)
    #t]
   [else
    ;;(+ 1 2 3) ; => 1 + 2 + 3
    (and
     (<= lo (first a-lon) hi) ; 5 <= thing <= 95
     (check-temps (rest a-lon) lo hi))]))

;; convert : LoD -> num
(define (convert a-lon)
  (cond
   [(empty? a-lon)
    0]
   [else
    (+ (first a-lon) ; 1
       ;;(rest a-lon) ; (list 2 3)
       (* 10 (convert (rest a-lon)))) ; 32
    ]))

(define (help-me-learn-to-program-racket a-lon pow)
  (cond
   [(empty? a-lon)
    0]
   [else
    (+ (* (expt 10 pow) (first a-lon))
       (help-me-learn-to-program-racket (rest a-lon) (add1 pow)))]))
(define (apostate-convert a-lon)
  (help-me-learn-to-program-racket a-lon 0))
       
(check-expect (convert (list 1 2 3))
              ;; (+ 1 (* 10 (convert (list 2 3))))
              ;; (+ 1 (* 10 (+ 2 (* 10 (convert (list 3))))))
              ;; (+ 1 (* 10 (+ 2 (* 10 (+ 3 (* 10 (convert (list))))))))
              ;; (+ 1 (* 10 (+ 2 (* 10 (+ 3 (* 10 0))))))
              ;; (+ 1 (* 10 2) (* 100 (+ 3 (* 10 0))))
              ;; (+ 1 (* 10 2) (* 100 3) (* 1000 0))
              ;; (+ 1 20 300 0)
              ;; (+ 1 320)
              321)

;; average-price : LoN -> Num
(define (average-price a-lon)
  (cond
   [(empty? a-lon)
    0]
   [else

    ;; X0 X1 ... Xn
    
    ;; (+
    ;;  (/ (first a-lon) (length a-lon)) ; X0 / n
    ;;  (* (/ (- (length a-lon) 1)
    ;;        (length a-lon))
    ;;     (average-price (rest a-lon)))) ; (+ X1 ... Xn) / n-1 * n-1/n

    ;; (+ X0 ... Xn) / n

    (/ (sum a-lon)
       (length a-lon))
    
    ]))

;; convert-one-FC : Num -> num
(define (convert-one-FC n)
  (* 5/9 (- n 32)))

;; convertFC : lon -> lon
(define (convertFC a-lon)
  (cond
   [(empty? a-lon)
    empty]
   [else
    (cons
     (convert-one-FC (first a-lon))
     (convertFC (rest a-lon)))]))

;; eliminate-exp : LoN num -> LoN
(define (eliminate-exp a-lon moms-limit)
  (cond
   [(empty? a-lon)
    empty]
   [else
    (cond
     [(<= (first a-lon) moms-limit)
      (cons
       (first a-lon)
       (eliminate-exp (rest a-lon) moms-limit))]
     [else
      (eliminate-exp a-lon moms-limit)])]))

;; suffixes : L -> listof L
(define (suffixes l)
  (cond
   [(empty? l)
    (list empty)]
   [else

    ;; cons : Y L(Y) -> L(Y)

    (cons
     l ; L(X) = Y
     ;; (first l) ; X
     ;; (rest l) ; L(X)
     (suffixes (rest l))) ; L(L(X)) = L(Y)

    ]))

(define-struct unknown ())
(define-struct person (name birthyear eye-color father mother))

(define Asael (make-unknown))
(define JSI
  (make-person "Joseph Smith" 1780
               'old
               Asael
               (make-unknown)))
(define TheLuce
  (make-person "Lucy, the MACK" 1780
               'beedy
               (make-unknown)
               (make-unknown)))
(define JSII
  (make-person "Joseph Smith" 1805
               'super-prophet-like
               JSI
               TheLuce))
(define TheEmma
  (make-person "Emma Hale Smith Bideman" 1803
               'penetrating
               (make-unknown)
               (make-unknown)))
(define JSIII
  (make-person "Joseph Smith" 1838
               'prophet-like
               JSII
               TheEmma))

;; count-persons : (or unknown person)  -> num
(define (count-persons ftree)
  (cond
   [(unknown? ftree)
    0]
   [else
    (+ 1
       (count-persons (person-father ftree))
       (count-persons (person-mother ftree)))]))

;; person-age : person -> num
(define (person-age p)
  (- 2011 (person-birthyear p)))

;; total-age : (or unknown person) -> num
(define (total-age ftree)
  (cond
   [(unknown? ftree)
    0]
   [else
    (+ (person-age ftree)
       (total-age (person-father ftree))
       (total-age (person-mother ftree)))]))

;; average-age : (or unknown person) -> num
(define (average-age ftree)
  (cond
   [(unknown? ftree)
    0]
   [else
    (/ (total-age ftree)
       (count-persons ftree))]))

;; eye-colours : (or unknown person) -> LoS
(define (eye-colours ftree)
  (cond
   [(unknown? ftree)
    empty]
   [else

    (cons
     (person-eyecolor ftree) ; S

                                        ; LoS
     (append (eye-colours (person-father ftree)) ; LoS
             (eye-colours (person-mother ftree))))])) ; LoS
