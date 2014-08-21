;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 1-5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; check-temps1 : LoN -> bool
(define (check-temps1 a-lon)
  (cond
   [(empty? a-lon)
    #t]
   [else
    (and
     (<= 5 (first a-lon) 95)
     (check-temps1 (rest a-lon)))]))

;; check-temps : LoN num num -> bool
(define (check-temps a-lon low high)
  (cond
   [(empty? a-lon)
    #t]
   [else
    (and
     (<= low (first a-lon) high)
     (check-temps1 (rest a-lon)))]))

;; convert : LoN -> num
(define (convert a-lon)
  (cond
   [(empty? a-lon)
    0]
   [else

    ; 321
    (+ (first a-lon) ; 1
       ;; (rest a-lon) ; (list 2 3)

       (* 10 ; 320
          (convert (rest a-lon)))) ; 32
    ]))

(check-expect (convert (list 1 2 3))
              ;;(+ 1 (* 10 (convert (list 2 3))))
              ;;(+ 1 (* 10 (+ 2 (* 10 (convert (list 3))))))
              ;;(+ 1 (* 10 (+ 2 (* 10 (+ 3 (* 10 (convert (list))))))))
              ;;(+ 1 (* 10 (+ 2 (* 10 (+ 3 (* 10 0))))))
              ;;(+ 1 (* 10 (+ 2 (* 10 (+ 3 0)))))
              ;;(+ 1 (* 10 (+ 2 (* 10 3))))
              ;;(+ 1 (* 10 (+ 2 30)))
              ;;(+ 1 (* 10 32))
              ;;(+ 1 320)
              321)

;; average-price : LoN -> num
(define (average-price a-lon)
  (cond
   [(empty? a-lon)
    0]
   [else

    ;; X0 ... Xn
    ;; (+ X0 ... Xn) / n 

    ;; X0
    ;; X1 ... Xn
    ;; (+ X1 ... Xn) / n - 1 * (n - 1 / n) + X0 / n

    (/ (sum a-lon)
       (length a-lon))
    
    ;; (+ (* (average-price (rest a-lon))
          
    ;;       (/ (- (length a-lon) 1)
    ;;          (length a-lon)))
    ;;    (/ (first a-lon)
    ;;       (length a-lon)))
    ]))

(check-expect (average-price (list 1 2 3))
              2)

;; convert-one-FC : Num -> Num
(define (convert-one-FC num)
  (* (- num 32) 5/9))

;; convertFC : LoN -> LoN
(define (convertFC a-lon)
  (cond
   [(empty? a-lon)
    empty]
   [else
    (cons (convert-one-FC (first a-lon))
          (convertFC (rest a-lon)))]))

;; eliminate-exp : LoN -> LoN
(define (eliminate-exp a-lon limit)
  (cond
   [(empty? a-lon)
    empty]
   [else
    (cond
     [(<= (first a-lon) limit)
      (cons
       (first a-lon)
       (eliminate-exp (rest a-lon) limit))]
     [else
      (eliminate-exp (rest a-lon) limit)])]))


;; suffixes : list -> LoL
(define (suffixes l)
  (cond
   [(empty? l)
    (list empty)]
   [else

    ;; 5 ; num
    ;; (list 2 3) ; L(num)
    ;; ;; cons : X L(X) -> L(X)
    ;; (cons 5 (list 2 3)) ; L(num)
    

    (cons
     l ; L(X) = Y
     
     ;; (first l) ; X

     ;; (rest l) ; L(X) = Y

     (suffixes (rest l))) ; L(L(X)) = L(Y)

    ; L(L(X)) = L(Y)
    ]))

(define (suffixes-supa-cleva l)
  (cons l
        (cond
         [(empty? l)
          empty]
         [else
          (suffixes-supa-cleva (rest l))])))

(define-struct unknown ())
(define-struct person (name birthyear eyecolor father mother))

(define Abraham
  (make-person "Abe" 1000
               'failing
               (make-unknown)
               (make-unknown)))
(define Homer
  (make-person "Homer" 1950
               'donuts
               Abraham
               (make-unknown)))
(define Marge
  (make-person "Marge" 1955
               'blue
               (make-unknown)
               (make-unknown)))
(define Bart
  (make-person "Bart" 1990
               'black-dots
               Homer
               Marge))

;; count-persons : (or unknown person) -> number
(define (count-persons ftree)
  (cond
   [(unknown? ftree)
    0]
   [else
    (+ 1
       (count-persons (person-father ftree))
       (count-persons (person-mother ftree)))]))

;; person-age : person -> number
(define (person-age p)
  (- 2011 (person-birthyear p)))

;; total-age : (or unknown person) -> number
(define (total-age ftree)
  (cond
   [(unknown? ftree)
    0]
   [else
    (+ (person-age ftree)
       (total-age (person-father ftree))
       (total-age (person-mother ftree)))]))

;; average-age : (or unknown person) -> number
(define (average-age ftree)
  (cond
   [(unknown? ftree)
    0]
   [else
    (/ (total-age ftree)
       (count-persons ftree))]))

;; eye-colors : (or unknown person) -> (list symbols)
(define (eye-colors ftree)
  (cond
   [(unknown? ftree)
    empty]
   [else
    (cons
     (person-eyecolor ftree) ; X
     (append ; L(X)
      (eye-colors (person-father ftree)) ; L(X)
      (eye-colors (person-mother ftree))))])) ; L(X)

