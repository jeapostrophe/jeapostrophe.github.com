#lang plai
(print-only-errors #t)

;; check-temps : lon -> bool
(define (check-temps lo hi lon)
  (foldr (λ (first-lon ct-rest-lon)
           (and (< lo first-lon hi)
                ct-rest-lon))
         true
         lon))

(test (check-temps 5 95 empty)
      true)
(test (check-temps 5 95 (list 1))
      false)
(test (check-temps 5 95 (list 10))
      true)
(test (check-temps 5 95 (list 1 7))
      false)
(test (check-temps 5 95 (list 10 7))
      true)

;; convert : lon -> num
(define (convert lon)
  (foldr (λ (first-lon c-rest-lon)
           (+ first-lon
              (* 10 c-rest-lon)))
         0
         lon))

(test (convert empty)
      0)
(test (convert (list 2 3))
      32)
(test (convert (list 1 2 3))
      321)

(define (sum lon) 
  (foldr + 0 lon))

;; average-price : lon -> num
(define (average-price lon)
  (define len (length lon))
  (foldr (λ (x r) (+ (/ x len) r))
         0
         lon))

;; (foldr f e (map g l))
;; =
;; (foldr (λ (x r) (f (g x) r)) e l)

(test (average-price empty)
      0)
(test (average-price (list 1))
      1)
(test (average-price (cons       1    empty))
      1)
(test                (   + (/ 1 1)        0)
      1)
(test (average-price (list 1 1))
      1)
(test (+ (/ 1 2) (/ 1 2) 0)
      1)

;; convertFC : lon -> lon
(define (convertFC lon)
  (map (λ (x) (- x 32)) lon))

(test (convertFC empty)
      empty)
(test (convertFC (list 32))
      (list 0))

(define (eliminate-exp lon)
  (filter (λ (x) (< x 10)) lon))

;; suffixes : los -> lolos
(define (suffixes los)
  (cons los
        (cond [(empty? los)
               empty]
              [else
               (suffixes (rest los))])))

(test (suffixes empty) 
      (list empty))
(test (suffixes (list 1 2)) 
      (list (list 1 2)
            (list 2)
            empty))

(struct unknown ())
(struct person (name age eye mom pop))

(define jay-t
  (person "Jay" 27 "Amazing blue"
          (person "Pammy" 999 "Brown"
                  (unknown) (unknown))
          (person "Jimmy" 999 "Brown"
                  (unknown) (unknown))))

(define (tree-fold replace-person
                   replace-unknown
                   t)
  (cond [(unknown? t)
         replace-unknown]
        [else
         (replace-person
          (person-name t)
          (person-age t)
          (person-eye t)
          (tree-fold replace-person
                     replace-unknown
                     (person-mom t))
          (tree-fold replace-person
                     replace-unknown
                     (person-pop t)))]))

(define (count-persons t)
  (tree-fold (λ (n a e mc pc)
               ;; eye-colors = (cons e (append mc pc))
               ;; tree-map = (person (f n) a e mc pc)
               (+ 1 mc pc))
             0 ;; eye-colors = empty
             t))

(test (count-persons (unknown))
      0)
(test (count-persons jay-t)
      3)

;; compose-func : (b -> c) (a -> b) -> (a -> c)
(define (compose-func after before)
  (define (new-function x)
    (after (before x)))
  new-function)

(test ((compose-func add1 sub1) 5)
      5)
(test ((compose-func string-length number->string)
       1000)
      4)

;; flatten : lolos -> los
(define (flatten lolos)
  ;; (cond [(empty? lolos)
  ;;        empty]
  ;;       [else
  ;;        (append (first lolos)
  ;;                (flatten (rest lolos)))])
  (foldr append empty lolos))

(test (flatten empty)
      empty)
(test (flatten (list empty))
      empty)
(test (flatten (list (list 1 2) empty (list 3 4)))
      (list 1 2 3 4))

;; bucket : lon -> lolon
(define (bucket lon)
  (foldr (λ (first-lon bucket-rest-lon)
           (cond
           [(empty? bucket-rest-lon)
            ;; Test case 1
            ;; first-lon = 1
            ;; bucket-rest-lon = empty

            (list (list first-lon))]
           [(= first-lon
               (first (first bucket-rest-lon)))
            
            ;; Test case 2
            ;; first-lon = 1
            ;; bucket-rest-lon = (list (list 1))

            (cons (cons first-lon
                        (first bucket-rest-lon))
                  (rest bucket-rest-lon))]
           [else
            ;; Test case 3
            ;; first-lon = 2
            ;; bucket-rest-lon = (list (list 1 1))

            ;; (list (list 2)
            ;;       (list 1 1))
            ;; (cons (list 2)
            ;;       bucket-rest-lon)
            (cons (list first-lon)
                  bucket-rest-lon)]))
         empty
         lon))

(test (bucket empty)
      empty)
(test (bucket (list 1))
      (list (list 1)))
(test (bucket (list 1 1))
      (list (list 1 1)))
(test (bucket (list 2 1 1))
      (list (list 2)
            (list 1 1)))

