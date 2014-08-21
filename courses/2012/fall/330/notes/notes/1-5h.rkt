#lang plai
(print-only-errors #t)

;; (define l (cons 1 empty))
;; (equal? l l)
;; (eq? l l)
;; (equal? l (cons 1 empty))
;; (eq? l (cons 1 empty))

;; check-temps : num num lon -> boolean
(define (check-temps lo hi l)
  (foldr (λ (first-arg second-arg)
           (and (< lo first-arg hi)
                second-arg))
         true
         l))

(test (check-temps 5 95 empty)
      true)
(test (check-temps 5 95 (list 1))
      false)
(test (check-temps 5 95 (list 1 6))
      false)
(test (check-temps 5 95 (list 6 90))
      true)

;; convert : lon -> num
(define (convert l)
  (foldr (λ (f cr)
           (+ f (* 10 cr)))
         0
         l))

(test (convert empty) 0)
(test (convert (list 1 2 3)) 321)

;; sum : lon -> num
(define (sum l)
  (foldr + 0 l))

;; average-price : lon -> num
(define (average-price l)
  (define len (length l))
  (foldr (λ (x r) (+ (/ x len) r)) 0 l))

(test (average-price empty) 0)
(test (average-price (list 1)) 1)
(test (average-price (list 1 1)) 1)

;; convertFC : lon -> lon
(define (convertFC l)
  (map (λ (x) (- x 32)) l))

(test (convertFC empty) empty)
(test (convertFC (list 32)) (list 0))

;; suffixes : los -> lolos
(define (suffixes l)
  (cons l
        (cond [(empty? l)
               empty]
              [else
               (suffixes (rest l))])))

(test (suffixes empty) (list empty))
(test (suffixes (list 1 2))
      (list (list 1 2)
            (list   2)
            (list    )))

(struct unknown ())
(struct person (name age eye mom pop))

(define (tree-fold replace-person repace-unknown t)
  (cond [(unknown? t)
         repace-unknown]
        [else
         (replace-person
          (person-name t)
          (person-age t)
          (person-eye t)
          (tree-fold replace-person repace-unknown
                     (person-mom t))
          (tree-fold replace-person repace-unknown
                     (person-pop t)))]))

(define (count-persons t)
  (tree-fold (λ (n a e m p)
               (+ 1 m p))
             0
             t))

(define some-t
  (person "Jay" 27 "Beautiful blue"
          (person "Jim" 999 "Brown"
                  (unknown) (unknown))
          (person "Pammy" 999 "Brown"
                  (unknown) (unknown))))

(test (count-persons (unknown))
      0)
(test (count-persons some-t)
      3)

;; compose-func : (b -> c) (a -> b) -> (a -> c)
(define (compose-func after before)
  (λ (x) (after (before x))))

(test ((compose-func add1 sub1) 0)
      0)
(test ((compose-func string-length number->string)
       100000)
      6)


;; IGNORE
;; (define (compose-func-awesome after before)
;;   (λ args
;;     (call-with-values
;;         (λ () (apply before args))
;;       (λ before-results
;;         (apply after before-results)))))

;; flatten : lolos -> los
(define (flatten lolos)
  (foldr append empty lolos))

(test (flatten empty)
      empty)
(test (flatten (list empty empty))
      empty)
(test (flatten (list (list 1 2 3) (list 4 5 6)))
      (list 1 2 3 4 5 6))

;; bucket : lon -> lolon
(define (bucket lon)
  (foldr (λ (first-lon bucket-rest-lon)
           (cond [(empty? bucket-rest-lon)
                  (list (list first-lon))]
                 [(= first-lon
                     (first (first bucket-rest-lon)))
                  (cons (cons first-lon
                              (first bucket-rest-lon))
                        (rest bucket-rest-lon))]
                 [else
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
(test (bucket (list 2))
      (list (list 2)))
(test (bucket (list 1 2))
      (list (list 1)
            (list 2)))
