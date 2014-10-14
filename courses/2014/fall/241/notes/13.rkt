#lang racket/base

;; 8.1-4
;; k! diff perm of each k-subseq

;; num of perms of the entire thing is NOT n!

;; (k!) * (k!) ... * (k!) (n/k) times
;; (k!)^(n/k) leafs in tree because perms of whole thing

;; h >= lg (k!)^(n/k)
;; h >= (n/k) lg (k!)
;;   >= (n/k) lg (k/2)^(k/2)
;;   >= (nk/k2) lg (k/2)
;;   >= (n/2) lg (k/2)
;;   >= n lg k

;; Sort : (A -> A -> Bool) (List A) -> (List A)

;; Stable
(define (stable <= l)
  (map cdr
       (sort
        (λ (x y)
          (if (= (cdr x) (cdr y))
              (<= (car x) (car y))
              (<= (cdr x) (cdr y))))
        (for/list ([i (in-naturals)]
                   [e (in-list l)])
          (cons i e)))))

;; N into base_k...

(define (convert-n-to-binary n)
  (if (zero? n)
      null
      (if (= 1 (modulo n 2))
          (cons 1 (convert-n-to-binary (quotient n 2)))
          (cons 0 (convert-n-to-binary (quotient n 2))))))

(reverse (convert-n-to-binary 7))
(reverse (convert-n-to-binary 8))

(define (convert-n-to-mary n m)
  (if (zero? n)
      null
      (cons (modulo n m) (convert-n-to-mary (quotient n m) m))))

(reverse (convert-n-to-mary 26 3))

(define (view-x-as-in-basen-and-get-pos x n i)
  (remainder x (expt n (add1 i))))

;; Radix Sort : (L : List a) (NumberOfPlaces : Num) (ExtractPlace : a -> Num -> Num)
(define (radix-sort l k ep)
  (for/fold ([l l])
            ([place (in-range k)])
    (sort (λ (x y) (<= (ep place x) (ep place y)))
          l)))


(view-x-as-in-basen-and-get-pos 7 2 2) "should be" 1
(view-x-as-in-basen-and-get-pos 7 2 1) "should be" 1
(view-x-as-in-basen-and-get-pos 7 2 0) "should be" 1


;; (<= (/ (Sum (j i (- (+ i k) 1)) (A j)) k)
;;     (/ (Sum (j (+ i 1) (+ i k)) (A j)) k))
;; (<= (Sum (j i (- (+ i k) 1)) (A j))
;;     (Sum (j (+ i 1) (+ i k)) (A j)))
;; (<= (- (Sum (j i (+ i k)) (A j)) (A (+ i k)))
;;     (Sum (j (+ i 1) (+ i k)) (A j)))
;; (<= (- (Sum (j i (+ i k)) (A j)) (A (+ i k)))
;;     (- (Sum (j i (+ i k)) (A j)) (A i)))
;; (<= (- (A (+ i k)))
;;     (- (A i)))
;; (>= (A (+ i k))
;;     (A i))
