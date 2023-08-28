;; Yo! Raps.

;; sort : list -> list
(define (sort l)
  (define-values (l1 l2)
    (split l))
  (merge (sort l1)
         (sort l2)))

;; sortK : list -> time
(define (sortK l)  
  (+ (splitK (length l))
     (sortK (/ (length l) 2))
     (sortK (/ (length l) 2))
     (mergeK (length l))))

;; x = 2x + 1


T(1) = Theta(1)
T(n) = Theta(1) + T(n/2) + T(n/2) + Theta(n) + Theta(1)
     = 2T(n/2) + Theta(n)

(kn)^lg7

kn x  n ---> A_0 ... A_k where A_i is nxn
n  x kn ---> B_0
             ...    where B_i is nxn
             B_k 

k^2 * n ^ lg7
