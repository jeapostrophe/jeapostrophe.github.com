#lang racket
;; heap = O(1)
;; bbst = O(lg N)
;; list = O(N)
(define (extract-min l)
  (for/fold ([m +inf.0])
            ([e (in-list l)])
    (min m e)))

(extract-min '(5 7 2 0 4 -17 29))

;; Peano numbers define the naturals as...
;; N = 0 | +1 N (S N)

(struct Pn:Z ())
(struct Pn:S (n))

(define (convert p)
  (match p
    [(Pn:Z)
     0]
    [(Pn:S n) 
     (add1 (convert n))]))

(convert (Pn:S (Pn:S (Pn:S (Pn:Z)))))

(struct Bn:MT ())
(struct Bn:0 (n))
(struct Bn:1 (n))

(define (Bn-convert b)
  (match b
    [(Bn:MT)
     0]
    [(Bn:0 n)
     (* 2 (Bn-convert n))]
    [(Bn:1 n)
     (add1 (* 2 (Bn-convert n)))]))

(Bn-convert (Bn:1 (Bn:1 (Bn:0 (Bn:MT)))))
(Bn-convert (Bn:0 (Bn:1 (Bn:1 (Bn:0 (Bn:MT))))))

(define Bn-mult2 Bn:0)
(define (Bn-div2&floor b)
  (match b
    [(Bn:MT)
     0]
    [(Bn:0 n)
     n]
    [(Bn:1 n)
     n]))

(Bn-convert (Bn-div2&floor (Bn:0 (Bn:1 (Bn:1 (Bn:0 (Bn:MT)))))))

;; Sigma goblegoock = 2^{h+1}-1

;; Forall i
;;  LEFT(LEFT(I)) > RIGHT(i)
;;  2*2*i > 2*i+1

;; 2*2*1 > 2*1+1 
;; 4 > 3

;; 2*2*i > 2*i+1
;; 2*2*(i+1) > 2*(i+1)+1
;; 4i+4 > 2i + 2 + 1
;; 4i+4 > 2i + 3

;; Min is 2^{h}
;; Max is 2^{h+1}-1

;; P(i) = forall j, i < j -> Val(j) < Val(i)

;; P(n) = forall j, n < j -> Val(j) < Val(i)
;;      = trivial true because n < j implies j is not in the array

;; This is bad because...

;; P(h) = forall n, n is in layer h+i (for some i) of the tree, Val(n)
;; < Val(root of tree of depth h)

;;;;;;;


;;; Sorted means that (i <= j -> Val(i) <= Val(j))

Val(Parent(i)) <= Val(i)
Val(floor(i/2)) <= Val(i)
Apply Sorted
floor(i/2) <= i

i = 0 ~~~~> floor(0/2) <= 0
i = n+1 ~~~~> floor((n+1)/2) <= n+1

;;;;

If something were after the middle, then it's LEFT is outside of the array
2*(floor(N/2) + i)
N + 2i

;;;

\Sigma_{i=0}^{\inf} i * x^i

;; Next time: compare Pn-mult2 and Bn-add1
