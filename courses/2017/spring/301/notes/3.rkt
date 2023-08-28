#lang racket/base
(require racket/set
         redex)

;; Lambda Calculus is a language with only functions
(define-language lc
  [(M N L)
   (λ (x) M)
   (M N)
   x]
  [(x y z)
   variable])

(define t-id
  (term (λ (x) x)))
(define t-idid
  (term ((λ (x) x)
         (λ (x) x))))
;; -> ((λ (x) x) (λ (x) x))
;; -> x [substitute (λ (x) x) for x]
;; -> (λ (x) x)

(define-metafunction lc
  [(free-vars (λ (x) M))
   ,(set-remove (term (free-vars M)) (term x))]
  [(free-vars (M N))
   ,(set-union (term (free-vars M)) (term (free-vars N)))]
  [(free-vars x)
   ,(set (term x))])

(module+ test
  (test-equal (term (free-vars ((λ (x) x) (λ (x) x)))) (set))
  (test-equal (term (free-vars ((λ (x) x) (λ (x) y)))) (set 'y)))

(define-metafunction lc
  subst : x M N -> L
  [(subst x M x)
   M]
  [(subst x M z)
   z]
  
  [(subst x M (λ (x) N))
   (λ (x) N)]
  [(subst x M (λ (y) N))
   (λ (z) (subst x M (subst y z N)))
   (where z ,(variable-not-in (term M) (term y)))]

  [(subst x M (N L))
   ((subst x M N)
    (subst x M L))])

(module+ test
  (test-equal (term (subst x y x))
              (term y))
  (test-equal (term (subst x y z))
              (term z))
  (test-equal (term (subst x y (λ (x) x)))
              (term (λ (x) x)))
  (test-equal (term (subst x y (λ (z) x)))
              (term (λ (z) y)))
  (test-equal (term (subst x y (λ (y) x)))
              (term (λ (y1) y))))

;; Reduction in the Lambda Calculus has three rules:

;;  alpha
;;;;;;      (λ (x) M)  alpha  (λ (y) (subst x y M))
;;;;;;         if y is not in (free-vars M)

;;  beta
;;;;;;      ((λ (x) M) N)    beta    (subst x N M)

;;  eta
;;;;;;      (λ (x) (M x))    eta     M
;;;;;;        if x is not in (free-vars M)

;; n (normal reductions of the LC) = union of alpha, beta, eta

(define beta
  (reduction-relation
   lc 
   [--> ((λ (x) M) N)
        (subst x N M)]))

(define n
  beta)

(define -->n
  (compatible-closure beta lc M))

(traces -->n
        (term (λ (x) x)))

(traces -->n
        (term ((λ (x) x)
               (λ (x) x))))

;; Booleans
;; In B, we chose T and F to mean true and false

(define  lc-true (term (λ (on-true) (λ (on-false) on-true))))
(define lc-false (term (λ (on-true) (λ (on-false) on-false))))
(define lc-if (term (λ (bool) (λ (on-true) (λ (on-false) ((bool on-true) on-false))))))

;; if  true M N =n M
(traces -->n (term (((,lc-if ,lc-true) em) en)))
;; if false M N =n N
(traces -->n (term (((,lc-if ,lc-false) em) en)))

;; Pairs
(define lc-pair (term (λ (f) (λ (s) (λ (sel) (((,lc-if sel) f) s))))))
(define lc-fst (term (λ (p) (p ,lc-true))))
(define lc-snd (term (λ (p) (p ,lc-false))))

;; fst (pair M N) = M
(traces -->n (term (,lc-fst ((,lc-pair em) en))))
;; snd (pair M N) = N
(traces -->n (term (,lc-snd ((,lc-pair em) en))))

;; Numbers
;;; Church Numeral

(define lc-zero (term (λ (f) (λ (x) x))))
(define  lc-one (term (λ (f) (λ (x) (f x)))))
(define  lc-two (term (λ (f) (λ (x) (f (f x))))))

(define lc-add1 (term (λ (n) (λ (f) (λ (x) (f ((n f) x)))))))
(define lc-add (term (λ (m) (λ (n) ((m ,lc-add1) n)))))
(define lc-iszero? (term (λ (n) ((n (λ (x) ,lc-false)) ,lc-true))))

(traces -->n (term (,lc-iszero? ((,lc-add ,lc-two) ,lc-one))))

;; In book, look at sub1
(define lc-wrap
  (term (λ (p) ((,lc-pair ,lc-false)
                (((,lc-if (,lc-fst p))
                  (,lc-snd p))
                 (f (,lc-snd p)))))))
(define lc-sub1
  (term (λ (n) (λ (f) (λ (x) ((,lc-snd (,lc-wrap f)) ((,lc-pair ,lc-true) x)))))))

(define lc-mult
  (term (λ (n)
          (λ (m)
            (((,lc-if (,lc-iszero? n))
              ,lc-zero)
             ((,lc-add m) ((mult (,lc-sub1 n)) m)))))))

(module+ test
(test-equal (term (free-vars ,lc-mult)) (set 'mult)))

(define lc-mkmult0
  (term (λ (mult)
          (λ (n)
            (λ (m)
              (((,lc-if (,lc-iszero? n))
                ,lc-zero)
               ((,lc-add m) ((mult (,lc-sub1 n)) m))))))))

(define lc-mkmult
  (term (λ (mkmult)
          (λ (n)
            (λ (m)
              (((,lc-if (,lc-iszero? n))
                ,lc-zero)
               ((,lc-add m) (((mkmult mkmult) (,lc-sub1 n)) m))))))))

;; mult = mkmult mkmult
(define lc-mult2
  (term (,lc-mkmult ,lc-mkmult))) 

(module+ test
  (test-equal (term (free-vars ,lc-mkmult)) (set)))

#;(traces -->n (term ((,lc-mult2 ,lc-two) ,lc-two)))

(define lc-mkmk
  (term (λ (k) (λ (t) (t ((k k) t))))))
(define lc-mk
  (term (,lc-mkmk ,lc-mkmk)))
;; mult = mk mkmult0

;; Theorem 3.1: M (mk M) =n (mk M) for any M

;; mk M = (mkmk mkmk) M
;;      = ((λ (k) (λ (t) (t ((k k) t)))) mkmk) M
;;    ->n (λ (t) (t ((mkmk mkmk) t))) M
;;    ->n (M ((mkmk mkmk) M))
;;      = (M (mk M))
;; thus, symmetry

;; mk discovers the fixed-point of the term M

;; The Y combinator:
(define lc-Y
  (term (λ (f)
          ((λ (x) (f (x x)))
           (λ (x) (f (x x)))))))

(define Omega
  (term ((λ (x) (x x))
         (λ (x) (x x)))))

;; Omega is a primitive INFINITE loop



(module+ test
  (test-results))
