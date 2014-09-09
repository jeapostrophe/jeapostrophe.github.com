;; asymptotic notations

#lang racket
(exact->inexact
 (/ (* 97
       1000000
       1000000000000)
    (* 25 366
       1000000000)))

;; Let f .n/ and g.n/ be asymptotically nonnegative functions. Using
;; the basic defi- nition of ‚-notation, prove that max.f .n/; g.n// D
;; ‚.f .n/ C g.n//.

asnn(f) = \exists n_f . \forall n > n_f . f(n) > 0

f(n)
g(n)
h(n) = max( f(n), g(n) )

(define (h n)
  (max (f n) (g n)))

H1: asnn(f) ... n_f
H2: asnn(g) ... n_g
n_0 = max(n_f, n_g)
-----------
 h(n) = \theta(f(n) + g(n))
==>
\exists c_0, c_1, n_0 . \forall n > n_0
 c_0 * (f(n) + g(n)) <= h(n) <= c_1 * (f(n) + g(n))
==>
\exists c_0, c_1, n_0 . \forall n > n_0
 c_0 * (f(n) + g(n)) <= max( f(n), g(n) ) <= c_1 * (f(n) + g(n))
---
c_0 <= 1/2, c_1 >= 1

;;;;;;;;;;;;;;;;;;;

\theta(n^2)          .oO (lambda (n) (* n n))
\theta(f(n) + g(n))  .oO (lambda (n) (+ (f n) (g n)))

\theta : (nat -> nat) -> Prop
(define (\theta h)
  (exists c_0 c_1 n_0
          (forall n
                  (implies (n >= n_0)
                           ......))))


;;;;;;;;;;;; 3.1-2

\forall a, b .
 (n + a)^b = \theta(n^b)

means

\forall a, b .
\theta(n) = { f(n) | \exist c_1, c_2, n_0 . \forall n >= n_0 
              s.t. 0 <= c_1(n^b) <= (n+a)^b <= c_2(n^b) }

forall k, exists j, k = j^b
k = c_1, j = c_1'

c_1^b(n^b) <= (n+a)^b
(c_1*n)^b <= (n+a)^b
c_1*n <= n+a
c_1*n <= n - |a| <= n+a
c_1*n <= n - |a|

forall n m, n > m => exists i, n = m + i

c_1*(n_0 + i) <= (n_0 + i) - |a|
n_0 = 2|a|
c_1*(2|a| + i) <= (2|a| + i) - |a|
c_1*(2|a| + i) <= |a| + i
(1/2)*(2|a| + i) <= |a| + i
|a| + i/2 <= |a| + i
c_1 <= 1/2^b
