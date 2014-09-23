#lang racket

(1/6)*(1/5)*(1/4)*(1/3)*(1/2)*(1/1)
=
(1/6*5*4*3*2*1)
=
(1/6!)

....

5.2-2

;; Get not the best 1st:

\Sigma_{i=1}^{n-1} (1/n) = (n-1)/n
(n-1/n) call her "i"

;; See k people that aren't best and aren't better than "i"
? ? ? ?
;; Then bam! best one
MAX
;; See (n-k-2) people that could be better than "i"
? ?

;; Of all the ones better than i, MAX must come first
;; There are (n-i) better than i, MAX comes first 1/(n-i)

(a*b + a*c) = a*(b + c)

\Sigma_{i=1}^{n-1} (1/n) (1 / n - i)
(1/n) \Sigma_{i=1}^{n-1} (1 / n - i)
(1/n) \Sigma_{i=1}^{n-1} (1 / i)
;; Harmonic Number (n-1)
(1/n) \ln (n-1)

(1/n-1) + (1/n-2) + ... (1/n-(n-1)) = (1/1)

3 4 5 2 1

i = 2
count(3 4 5) = 3 = 5 - 2
i = 3
5 - 3 = 2 = count(4 5)

;; 5.2-5

X_ij for when A[i] and A[j] is an inversion
Pr{X_ij} = 1/2

i = 0, j = 1
i = 0, j = 2
i = 1, j = 2

NNN 123 0
NNY 132 1
YNN 213 1
NYY 231 2
YYN 312 2
YYY 321 3

9 total, 6 permutations, so 9/6 = 3/2

(for (i=1 to n)
  (for (j=i+1 to n)
    X_ij))

(for (i=1 to n)
  (for (j=i+1 to n)
    1))*X_ij

(choose n 2) (1/2)
(/ n! (* ((- n 2)!) 2)) (1/2)
(/ (* n (- n 1)) (* 2)) (1/2)
(/ (* n (- n 1)) 4)

(/ 6 4) = (3/2)


