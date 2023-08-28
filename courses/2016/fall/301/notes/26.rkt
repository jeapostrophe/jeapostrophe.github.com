#lang lazy

(define (sq x)
  (printf "I am about to square\n")
  (* x x))

(sq 4)

(sq (begin (printf "I am about to evaluate 4\n")
           4))

(define (ig x)
  (if (zero? (random 2))
    42
    x))

(ig (sq (begin (printf "I am about to evaluate 4\n")
               4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define l1
  (list 1 2 3 4))

l1
(second l1)
l1

(define l2
  (list (sq 1) (sq 2) (sq 3) (sq 4)))

l2
;; second : list? -> any
(second l2)
l2

(define t1
  (cons
   (cons (cons (sq 1) (sq 2))
         (cons (sq 3) (sq 4)))
   (cons (cons (sq 5) (sq 6))
         (cons (sq 7) (sq 8)))))

t1
(car (cdr (car t1)))
t1

;;;

(define l3
  (list (/ 1 0)
        (+ 5 5)
        ((λ (x) (x x))
         (λ (x) (x x)))))

l3
(second l3)
l3

;;;;;;

;; ones = 1 :: ones
(define ones
  (cons 1 ones))

(list-ref ones 1742)
(list-ref ones 1000000)

(define (cycle v)
  (cons v (cycle v)))

(define twos (cycle 2))
(list-ref twos 2000)

(define (bicycle v1 v2)
  (cons v1 (bicycle v2 v1)))

(define one-twos (bicycle 3 4))
(list-ref one-twos 2000)
(list-ref one-twos 2001)

;; naturals = 0 1 2 3 4 5 6 7 8 9

;; (nan n) = n :: (nan n+1)
(define (naturals-after-n n)
  (cons n (naturals-after-n (+ 1 n))))

;; nat = (nan 0)
(define naturals
  (naturals-after-n 0))
(list-ref naturals 6000)

;;      nats = 0 1 2 3 4 5
;; rest nats = add1 to every nat
;; rest nats = 1 2 3 4 5 6
(define nats
  (cons 0 (map add1 nats)))
(list-ref nats 17)
nats

(define nats-as-strs
  (map number->string nats))

(list-ref nats-as-strs 8)
nats-as-strs

;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;   fibs = 1 1 2 3 5  8 13 21
;;  rfibs = 1 2 3 5 8 13 21
;; rrfibs = 2 3 5 8 13 21

(define (map2 f l1 l2)
  (cons (f (first l1)
           (first l2))
        (map2 f
              (rest l1)
              (rest l2))))

(define fibs
  (cons 1
        (cons 1
              (map2 + fibs (rest fibs)))))

(list-ref fibs 200)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(some-f some-expr)
;;  A: Look at the definition of some-f and everything it calls
;;  Q: Is some-expr run?
;;  Y: some-expr launches the missile
;; Q': Did the missles get launched?

#;(some-f some-expr-that-loads-the-hopper-with-bunnies
          some-expr-that-loads-the-hopper-with-grenades
          some-expr-that-launches-the-hopper)

;; a strictness point (notate with !)
;; application is s.p.
;; if is s.p.

;; One type-based analysis of s.p. is called a "monad"

;; A monad is a monoid in the category of endofunctors

;; A monad M is a parameterize type (just like List)
;;   M A

;; return : (a:A)
;;       -> M A

;;   bind : (m:M A)
;;          (f:A -> M B)
;;       -> M B

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; let/cc k = talk about space

;; GHC = Glasgow Haskell Compiler

;; GHC : Haskell :: Racket : "Scheme"

(display "EOF\n\n\n\n\n\n\n\n\n\n\n\n")
