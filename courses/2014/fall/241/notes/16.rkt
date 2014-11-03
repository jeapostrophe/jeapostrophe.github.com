#lang racket/base

;; Chapter 11

;; 11.1
;; - An array
;; - Reasonable when U is small
;; - Think about boxing versus storing data in table

;; 11.1-1
;; - Think about what you can do to the representation

;; 11.1-2
;; - Use the logarithm.

(define (bitset-mt) 0)
(define (bitset-set? bv k)
  (not (zero? (bitwise-and bv (expt 2 k)))))
(define (bitset-add bv k)
  (bitwise-ior bv (arithmetic-shift 1 k)))

(bitset-add (bitset-add (bitset-add (bitset-mt) 0) 1) 2)
7 
#b111

;; 11.2
;; - Pidgey and Collisions
;; - What to do when colliding?
;; - Simple Uniform Hashing... is it reasonable?
;; - What does "n = O(m)" mean?

;; 11.2-1
;; - How many different "k,l"s are there?

(define (fac n)
  (if (zero? n)
      1
      (* n (fac (sub1 n)))))
(define (choose n k)
  (/ (fac n) (* (fac k) (fac (- n k)))))

(define (prob n m)
  (/ (choose n 2) m))

(real->decimal-string (prob 100 20))

;; (= x (modulo (+ (* y m) x) m))

;; 11.2-3
;; - Work through this... what does sorted do?

;; 11.2-4
;; - What does a free list do?

;; 11.2-6
;; - How can we think about all the keys in the table?

;; 11.3
;; - Why are primes useful?
;; - Why are powers of 2 bad?
;; - Why do we want the fractional part and why multiply by m after?
;; - Why the weird formula for A?

;; 11.3.3 (feel free to skip)
;; - Universal means that the number of times two keys collide is no
;; more than "average"
;; - The construction of the functions is cool and subtle

;; 11.3-1
;; - What is the cost of hashing?

;; 11.3-2
;; - What properties are true of modular addition?

;; 11.4
;; - Don't hash to one value, but to a permutation
;; - Wacky idea, huh?
;; - Why is uniform hashing so hard?
;; - Linear probing is the earlier exercise
;; - Quadratic's defining characteristic is same-ness
;; - Double is about a gap sequence

;; 11.4-1
;; - Practice your grasp of these hashing ideas
                                   
;; 11.4-3
;; - Practice your grasp of the analysis

;; 11.5 (skip)

