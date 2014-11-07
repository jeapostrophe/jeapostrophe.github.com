#lang racket/base
(require racket/match
         racket/list)

;; Chapter 15 - Dynamic Programming
;; - Divide and conquer is for separate work
;; - DP is for duplicated work

;; (My examples use memoization)

;; Simple example with Fibonacci

(define COUNT 0)
(define (count!)
  (set! COUNT (+ 1 COUNT)))
(define (reset-count!)
  (set! COUNT 0))

(define (slow-fib n)
  (count!)
  (if (< n 2)
      1
      (+ (slow-fib (- n 1))
         (slow-fib (- n 2)))))

(module+ test
  (define N 30)
  (reset-count!)
  (slow-fib N)
  (printf "Slow was ~a steps.\n" COUNT))

(define FIB-TABLE (make-hasheq))
(define (fast-fib n)
  (hash-ref! FIB-TABLE n
             (λ ()
               (count!)
               (if (< n 2)
                   1
                   (+ (fast-fib (- n 1))
                      (fast-fib (- n 2)))))))

(module+ test
  (reset-count!)
  (fast-fib N)
  (printf "Fast was ~a steps.\n" COUNT))

(require racket/stream)
(define (stream-map2 f x y)
  (stream-cons
   (f (stream-first x) (stream-first y))
   (stream-map2 f (stream-rest x) (stream-rest y))))

(define (count+ x y)
  (count!)
  (+ x y))
(define FIBS
  (stream-cons 1 (stream-cons 1 (stream-map2 count+ FIBS (stream-rest FIBS)))))
;;           FIBS = 1 1 2 3 5 8
;;      REST FIBS = 1 2 3 5 8
;; REST REST FIBS = 2 3 5 8 
(define (awesome-fib n)
  (stream-ref FIBS n))

(module+ test
  (reset-count!)
  (awesome-fib N)
  (printf "Awesome was ~a steps.\n" COUNT))

(define-syntax-rule (define/memo (f n) body)
  (define f
    (let ([TABLE (make-hasheq)])
      (λ (n)
        (hash-ref! TABLE n (λ () body))))))

;; 15.1
;; - Is this a real problem? Can you think of something like it?

;; Here is some code that is the example:

(struct plan (value how) #:transparent)
(define (rod-cutting p n)
  (define/memo (inner n)
    (if (zero? n)
        (plan (p 0) empty)
        (argmax plan-value
                (for/list ([i (in-range 1 (add1 n))])
                  (match-define (plan value how) (inner (- n i)))
                  (plan (+ (p i) value)
                        (cons i how))))))
  (inner n))

(module+ test
  (define VALUES (vector 0 1 5 8 9 10 17 17 20 24 30))
  (define (example-p n)
    (if (< n (vector-length VALUES))
        (vector-ref VALUES n)
        -inf.0))
  (for ([i (in-range 15)])
    (printf "~a = ~a\n"
            i
            (rod-cutting example-p i))))

;; The O(2^n) -> O(n^2) is THE BIG DEAL
;; The time-memory trade-off is THE BIG DEAL in this chapter.

;; 15.1-1
;; - Substitution

;; 15.1-5
;; - Don't look at my code!

;; 15.2
;; - This is a VERY important use case.
;; - How is P(n) different than rod-cutting?

;; 15.2-1
;; - Practice the algorithm and make sure you "get" it

;; 15.2-3
;; - Substitution

;; 15.3
;; - This section is the most important.
;; - In particular, the counter-example to opitmal substructure
;; - The idea of independent sub-problems
;; - Costs of bottom-up vs top-down

;; 15.3-2
;; - Duplicated sub-problems

;; 15.3-4
;; - Understand the matrix algorithm
;; - Make a VERY small example :)

;; 15.4
;; - Classic problem in bioinformatics
;; - This is just practice with the idea, so feel free to skim the
;; surface of the problem and just look at how to reinforce your grasp
;; of the DP ideas.

;; 15.4-4
;; - Which elements do you look at and in what order?
;; - This is a big reason to not use top-down

;; 15.5
;; - How cute.
;; - Another example AND a fun problem

;; 15.5-2
;; - Run the algorithm by hand or work it out yourself
