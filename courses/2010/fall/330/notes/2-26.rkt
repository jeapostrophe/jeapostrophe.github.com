#lang racket
; Basic continuations
(+ 3 4)

; let/cc = let with current continuation
(let/cc k
  (+ 3 4))

(let/cc k
  ; 1- display 7 twice
  ; 1- display 7 once
  ; 1- display 7, display void
  (k (+ 3 4)))
; The continuation is not just "print", but rather "print and stop"

(let/cc k
  ; 2- 4 and then error because + with one arg
  ; 1- 3
  ; 4- 7
  ; 1- infinite loop
  ; 1- 4
  (+ 3 (k 4)))

; Exceptions
(define (f x)
  (let/cc throw-exn
    (+ 1 
       (if (zero? x)
           (throw-exn "You are a bad person")
           (/ 1 x)))))

(f 2)
(f 0)

; Returns
(define (g x)
  (let/cc return
    (when (= x 1)
      (return 3))
    (+ x 1)))

(g 1)
(g 0)

; Producers & Consumers
(define (make-producer the-producer)
  (define resume (box #f))
  (define future-produce (box #f))
  (define (route real-produce)
    (define (produce val)
      (let/cc resume-point
        (set-box! resume resume-point)
        ((unbox future-produce) val)))
    (set-box! future-produce real-produce)
    (if (unbox resume)
        ((unbox resume) 'dummy)
        (the-producer produce)))
  route)

(define (consume prod)
  (let/cc k (prod k)))

;; Uses

(define route
  (make-producer
   (λ (produce)
     (begin
       (produce 'kirtland)
       (produce 'nauvoo)
       (produce 'slc)))))
(list (consume route)
      (consume route)
      (consume route))

(define ones
  (make-producer
   (λ (produce)
     (let loop ()
       (produce 1)
       (loop)))))

(define nats
  (make-producer
   (λ (produce)
     (let loop ([n 0])
       (produce n)
       (loop (add1 n))))))

(list (consume nats)
      (consume nats)
      (consume nats))

(define evens
  (make-producer
   (λ (produce)
     (let loop ([n 0])
       (produce n)
       (loop (+ 2 n))))))

(list (consume evens)
      (consume evens)
      (consume evens))

(define (list->producer l)
  (make-producer
   (λ (produce)
     (for-each produce l))))

(define lp (list->producer '(1 2 3 4)))
(list (consume lp)
      (consume lp)
      (consume lp))
