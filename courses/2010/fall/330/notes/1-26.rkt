#lang racket

; Basic continuations
(+ 3 4)

; let/cc -> let current continuation
(let/cc k
  (+ 3 4))

(let/cc k
  (k (+ 3 4)))

(let/cc k
  ; 1- "4" "7"
  ; 2- "4" "procedure"
  ; 4- "4" error
  ; 1- "4"
  (+ 3 (k 4)))

; Exceptions
(define (f throw x)
  (if (zero? x)
      (throw "you're a bad man")
      (/ 1 x)))
(f display 0)

(define (g y)
  (let/cc esc
    (define (throw x) (display x) (esc #f))
    (+ (f throw y) (f throw (- y 2)))))
(g 2)
(g 0)

; Producers & Consumers
(define (make-producer the-producer)
  (define resume (box #f))
  (define actual-produce (box #f))
  (define (route real-produce)
    (define (produce v)
      (let/cc resume-point
        (set-box! resume resume-point)
        ((unbox actual-produce) v)))
    (set-box! actual-produce real-produce)
    (if (unbox resume)
        ((unbox resume) 'dummy)
        (the-producer produce)))
  route)

(define (consume producer)
  (let/cc k (producer k)))

;;

(define route
  (make-producer
   (λ (produce)
     (produce 'kirtland)
     (produce 'nauvoo)
     (produce 'slc))))

(list (consume route)
      (consume route)
      (consume route))

(define ones
  (make-producer
   (λ (produce)
     (let loop ()
       (produce 1)
       (loop)))))

(define evens
  (make-producer
   (λ (produce)
     (let loop ([n 0])
       (produce n)
       (loop (+ n 2))))))

(define (list->producer l)
  (make-producer
   (λ (produce)
     (for-each produce l)
     #;(let loop ([l l])
       (unless (empty? l)
         (produce (first l))
         (loop (rest l)))))))

(define xs (list->producer (list 1 2 3)))
(consume xs)
(consume xs)
(consume xs)

