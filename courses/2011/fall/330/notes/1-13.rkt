#lang lazy

;; Basics
4
(+ 1 1)
(define plus-promise
  (let ([x (+ 5 5)])
    (+ x x)))
plus-promise
(! plus-promise)

;; Errors
(define div-promise
  (let ([x (/ 1 0)])
    (+ x x)))
div-promise
;(! div-promise)

;; Effects
(define print-promise
  (let ([x (begin (printf "You caught me!!! uggghhhh\n")
                  (+ 5 5))])
    (+ x x)))
print-promise
(printf "Print promise defined!\n")
(! print-promise)
(! print-promise)

;; Filtering
(define l (list (- 2 1) (* 1 2) (- 4 1) 4 5 6 7 8 (* 3 3)))
l
(define just-the-evens (filter even? l))
just-the-evens
(first just-the-evens)
just-the-evens
l

;; Mapping
(define add1-l (map add1 l))
add1-l
(first add1-l)
add1-l
(second add1-l)
add1-l
(third add1-l)
add1-l
(fourth add1-l)
add1-l

;; Ones
(define ones
  (cons 1
        ones))

(first ones)
(second ones)
(third ones)
(list-ref ones 100)
(list-ref ones (random 1000))
ones

;; Cycle
(define (bi-cycle a b)
  (cons a
        (cons b
              (bi-cycle a b))))

(define one-two-punch
  (bi-cycle 1 2))

(first one-two-punch)
(second one-two-punch)
(third one-two-punch)
(list-ref one-two-punch 100)

;; There are none
;;(filter even? ones)

;; Nats
(define nats
  (cons 0
        (map add1 nats)))

(first nats)
(second nats)
(take 5 nats)
(! (take 5 nats))
(!list (take 5 nats))
(!! (take 5 nats))

(rest nats)
= (map add1 nats)
= (cons (add1 (first nats)) (map add1 (rest nats)))
= (cons (add1 0) (map add1 (rest nats)))
= (cons 1 (map add1 (rest nats)))
= (cons 1 (map add1 (cons 1 (map add1 (rest nats)))))
= (cons 1 (cons (add1 1) (map add1 (map add1 (rest nats)))))
= (cons 1 (cons 2 (map add1 (map add1 (rest nats)))))

;; Pows2
(define pows2
  (map (lambda (n)
         (expt 2 n))
       nats))

(first pows2)
(!! (take 10 pows2))

;; Fibs
(define fibs
  (cons 0
        (cons 1
              (map (lambda (x y) ; Eta-expansion of +
                     (printf "Adding ~a and ~a\n" x y)
                     (+ x y))
                   fibs
                   (rest fibs)))))

(!! (take 10 fibs))

;; Shell scripts
