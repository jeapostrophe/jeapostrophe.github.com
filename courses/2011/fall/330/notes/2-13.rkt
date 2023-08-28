#lang lazy

;; Basics
+

;; Evaluate early = eager or call-by-value
;; Evaluate later, each time = stupid or call-by-name
;; Evaluate later, one time, only if necessary = lazy or call-by-need
;(with (x (+ 5 5))
;      (+ x x))
;(with (x (+ 5 5))
;      12)

5
(+ 1 1)

(define add-promise
  (let ([x (+ 5 5)])
    (+ x x)))
add-promise
(! add-promise)

;; Errors
(define div-promise
  (let ([x (/ 1 0)])
    (+ x x)))
div-promise

;; Effects
(define padd-promise
  (let ([x (begin (printf "You caught me working!\n")
                  (+ 5 5))])
    (+ x x)))
padd-promise
(printf "I am about to force it!\n")
(! padd-promise)

;; Filtering
(define l (list 0 1 2 3 (+ 2 2) 5 6 7 8 9))
l
(first l)
l

(define just-the-evens
  (filter even? l))
just-the-evens
(third just-the-evens)
just-the-evens
(second just-the-evens)
just-the-evens

;; Mapping
(define one-more
  (map add1 l))
one-more
(first one-more)
one-more
(list-ref one-more 4)

;; Ones
(define ones
  (cons 1
        ones))

(list-ref ones 0)
(list-ref ones 25)
(list-ref ones 1000)

;; Cycle
(define one-two-punch
  (cons 1
        (cons 2
              one-two-punch)))

(list-ref one-two-punch 0)
(list-ref one-two-punch 25)
(list-ref one-two-punch 1000)

(define (bi-cycle even-thing odd-thing)
  (cons even-thing
        (cons odd-thing
              (bi-cycle even-thing odd-thing))))

(define one-two-punch2
  (bi-cycle 1 2))

(list-ref one-two-punch2 0)
(list-ref one-two-punch2 25)
(list-ref one-two-punch2 1000)

;; Nats
(define nats
  (cons 0
        (map add1 nats)))

(take 25 nats)
(! (take 25 nats))
(!list (take 25 nats))
(!! (take 25 nats))

(first nats)
= (first (cons 0
               (map add1 nats)))
= 0

(rest nats)
= (rest (cons 0
              (map add1 nats)))
= (map add1 nats)

(first (rest nats))
= (first (map add1 nats))
= (first (cons (add1 (first nats))
               (map add1 (rest nats))))
= (add1 (first nats))
= (add1 0)
= 1

(first (rest (rest nats)))
= (first (rest (map add1 nats)))
= (first (rest (cons (add1 (first nats))
                     (map add1 (rest nats)))))
= (first (map add1 (rest nats)))
= (first (map add1 (map add1 nats)))

;; Pows2
(define pows2
  (map (lambda (n)
         (expt 2 n))
       nats))

(!! (take 25 pows2))

(define pow-again
  (cons 1
        (map (lambda (n) (* 2 n))
             pow-again)))

(!! (take 25 pow-again))

;; Fibs
(define fibs
  (cons 0
        (cons 1
              (map (lambda (x y)
                     (printf "Adding ~a and ~a\n" x y)
                     (+ x y))
                   fibs
                   (rest fibs)))))

(!! (take 10 fibs))

;; Find all strings of A-Zs forever
(define first-list-of-strings
  (list empty))

(define (put-all-the-letters-on-the-list l)
  (apply append
         (map
          (lambda (e)
            (build-list 26
                        (lambda (i)
                          (cons i e))))
          l)))

(!! first-list-of-strings)
(!! (put-all-the-letters-on-the-list first-list-of-strings))

(define list-of-seqs
  (cons first-list-of-strings
        (map put-all-the-letters-on-the-list
             list-of-seqs)))

(!! (second list-of-seqs))
  
;;(!! (third list-of-seqs))

(define (my-apply-append lol)
  (append (first lol)
          (my-apply-append (rest lol))))

(define all-of-them
  (my-apply-append list-of-seqs))

(!!list (take 200 all-of-them))


;; First collision
;;;(first (filter is-collision? all-of-them))

;; Shell scripts
