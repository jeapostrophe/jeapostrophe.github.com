#lang lazy

(define (sq x)
  (printf "Called with an x\n")
  (* x x))
(sq (+ 2 3)) ;; <-- What is this?
(sq 5)
(* 5 5)
25

(* (+ 2 3) (+ 2 3))

;; Conceptually... no
;; What are side-effects?
;; Will there be more work?

(define (f x)
  (λ (y) (+ x y)))
;; ((f 3) (+ x 4)) ;; <-- Is this okay?

;; (+ 3 (+ 3 4)) ;; <- Should this work or always be bad?

;; (let ([x 5])
;;   ((f 3) x)) ;; <-- What's going on here?

;; Why do we need to compute?
;; --> Strictness point

(define (g x)
  42)
(g (sleep 100))
(g (/ 1 0))
(g ((λ (x) (x x))
    (λ (x) (x x))))
(g (printf "Hey there!\n"))

;; printf promise

(sq (begin (printf "Evaluating...\n")
           5))

;; lists of promises

(define l1
  (list (/ 1 0)
        (+ 5 5)
        ((λ (x) (x x))
         (λ (x) (x x)))
        (string-append "nerd" "fighter")))
l1
(second l1)
l1
(fourth l1)
l1

;; promises of lists

(define l2
  (append (list (+ 0 1) 2)
          (list 2 3)))
l2
;;(first l2)
l2
(second l2)
l2
(third l2)
l2

;; filter vs map

(define l3 (map add1 (list 1 2 3 4 5)))
l3
(third l3)
l3

(define l4 (filter odd? (list 1 2 3 4 5)))
l4
(second l4)
l4
(third l4)
l4

;; ones

;; ones = 1 : 1 : 1 : 1 ....
;; first ones = 1
;; rest ones = 1 : 1 : 1 : 1 ...

(define ones (cons 1 ones))
(list-ref ones 1742)

(list-ref (map add1 ones) 800)
(first (filter odd? ones))
;; (first (filter even? ones)) ;; <-- infinite


;; cycle

(define (cycle v)
  (cons v (cycle v)))
(list-ref (cycle 42) 600)

;; bicycle

(define (bicycle v1 v2)
  (cons v1 (cons v2 (bicycle v2 v1))))
(list-ref (bicycle 0 1) 600)

;; nats

;;       nats = 0 : 1 : 2 : 3 : 4 : ...
;; first nats = 0
;;  rest nats = 1 : 2 : 3 : 4 : 5 : ...

(define nats (cons 0 (map add1 nats)))
(list-ref nats 600)

(define (make-snats v)
  (cons v (make-snats (add1 v))))
(define snats (make-snats 0))
(list-ref snats 600)

;; fibs

;;      first fibs = 1
;; first rest fibs = 1
;;            fibs = 1 : 1 : 2 : 3 : 5 : 8 : ....
;;                   +   +   +   +
;;       rest fibs = 1 : 2 : 3 : 5 : 8 : ....
;;                   =   =   =   =
;;  rest rest fibs = 2 : 3 : 5 : 8 : ....

(define fibs 
  (cons 1 
        (cons 1 
              (map + fibs (rest fibs)))))

(!!list (take 10 fibs))
(list-ref fibs 200)

(/ (list-ref fibs 100)
   (list-ref fibs 200))

;; Laziness and effects

;; ((cps x)
;;  (λ (x)
;;    ... uses x ....))

;; Monads

;; A monoid in the category of endo-functors

;; return : A -> M A
(define (list-monad-return x)
  (list x))
(define (list-monad-return2 x y)
  (list x y))

;;   bind : M A -> (A -> M B) -> M B
(define (list-monad-bind mx f)
  (append-map f mx))
