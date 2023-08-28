#lang plai
(print-only-errors #t)
(halt-on-errors #t)

;; A list is either an empty or (cons element list)

(define l1 empty)
(define lRon (cons 1 empty))
(define l2 (cons 2 lRon))
(define l3 (cons 4 (cons 2 empty)))
(define long-list
  (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 empty)))))))
(define long-list-again
  (list 2       2       2       2       2       2))

;; jlength : list -> num
;; Purpose: find the number of elements in the list
(define (jlength l)
  (cond
    [(empty? l)
     0]
    [(cons? l)
     ;; l = lRon, first l = 1, (jlength (rest l)) = 0
     ;; .... (first l) .... (jlength (rest l)) ....
     ;; 1
     ;; l = l2, first l = 2, jlength rest l = 1
     ;; (first l)
     ;; l = l3, first l = 4, jlength rest l = 1
     (add1 (jlength (rest l)))]))

(test (jlength l1) 0)
(test (jlength lRon) 1)
(test (jlength l2) 2)
(test (jlength l3) 2)
(test (jlength long-list-again) 6)
(test (jlength long-list) 6)

;; sum : list -> num
;; computes the total of all the stuff in the list
(define (sum l)
  (cond
    [(empty? l)
     0]
    [(cons? l)
     ;; ... (first l) ... (sum (rest l)) ...
     ;; l = [1] first = 1 sumrest = 0
     ;; (first l)
     ;; l =[10, 10, 10] first = 10 rest = [10,10] sumrest = 20
     (+ (first l) (sum (rest l)))]))

(test (sum empty) 0)
(test (sum (cons 1 empty)) 1)
(test (sum (cons 10 empty)) 10)
(test (sum (cons 10 (cons 10 (cons 10 empty)))) 30)

;; homomorphism

(test (sum (cons 10 (cons 10 (cons 10 empty)))) 30)
(test      (+    10 (sum (cons 10 (cons 10 empty)))) 30)
(test      (+    10 (+    10 (sum (cons 10 empty)))) 30)
(test      (+    10 (+    10 (+    10 (sum empty)))) 30)
(test      (+    10 (+    10 (+    10     0))) 30)
;; sum... turned the empty into 0 ... turned the cons into +

;; generalize-sum : (a b -> b) b (listof a) -> b
(define (generalize-sum turn-cons-into turn-empty-into l)
  (cond
    [(empty? l)
     turn-empty-into]
    [(cons? l)
     (turn-cons-into
      (first l)
      (generalize-sum turn-cons-into turn-empty-into
                      (rest l)))]))

;; generalize-sum = foldr / reduce in py

(define (osum l)
  (generalize-sum + 0 l))

(test (osum empty) 0)
(test (osum (cons 1 empty)) 1)
(test (osum (cons 10 empty)) 10)
(test (osum (cons 10 (cons 10 (cons 10 empty)))) 30)

(define (olength l)
  (define (add1-to-right-and-ignore-left x y)
    (add1 y))
  (define (f x y)
    (add1 y))
  (generalize-sum ;; (λ (x y) (add1 y))
   (lambda (x y) (add1 y))
   0 l))

(test (olength empty) 0)
(test (olength (cons 1 empty)) 1)
(test (olength (cons 10 empty)) 1)
(test (olength (cons 10 (cons 10 (cons 10 empty)))) 3)

;; absolutely : list num -> list num
;; finds the absolute of each number
(define (absolutely l)
  (cond
    [(empty? l)
     empty]
    [(cons? l)
     (cons (abs (first l))
           (absolutely (rest l)))]))

(test (absolutely empty) empty)
(test (absolutely (cons 1 empty)) (cons 1 empty))
(test (absolutely (cons -15 empty)) (cons 15 empty))
(test (absolutely (cons -15 (cons 15 (cons -5 empty))))
      (cons 15 (cons 15 (cons 5 empty))))

(define (generalized-absolutely f l)
  #;
  (cond
  [(empty? l)
  empty]
  [(cons? l)
  (cons (f (first l))
  (generalized-absolutely f (rest l)))])
  (generalize-sum
   (λ (element answer-on-the-rest)
     (cons (f element)
           answer-on-the-rest))
   empty l))

;; (generalized-absolutely f (list x_0 ... x_n))
;; = (list (f x_0) ... (f x_n))

;; generalized-absolutely = map

(test (generalized-absolutely abs empty) empty)
(test (generalized-absolutely abs (cons 1 empty)) (cons 1 empty))
(test (generalized-absolutely abs (cons -15 empty)) (cons 15 empty))
(test (generalized-absolutely abs (cons -15 (cons 15 (cons -5 empty))))
      (cons 15 (cons 15 (cons 5 empty))))

;; (op a (op b c)) == (op (op a b) c)


;; filter

(define (filter ? l)
  (generalize-sum
   (λ (element answer-on-the-rest)
     (if (? element)
       (cons element
             answer-on-the-rest)
       answer-on-the-rest))
   empty l))

(test (filter positive? (list -4 10 -10 0)) (list 10))
