;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 1-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; conkeror

;; sum : list of numbers -> number
;; compute the sum of all the numbers
(define (sum l)
  (cond
    [(empty? l)
     0]
    [else
     (+ (first l)
        (sum (rest l)))]))

(check-expect (sum empty) 0)
(check-expect (sum (cons 1 empty)) 1)
(check-expect (sum (cons 88 (cons 1 empty))) 89)

;; catamorphism

;; alt + left/right moves over ()s
;; hold shift to select
(check-expect (sum (cons 88 (cons 1 empty)))
                   (   + 88 (   + 1     0)))

(define (sum/cata l)
  (foldr ;; foldr is Racketese for "make me a catamorphism"
   + 0
   l))
  
(check-expect (sum/cata empty) 0)
(check-expect (sum/cata (cons 1 empty)) 1)
(check-expect (sum/cata (cons 88 (cons 1 empty))) 89)

;; Google MapReduce

(define (product l)
  (foldr * 1 l))

(check-expect (product empty) 1)
(check-expect (product (cons 1 empty)) 1)
(check-expect (product (cons 88 (cons 1 empty))) 88)

;; replace-cons is higher order argument
;; my-foldr :: (a b -> b) b (listof a) -> b
(define (my-foldr replace-cons replace-empty l)
  (cond 
    [(empty? l) replace-empty]
    [else 
     (replace-cons (first l) 
                   (my-foldr replace-cons replace-empty
                             (rest l)))]))

(define (copy l)
  (my-foldr cons empty l))

(check-expect (copy empty)
              empty)
(check-expect (copy (cons 1 empty))
              (cons 1 empty))
(check-expect (copy (cons 88 (cons 1 empty)))
              (cons 88 (cons 1 empty)))

(define (find-length-and-sum str num)
  (+ (string-length str) num))
(define (sum-lengths l)
  (foldr find-length-and-sum 0 l))

(check-expect (sum-lengths (list "Jay" "McCarthy"))
              11)

(define (even-steven? l)
  (filter ;; MAKE-ME-a-function-like-even-steven?
   even?
   l)
  #;(cond
    [(empty? l)
     empty]
    [else
     (cond [(even? (first l))
            (cons (first l)
                  (even-steven? (rest l)))]
           [else
            (even-steven? (rest l))])]))

(check-expect (even-steven? (list 1 2 3 5 7 11))
              (list 2))

(define (my-filter ? l)
  (cond
    [(empty? l)
     empty]
    [(? (first l))
     (cons (first l)
           (my-filter ? (rest l)))]
    [else
     (my-filter ? (rest l))]))

(define (odd-mcgobb? l)
  (my-filter ;; MAKE-ME-a-function-like-even-steven?
   odd?
   l))

(check-expect (odd-mcgobb? (list 1 2 3 5 7 11))
              (list 1 3 5 7 11))

;; filter : (a -> bool) (listof a) -> (listof a)

(define (multiply-by-2 x)
  (* 2 x))

;; map : (a -> b) (listof a) -> (listof b)

(define (evenator l)
  (map
   multiply-by-2
   l)
  #;
  (cond
    [(empty? l)
     empty]
    [else
     (cons (* 2 (first l))
           (evenator (rest l)))]))

;; mapping functions

(check-expect (evenator (list 1 2 3 4 5))
              (list 2 4 6 8 10))
  
(check-expect (map number->string (list 1 2 3 4 5))
              (list "1" "2" "3" "4" "5"))

(define (nenator n l)
  (local [(define (multiply-by-n x)
            (* n x))]
    (map multiply-by-n l)))

(check-expect (nenator 2 (list 1 2 3 4 5))
              (list 2 4 6 8 10))

(define (mind-blowing-nenator n l)
  (map (λ #;lambda (x) (* n x)) l))

(check-expect (mind-blowing-nenator 2 (list 1 2 3 4 5))
              (list 2 4 6 8 10))
