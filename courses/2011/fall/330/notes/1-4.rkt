;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 1-4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; sum : LoN -> number
; Purpose: to find the sum of alllll the numbers
#;(define (sum a-lon)
  (cond
    [(empty? a-lon)
     ; a-lon is emptinated
     0]
    [else
     ; a-lon is a cons
     ; (first a-lon) is a number
     ; (rest a-lon) is a LoN
     ; (f (rest a-lon)) for some f
     #;55
     
     ; fi = 999
     ; rest = 55:!
     ; sum(rest) = 55
     #;1054     
     (+ (first a-lon) (sum (rest a-lon)))]))
(define (my-foldr my-cons my-empty a-lon)
  (cond
    [(empty? a-lon)
     my-empty]
    [else
     (my-cons (first a-lon) (my-foldr my-cons my-empty (rest a-lon)))]))

(define (copy a-lon)
  (my-foldr cons empty a-lon))
(define (sum a-lon)
  (my-foldr + 0 a-lon))
(define (product a-lon)
  (my-foldr * 1 a-lon))

(check-expect (sum empty)
              0)
(check-expect (sum (cons 55 empty))
              55)
(check-expect (sum (cons 999 (cons 55 empty)))
              1054)
(check-expect (sum (cons 1 (cons 99 empty)))
              100)
(check-expect (sum (cons 1 (cons 99 (cons 1 (cons 99 (cons 1 (cons 99 empty)))))))
              300)

;; homomorphism

(cons 1 (cons 2 (cons 3 empty)))
;; sum>
(+    1 (+    2 (+    3     0)))
;; product>
(*    1 (*    2 (*    3     1)))


;; reverse homomorphism
(+ (+ (+ 0 1) 2) 3) ; foldl

#;(local [....] ....)

(define (weird+2 weird-osity)
  (local
    [(define (the-fun x y)
       (+ x y weird-osity))]
    the-fun))
(define (sum-and-then-some2 weird-osity a-lon)
  (my-foldr (weird+2 weird-osity) 0 a-lon))

(define (sum-and-then-some weird-osity a-lon)
  (local
    [(define (weird+ x y)
       (+ x y weird-osity))]
    (my-foldr weird+ 0 a-lon)))

(check-expect (sum-and-then-some 10 (cons 1 (cons 2 (cons 3 empty))))
              (+ 11 (+ 12 (+ 13 0))))
(check-expect (sum-and-then-some 99 (cons 1 (cons 2 (cons 3 empty))))
              (+ 100 (+ 101 (+ 102 0))))

; even-steven? : LoN -> LoB
#;(define (even-steven? a-lon)
  (cond
    [(empty? a-lon)
     empty]
    [else
     (cons
      (even? (first a-lon))
      (even-steven? (rest a-lon)))]))
(define (even-steven? a-lon)
  (map even? a-lon))
  
(even-steven? (cons 1 (cons 2 ( cons 3 empty))))

(cons 1 (cons 2 (cons 3 empty)))

(map number->string (cons 1 (cons 2 (cons 3 empty))))

(map string->number
     (map number->string 
          (cons 1 (cons 2 (cons 3 empty)))))

(cons (even? 1) (cons (even? 2) (cons (even? 3) empty)))

;; only-even : LoN -> LoN
(define (only-even a-lon)
  (filter even? a-lon))

(define (thing a-lon)
  (local
    [(define (? a)
       (<= a (first a-lon)))]
    (filter ? a-lon)))

#;(define (only-even a-lon)
  (cond
    [(empty? a-lon)
     empty]
    [else
     (if 
      ; Distinguish situation 0 from 1
      (not (even? (first a-lon)))
      
      ; Test 1
      ; a-lon = (cons 1 (cons 2 (cons 3 empty)))
      ; (first a-lon) = 1
      ; (rest a-lon) = (cons 2 (cons 3 empty))
      ; (only-even (rest a-lon)) = (cons 2 empty)
      (only-even (rest a-lon))
      
      ; Test 0
      ; a=lon = (cons 2 (cons 3 empty))
      ; first = 2
      ; rest = (cons 3 empty)
      ; (only-even rest) = empty
      ; ans = (cons 2 empty)
      (cons (first a-lon)
            (only-even (rest a-lon))))]))

(check-expect (only-even empty)
              empty)
(check-expect (only-even (cons 2 (cons 3 empty)))
              (cons 2 empty))
(check-expect (only-even (cons 1 (cons 2 (cons 3 empty))))
              (cons 2 empty))
(check-expect (only-even (cons 42 (cons 1 (cons 2 (cons 3 empty)))))
              (cons 42 (cons 2 empty)))

