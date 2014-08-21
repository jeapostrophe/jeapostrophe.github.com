;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 2-4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; sum : LoN -> number
; Purpose: to compute the sum of allll the numbers
#;(define (sum a-lon)
  (cond 
    [(empty? a-lon)
     ; know that a-lon is empty
     0]
    [else
     ; know that a-lon is a cons
     ; know that (first a-lon) is number
     ;  that means I can use: +, -, *, zero?
     ; know that (rest a-lon) is LoN
     ;  that means I can use: sum   
     
     ; first = 1
     ;  (add1 first) = 2, (even? first) = #f
     ; rest = mt
     ;  (sum rest) = 0
     #;1
     
     ; first = 45
     ; rest = mt
     ;  (sum rest) = 0
     #;45
     
     #;(first a-lon)
     
     ; first = 99
     ; rest = 45 -> mt
     ;  (sum rest) = 45
     (+ (first a-lon)
        (sum (rest a-lon)))]))

(define (my-foldr plus zero a-lon)
  (cond 
    [(empty? a-lon)
     zero]
    [else
     (plus (first a-lon)
           (my-foldr plus zero (rest a-lon)))]))

(define (sum a-lon)
  (my-foldr + 0 a-lon))

(cons 1 (cons 2 (cons 3 empty)))
; sum =>
(+    1 (+    2 (+    3     0)))
; product =>
(*    1 (*    2 (*    3     1)))
;; Homomorphism


;; foldr
(+    1 (+    2 (+    3     0)))
;; foldl
(+ (+ (+ 0 1) 2) 3)

(check-expect (sum empty)
              0)
(check-expect (sum (cons 1 empty))
              1)
(check-expect (sum (cons 45 empty))
              45)
(check-expect (sum (cons 99 (cons 45 empty)))
              144)
(check-expect (sum (cons 99 (cons 1 empty)))
              100)
(check-expect (sum (cons 99 (cons 1 (cons 99 (cons 1 empty)))))
              200)

#;(local [(define ...) ...] ...)

#;(define (sum-and-then-some weirdosity a-lon)
  (local
    [(define (weird+ x y)
       (+ x y weirdosity))]
    (my-foldr weird+ 0 a-lon)))

(define (sum-and-then-some weirdosity a-lon)
  (my-foldr (lambda (x y)
              (+ x y weirdosity))
            0 a-lon))

(check-expect (sum-and-then-some 10 (list 1 2 3))
              (+ 11 (+ 12 (+ 13 0))))
(check-expect (sum-and-then-some 100 (list 1 2 3))
              (+ 101 (+ 102 (+ 103 0))))

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

(list 1 2 3)
(list (even? 1) (even? 2) (even? 3))

(map add1 (list 1 2 3))

(map number->string (list 1 2 3))

(map string->number
     (map number->string 
          (list 1 2 3)))
     
(check-expect (even-steven? (cons 99 (cons 1 (cons 99 (cons 1 (cons 2 empty))))))
              (cons false (cons false (cons false (cons false (cons true empty))))))

; only-even : LoN -> LoN
#;(define (only-even a-lon)
  (cond
    [(empty? a-lon)
     empty]
    [else
     (cond
       [(even? (first a-lon))
        ; Test 1
        #;(check-expect (only-even (cons 2 empty))
                        (cons 2 empty))
        ; a-lon = 2 : !
        ; first = 2
        ; rest = !
        ; (only-even rest) = !
        (cons (first a-lon)
              (only-even (rest a-lon)))]
       [else     
        ; Test 2
        #;(check-expect (only-even (cons 99 (cons 1 (cons 99 (cons 1 (cons 2 empty))))))
                        (cons 2 empty))
        ; a-lon = 99:1:99:1:2:!
        ; first = 99
        ; rest = 1:99:1:2:!
        ; only-even rest = 2:!
        (only-even (rest a-lon))])]))

(define (only-even a-lon)
  (filter even? a-lon))

#;(cons 4 (only-even (cons 1 (cons 2 empty))))
#;(cons 4 (only-even (cons 2 empty)))

(check-expect (only-even empty)
              empty)
(check-expect (only-even (cons 2 empty))
              (cons 2 empty))
(check-expect (only-even (cons 99 (cons 1 (cons 99 (cons 1 (cons 2 empty))))))
              (cons 2 empty))

