;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
42

"HeLOL world"

'north

#|
f ( 10 )
1 + 1
rocket -> launchAtRussiaLOLz ()
mario.jump()
|#

#|
(f 10)
(+ 1 1)
(launchAtRussiaLOLz rocket)
(jump mario)
|#

(+ 1 1)

(check-expect
 (+ (+ 1 (+ 1 1))
    (+ 1 (+ 1 (+ 1 1))))
 7)

(define-struct 2d-point (x y))
; make-2d-point : x y -> 2d-point
; 2d-point-x : 2d-point -> x
; 2d-point-y : 2d-point -> y

(make-2d-point 5 5)

; Problem: Write a program to reflect a 2d point over the y-axis

; Step 1.
; reflect : 2d-point -> 2d-point
; Step 2.
; find the reflection of the point over the y-axis
; Step 4.
(define (reflect p)
  #;...
  ; Step 5.
  ; p
  ; (2d-point-x p) (2d-point-y p)
  ; Step 6.
  
  ; Test 1
  #;(check-expect (reflect (make-2d-point 5 5))
                  (make-2d-point -5 5))
  ; 5 5
  #;(make-2d-point -5 5)
  
  ; Test 2
  #;(check-expect (reflect (make-2d-point 2 3))
                  (make-2d-point -2 3))
  ; 2 3
  #;(make-2d-point -2 3)
  
  ; Generalize 1 & 2 to:
  (make-2d-point (- (2d-point-x p))
                 (2d-point-y p)))

; Step 3.
(check-expect (reflect (make-2d-point 5 5))
              (make-2d-point -5 5))
(check-expect (reflect (make-2d-point 2 3))
              (make-2d-point -2 3))


;;;;; Day 2

#t ; true
#f ; false

(= 10 (+ 5 5))

(< 5 21)

(> 5 5)

(and #t (< 3 4))

(number? #t)

(number? 67)

#;(if question
    true-answer
    false-answer)

(if #t
    42
    -1)

#;(/ 1 0)

#;(+ 1 (/ 1 0))

(if (zero? 0)
    "ain't gonna do it"
    (/ 1 0))

(zero? 42)
(= 0 42)

(if (zero? 42)
    "ain't gonna do it"
    (/ 1 42))

(if (zero? 42)
    "ain't gonna do it"
    (if (negative? 42)
        "negative division is impossible"
        (/ 1 42)))

(+ (+ 1 1)
   (+ 1 (+ 1 1)))

(if (zero? 42)
    "ain't gonna do it"
    (if (negative? 42)
        "negative division is impossible"
        (if (positive? 42)
            (/ 1 42)
            "impossible number")))

34+5i

#;(cond
    [question1 answer1]
    [question2 answer2]
    ...
    [else else-answer])

(cond
  [(zero? 42)
   "ain't gonna do it"]
  [(negative? 42)
   "negative division is impossible"]
  [(positive? 42)
   (/ 1 42)]
  [else
   "impossible number circa 1400"])

(define x 42)
(cond
  [(zero? x)
   "ain't gonna do it"]
  [(negative? x)
   "negative division is impossible"]
  [(positive? x)
   (/ 1 x)]
  [else
   "impossible number circa 1400"])

(define-struct 3d-point (x y z))

(make-2d-point 4 5)
(make-3d-point 4 5 6)


#|
double maggy ( NdPoint p ) {
}
|#

; / : number that isn't zero -> number

; maggy : 2d-point or a 3d-point -> number
(define (maggy p)
  ; Distinguish (1 & 2) & 3 AND 4
  (if (2d-point? p)
      ; Test 1
      #;(check-expect (maggy (make-2d-point 1 0))
                      1)
      #;(2d-point-x p)
      #;1
      
      ; Test 2
      #;(check-expect (maggy (make-2d-point 2 0))
                      2)
      #;(2d-point-x p)
      #;2
      
      ; Generalize 1 & 2
      #;(2d-point-x p)
      
      ; Test 3
      #;(check-expect (maggy (make-2d-point 0 2))
                      2)
      #;(2d-point-y p)
      #;2
      
      ; Generalize (1 & 2) & 3
      (sqrt (+ (sqr (2d-point-x p))
               (sqr (2d-point-y p))))
      
      ; Test 4
      #;(check-expect (maggy (make-3d-point 5 0 0))
                      5)
      (sqrt (+ (sqr (3d-point-x p))
               (sqr (3d-point-y p))
               (sqr (3d-point-z p))))))

; If y is zero, then it must be x, because it's a parallel
(check-expect (maggy (make-2d-point 1 0))
              1)
(check-expect (maggy (make-2d-point 2 0))
              2)
(check-expect (maggy (make-2d-point 0 2))
              2)
; Otherwise, we want the hypotanoooose of the triangle
(check-expect (maggy (make-2d-point 3 4))
              5)
(check-expect (maggy (make-3d-point 5 0 0))
              5)
(check-expect (maggy (make-3d-point 0 6 0))
              6)
(check-expect (maggy (make-3d-point 0 0 7))
              7)
(check-within (maggy (make-3d-point 3 2 2))
              #i4.12
              0.01)

;;; Day 3

; A list of numbers is... either
;  one number followed another list of numbers = (cons one-number the-other-numbers)
;  no numbers = empty

empty
(cons 1 empty)
(cons 3 (cons 1 empty))

(define jays-favourite-numbers
  (cons 9/2
        (cons 6/23
              (cons 42
                    (cons 777
                          (cons 1197
                                empty))))))

jays-favourite-numbers

(empty? jays-favourite-numbers)
(cons? jays-favourite-numbers)

(first jays-favourite-numbers)
(rest jays-favourite-numbers)

; sum : LoN -> number
; Purpose: to compute the sum of allll the numbers
(define (sum a-lon)
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
  
; homomorphisms
;; (cons 1 (cons 2 (cons 3 empty)))
;; =
;; (+ 1 (+ 2 (+ 3 0)))

; (reduce (map 1) (reduce (map 2) (reduce (map 3) zero)))


; even-steven? : LoN -> LoB
(define (even-steven? a-lon)
  (cond 
    [(empty? a-lon)
     empty]
    [else
     (cons
      (even? (first a-lon))
      (even-steven? (rest a-lon)))]))
     
     
(even-steven? (cons 99 (cons 1 (cons 99 (cons 1 (cons 2 empty))))))
     
#|
 int f ( int x ) {
  return f ( x );
 }
 f ( 100 );
|#
     
(define (f x)
  (f x))

#;(f 100)





