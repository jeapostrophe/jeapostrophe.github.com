;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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

