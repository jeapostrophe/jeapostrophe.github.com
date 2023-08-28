;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2-1) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "htdp")))))
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

