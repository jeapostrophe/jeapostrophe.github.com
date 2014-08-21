;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 1-1) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "htdp")))))
42

"HelLOL world"

#|
f ( 2 )
2 + 2
rocket . launchAtRussia ()
rocket -> launchAtRussia ()
|#

#|
(f 2)
(+ 2 2)
(launchAtRussiaLOLz rocket)
(launchAtRussiaLOLz rocket)
|#

(+ 2 2)

(+ 2 2)

(+ (+ 2 2)
   (+ 2 2))
(+ 4
   (+ 2 2))
(+ 4
   4)
8

'wicked-awesome-symbol

(check-expect (+ (+ 2 2)
                 (+ 2 2))
              8)
#;(check-expect (+ (+ 2 2)
                 (+ 2 2))
              9)
#;(check-expect (+ (+ 2 2)
                 (+ 2 3))
              8)

(define-struct 2d-point (x y))
; make-2d-point : x y -> 2d-point
; 2d-point-x : 2d-point -> x
; 2d-point-y : 2d-point -> y

(make-2d-point 10 25)

; Project: Write the function, reflect, that reflects the point across the y-axis

; Step 1. reflect : 2d-point -> 2d-point
; Step 2. To reflect the point
; Step 4.
(define (reflect p)
  ; Step 5.
  ; p
  ; (2d-point-x p) (2d-point-y p)
  ; Step 6.
  ; Test 1.
  #;(check-expect (reflect (make-2d-point 5 5))
                  (make-2d-point -5 5))
  ; 5 5
  #;(make-2d-point -5 5)
  ; Test 2.
  #;(check-expect (reflect (make-2d-point 25 9))
                  (make-2d-point -25 9))
  ; 25 9
  #;(make-2d-point -25 9)
  ; Generalize 1 & 2
  (make-2d-point (- (2d-point-x p))
                 (2d-point-y p)))

; Step 3.
(check-expect (reflect (make-2d-point 5 5))
              (make-2d-point -5 5))
(check-expect (reflect (make-2d-point 25 9))
              (make-2d-point -25 9))


