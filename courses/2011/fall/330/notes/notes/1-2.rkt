;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 1-2) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "htdp")))))
;;;;; Day 1

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

;;;; Day 2

#t
#f

(check-expect (= 10 (+ 5 5))
              #t)
(check-expect (= 11 (+ 5 5))
              #f)

(< 10 25)

(>= 67 99)

(+ 1 2 3)

(number? 67)

(number? #t)

#;(if question
    true-answer
    false-answer)

(if (number? 67)
    (+ 67 99)
    "Logic annihilated, universe is done.")

(if (number? #t)
    (+ #t 99)
    "Crisis averted")

(number->string 67)
(number->string 67/99)
(number->string 67.99)
(number->string (sqrt 67.99))

(if (number? 67)
    (+ 67 99)
    (if (boolean? 67)
        (and 67 #f)
        "I dunno what to do"))

(+ (+ 1 1)
   (+ 1 1))

#;(cond
    [question answer]
    ...
    [else else-answer])

(cond
  [(number? 67) (+ 67 99)]
  [(boolean? 67) (and 67 #f)]
  [else "I dunno what todo"])

(define x 69)
(cond
  [(number? x) (+ x 99)]
  [(boolean? x) (and x #f)]
  [else "I dunno what todo"])

#;(case thing-expr
    [(option ...) answer]
    [(option ...) answer]
    ...
    [else else-answer])

#;(case 67
    [(67 68 69) "pretty big"]
    [(32) "try harder"]
    [else "answer not clear"])

(define-struct 3d-point (x y z))

(make-2d-point 5 5)
(make-3d-point 6 7 8)

(define (maggy-in-flatland p)  
  ; 2d-point-x 2d-point-y
  (sqrt (+ (sqr (2d-point-x p))
           (sqr (2d-point-y p)))))

(define (maggy-in-doom3-on-the-run p)  
  ; 3d-point-x 3d-point-y 3d-point-z
  (sqrt (+ (sqr (3d-point-x p))
           (sqr (3d-point-y p))
           (sqr (3d-point-z p)))))

; maggy : 2d-point or a 3d-point -> number
(define (maggy p)
  ; p
  (if (2d-point? p)
      (maggy-in-flatland p)
      (maggy-in-doom3-on-the-run p)))

(check-expect (maggy (make-2d-point 0 7))
              7)
(check-expect (maggy (make-2d-point 6 0))
              6)
(check-expect (maggy (make-2d-point 3 4))
              5)
(check-expect (maggy (make-3d-point 8 0 0))
              8)
(check-expect (maggy (make-3d-point 0 9 0))
              9)
(check-expect (maggy (make-3d-point 0 0 10))
              10)
(check-expect (maggy (make-3d-point 3 4 0))
              5)
(check-within (maggy (make-3d-point 3 2 2))
              (sqrt 17)
              0.0001)

