;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 330-2010-08-30) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "htdp")))))
4
"Hello World"


(+ 2 2)

"S-expression"
"Sexpr"

(check-expect 
 (+ 2 (* 4 3))
 14)
(check-expect 
 (+ 3 (* 4 3))
 15)
(check-expect 
 (+ 4 (* 4 3))
 16)

; Given the radius of a disk, what is its area?
; Information: radius -> area
; Data: number -> number

; area-of-disk : number -> number
; Purpose: to compute the area of a disk from its radius
(define (area-of-disk r)
  ....)

; TODO: examples
; TODO: function body

