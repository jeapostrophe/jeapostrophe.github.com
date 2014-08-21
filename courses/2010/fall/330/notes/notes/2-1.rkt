;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2-1) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "htdp")))))
4

; 2 + 2 
(+ 2 2)

; 2 + 4 * 3
(+ 2 (* 4 3))

(* (+ 2 4) 3)

; Run = Cmd-T or Ctrl-T

; + : This is bad because no (
; (+ : No closing
; (+) : Needs 2 arguments
; (2 + 2) : Not a function, but a number

; S-expressions (or sexpr)

(* (+ (* (+ 2 4) 3) 4) (* (+ 2 (* (+ 2 4) 3)) 3))

(* (+ (* (+ 2 4)
         3)
      4) 
   (* (+ 2
         (* (+ 2 4) 3))
      3))

; Problem: Find the area of a disk, from its radius
; Information: radius -> area
; Data: number -> number

; area-of-disk : number -> number
; Purpose: To compute an area of a disk from a radius
(define (area-of-disk r)
  ....)

; TODO: test cases or example
; TODO: function body