;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2-2) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "htdp")))))
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

; PI : number
(define PI 3.14)

; Problem: Find the area of a disk, from its radius
; Information: radius -> area
; Data: number -> number

; area-of-disk : number -> number
; Purpose: To compute an area of a disk from a radius
(define (area-of-disk r)
  (* PI (expt r 2)))

; This is a block comment:
#;(check-expect (area-of-disk 0)
              0)

(check-expect (area-of-disk 0)
              0)
(check-expect (area-of-disk 1)
              PI)
(check-expect (area-of-disk 2)
              (* 4 PI))
(check-within (area-of-disk (/ 1 (sqrt PI)))
              1
              0.00001)

; Problem: Decide if a disk is the correct size for an Xbox360
; Information: disk's radius -> decision
; Data: number -> boolean

; cond's syntax:
#;(cond [question answer]
        [question answer]
        ...
        [else answer]) ; <-- that's optional

; disk-just-right-for-360? : number -> boolean
; Purpose: Computes if the disk is the correct radius for the 360's drive
(define (disk-just-right-for-360? r)
  #;(cond [(= r 5) true]
          [else false])
  #;(if (= r 5)
      true
      false)
  (= r 5))

(check-expect (disk-just-right-for-360? 4) false)
(check-expect (disk-just-right-for-360? 4.9999999999999999999999999999999999999999999999999999999)
              false)
(check-expect (disk-just-right-for-360? 5) true)
(check-expect (disk-just-right-for-360? 5.00000000000000000000000001) false)
(check-expect (disk-just-right-for-360? 6) false)

; Problem: What is the correct media size for a device?
; Information: device -> media size as a number

; A device-type is either
;  - 'Xbox360
;  - 'NES

; Data: device-type -> number

; media-size : device-type -> number
; Purpose: Compute the media size of the device
; Function header:
#;(define (media-size dt)
    ....)
; Function template: (based on data definition of device-type)
#;(define (media-size dt)
  (cond [(symbol=? dt 'Xbox360) ....]
        [(symbol=? dt 'NES) ....]))
; Function body
(define (media-size dt)
  (cond [(symbol=? dt 'Xbox360) 5]
        [(symbol=? dt 'NES) 6]))

(check-expect (media-size 'Xbox360) 5)
(check-expect (media-size 'NES) 6)

; Problem: Is a medium correctly sized for its intended device?
; Information: medium -> decision
; Relevant information: medium's device and its size -> decision

; Informal definition:
; A medium is a 
;  - device-type
;  - number representing size

; Formal definition:
; A medium is a (make-medium type size) where
;  - type is a device-type that the medium is intended for
;  - size is a number
(define-struct medium (type size))
; end of formal data definition

; make-medium : device-type number -> medium
; medium? : any -> boolean
; medium-type : medium -> device-type
; medium-size : medium -> number

(define Gradius (make-medium 'NES 6))
(define RadiantSilvergun (make-medium 'Xbox360 4.5))
(define Deathsmiles (make-medium 'Xbox360 5))

; Data: medium -> boolean

; medium-just-right? : medium -> boolean
; Purpose: Decide if the medium is correctly sized for its intended device
; Header
#;(define (medium-just-right? m)
  ...)
; Template
#;(define (medium-just-right? m)
  ...
  (medium-type m)
  ...
  (medium-size m)
  ...)
; Body
(define (medium-just-right? m)
  (= (media-size (medium-type m))
     (medium-size m)))

(check-expect (medium-just-right? Gradius) true)
(check-expect (medium-just-right? RadiantSilvergun) false)
(check-expect (medium-just-right? Deathsmiles) true)
