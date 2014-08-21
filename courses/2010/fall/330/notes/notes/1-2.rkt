;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 1-2) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "htdp")))))
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

; PI : number
(define PI 3.14)

; Given the radius of a disk, what is its area?
; Information: radius -> area
; Data: number -> number

; area-of-disk : number -> number
; Purpose: to compute the area of a disk from its radius
(define (area-of-disk r)
  (* PI (expt r 2)))

(check-expect (area-of-disk 0)
              0)
(check-within (area-of-disk (/ 1 (sqrt PI)))
              1
              0.01)
(check-expect (area-of-disk 1)
              PI)

; Decide if the disk is too big for the DVD player.
; Information: radius of disk in cm -> decision
; Data: non-negative number -> boolean

; Below here is a comment:
#;(cond [question answer]
        ...
        [else answer])

; too-big-for-xbox? : non-negative number -> boolean
; Purpose: to determine if a disk is too big (in radius) for an xbox
(define (too-big-for-xbox? r)
  #;(if (<= r 5)
        false
        true)
  #;(cond [(<= r 5) false]
          [else true])
  #;(not (<= r 5))
  (> r 5))

(check-expect (too-big-for-xbox? 0) false)
(check-expect (too-big-for-xbox? 5) false)
(check-expect (too-big-for-xbox? 5.0000000000000000000001) true)
(check-expect (too-big-for-xbox? 6) true)

; Problem: Compute the media size limit of a game storage device
; Information: what the device is -> size in cm

; A device-type is either
;  - 'NES
;  - 'Xbox360

; Data: device-type -> number

; media-size : device-type -> number
; Purpose: Compute the size limit of the device
(define (media-size dt)
  ; Function template
  ; that is totally derived from the data definition
  #;(cond [(symbol=? dt 'NES) ....]
          [(symbol=? dt 'Xbox360) ....])
  (cond [(symbol=? dt 'NES) 7]
        [(symbol=? dt 'Xbox360) 5]))

(check-expect (media-size 'NES) 7)
(check-expect (media-size 'Xbox360) 5)

; Problem: Decide if a medium is too big for its device
; Information: medium -> decision

; A medium is a (make-medium type size) where
;  - type is a device-type
;  - size is a number

(define-struct medium (type size))
; make-medium : device-type number -> medium
; medium? : any -> boolean
; medium-type : medium -> device-type
; medium-size : medium -> number

; Data examples:
(define Gradius (make-medium 'NES 7))
(define Deathsmiles (make-medium 'Xbox360 5))
(define FinalFantasy6 (make-medium 'NES 8))

; Data: medium -> boolean

; medium-too-big? : medium -> boolean
; Purpose: Decide if the medium is too big
; Template:
#;(define (medium-too-big? m)
  ....
  (medium-type m)
  (medium-size m)
  ....)
(define (medium-too-big? m)
  (< (media-size (medium-type m))
     (medium-size m)))

(check-expect (medium-too-big? Gradius) false)
(check-expect (medium-too-big? Deathsmiles) false)
(check-expect (medium-too-big? FinalFantasy6) true)