;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2-3) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "htdp")))))
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

;; Day 3

; A shape is either
;  - square
;  - rect
;  - circ
#;(define (area s)
    (cond [(square? s) (area-square s)]
          [(rect? s) (area-rect s)]
          [(circ? s) (area-circ s)]))

; A list-of-numbers is either
;  - empty
;  - (cons number list-of-numbers)

; lon1 : list-of-numbers that represents the list "1 2 3"
(define lon1 (cons 1 (cons 2 (cons 3 empty))))

(define lon2 empty)
(define lon3 (cons 2 (cons 3 empty)))
(define lon4 (cons 6 lon1))

(check-expect (empty? lon2) true)
(check-expect (empty? lon1) false)
(check-expect (cons? lon2) false)
(check-expect (cons? lon1) true)

(check-expect (rest lon1) (cons 2 (cons 3 empty)))
(check-expect (first lon1) 1)
(check-expect (first lon4) 6)
(check-expect (rest lon1) (cons 2 (cons 3 empty)))
(check-expect (first (rest lon1)) 2)
(check-expect (rest (rest lon1)) (cons 3 empty))
(check-expect (rest lon1) lon3)

; sum : list-of-numbers -> number
; Purpose: To compute the sum of all the numbers in the list
#;(define (sum lon)
    ....)
#;(define (sum lon)
    (cond [(empty? lon) ...]
          [(cons? lon) ...]))
#;(define (sum lon)
    (cond [(empty? lon) ...]
          [(cons? lon) 
           ...
           (first lon)
           ...
           (rest lon)
           ...
           ]))
#;(define (sum lon)
    (cond [(empty? lon) ...]
          [(cons? lon) 
           ...
           (first lon)
           ...
           (sum (rest lon))
           ...
           ]))
(define (sum lon)
  (cond [(empty? lon) 0]
        [(cons? lon) 
         (+ (first lon)
            (sum (rest lon)))]))

(check-expect (sum (cons 1 empty)) 1)
(check-expect (sum empty) 0)
(check-expect (sum (cons 1 (cons 3 (cons 5 (cons 7 (cons 11 empty))))))
              27)

; contains? : number list-of-numbers -> boolean
; Purpose: To determine if a number is in the list
#;(define (contains? n lon)
    (cond [(empty? lon) ... n ...]
          [(cons? lon) 
           ...
           n
           ...
           (first lon)
           ...
           (contains? n (rest lon))
           ...
           ]))
(define (contains? n lon)
  (cond [(empty? lon) false]
        [(cons? lon) 
         (or (= n (first lon))
             (contains? n (rest lon)))]))

(check-expect (contains? 42 empty) false)
(check-expect (contains? 1830 empty) false)
(check-expect (contains? 1 lon1) true)
(check-expect (contains? 2 lon1) true)
(check-expect (contains? 3 lon1) true)
(check-expect (contains? 4 lon1) false)

; append : list list -> list
; Purpose: Return a list that contains all the elements of the first list, followed by those of the second

(check-expect (append empty empty) empty)
(check-expect (append (cons 1 empty) (cons 27 empty)) (cons 1 (cons 27 empty)))

; A exit-code is either
;  - number
;  - symbol

; A test-trace is either
;  - empty
;  - (cons exit-code test-trace)

; failed? : exit-code -> boolean
; Purpose: To determine if a particular test failed, where failed means returned non-zero or not 'SIGAWESOME
#;(define (failed? ec)
    (cond [(number? ec) ... ec ...]
          [(symbol? ec) ... ec ...]))
(define (failed? ec)
  (cond [(number? ec) 
         #;(cond [(zero? ec) false]
                 [else true])
         #;(not (zero? ec))
         (not (= 0 ec))]
        [(symbol? ec)
         (not (symbol=? 'SIGAWESOME ec))]))

; A Peano-number is either
;  - Zero
;  - Succ Peano-number

(check-expect (failed? 0) false)
(check-expect (failed? 255) true)
(check-expect (failed? 'SIGAWESOME) false)
(check-expect (failed? 'SIGINT) true)
(check-expect (failed? 'SIGTERM) true)
(check-expect (failed? 'SIGUNIVERSEEXPLODED) true)

; any-failed? : test-trace -> boolean
; Purpose: To determine if any test failed
#;(define (any-failed? tt)
    (cond [(empty? tt) ...]
          [(cons? tt)
           ...
           (failed? (first tt))
           ...
           (any-failed? (rest tt))
           ...]))
(define (any-failed? tt)
  (cond [(empty? tt) false]
        [(cons? tt)
         (or 
          (failed? (first tt))
          (any-failed? (rest tt)))]))

(check-expect (any-failed? empty) false)
(check-expect (any-failed? (cons 0 empty)) false)
(check-expect (any-failed? (cons 'SIGAWESOME empty)) false)
(check-expect (any-failed? (cons 'SIGAWESOME 
                                 (cons 0
                                       (cons 'SIGCOMPUTEREXPLODED
                                             empty))))
              true)
(check-expect (any-failed? (cons 'SIGAWESOME 
                                 (cons 'SIGINT
                                       (cons 'SIGCOMPUTEREXPLODED
                                             empty))))
              true)

