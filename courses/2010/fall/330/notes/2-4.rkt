;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 2-4) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "htdp")))))
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

; test-trace? : any -> boolean
(define (test-trace? x)
  (or (empty? x) (cons? x)))

(check-expect (test-trace? empty) true)
(check-expect (test-trace? (cons 1 empty)) true)
(check-expect (test-trace? 5) false)

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

; A test-tree is a (make-test-tree lhs rhs) where
;  - lhs is a test
;  - rhs is a test
(define-struct test-tree (lhs rhs))

; test-tree-failed? : test-tree -> boolean
(define (test-tree-failed? tt)
  (or (test-failed? (test-tree-lhs tt))
      (test-failed? (test-tree-rhs tt))))

(check-expect (test-tree-failed? (make-test-tree 0 0))
              false)
(check-expect (test-tree-failed? (make-test-tree 0 -1))
              true)
(check-expect (test-tree-failed? (make-test-tree -1 0))
              true)
(check-expect (test-tree-failed? (make-test-tree 0 (cons 1 empty)))
              true)
(check-expect (test-tree-failed? (make-test-tree 'SIGAWESOME (cons 1 empty)))
              true)
(check-expect (test-tree-failed? (make-test-tree 'SIGAWESOME (make-test-tree -1 0)))
              true)

; A test is ethier
;  - test-tree
;  - test-trace
;  - exit-code

; test-failed? : test -> boolean
; Purpose: To determine if any sub-test failed
#;(define (test-failed? t)
  (cond [(test-tree? t) ....]
        [(test-trace? t) ....]
        [else ....]))
(define (test-failed? t)
  (cond [(test-tree? t) (test-tree-failed? t)]
        [(test-trace? t) (any-failed? t)]
        [else (failed? t)]))
        
(check-expect (test-failed? (make-test-tree 0 0))
              false)
(check-expect (test-failed? (make-test-tree 0 -1))
              true)
(check-expect (test-failed? (make-test-tree -1 0))
              true)
(check-expect (test-failed? (make-test-tree 0 (cons 1 empty)))
              true)
(check-expect (test-failed? (make-test-tree 'SIGAWESOME (cons 1 empty)))
              true)
(check-expect (test-failed? (make-test-tree 'SIGAWESOME (make-test-tree -1 0)))
              true)

(check-expect (test-failed? empty) false)
(check-expect (test-failed? (cons 0 empty)) false)
(check-expect (test-failed? (cons 'SIGAWESOME empty)) false)
(check-expect (test-failed? (cons 'SIGAWESOME (cons 1 empty))) true)

(check-expect (test-failed? 0) false)
(check-expect (test-failed? 1) true)
(check-expect (test-failed? 'SIGAWESOME) false)
(check-expect (test-failed? 'SIGHUP) true)

(define list1 (cons 1 empty))
(define list2 (cons 2 (cons 3 empty)))
(define list3 empty)
(cons list1 (cons list2 (cons list3 empty)))

; price-cut : list-of-numbers [0,1] -> list-of-numbers
; Purpose: Apply the percentage to the list of numbers
#;(define (price-cut lon %)
  (cond [(empty? lon) empty]
        [(cons? lon)
         (cons
          (* (first lon) %)
          (price-cut (rest lon) %))]))

(define (price-cut lon %)
  (local [(define (multiply-by-% n)
            (* n %))]
    (map multiply-by-% lon)))


; map : (a -> b) (listof a) -> (listof b)
(check-expect (map number->string (list 1 2 3))
              (list "1" "2" "3"))

(check-expect (price-cut empty 0.40) empty)
(check-expect (price-cut (cons 1 empty) 0.25)
              (cons 0.25 empty))

; after-n : lon num -> lon
; Purpose: Remove all numbers that are greater than the given number from the list
#;(define (after-n lon ceiling)
  (cond [(empty? lon) empty]
        [(cons? lon)
         (cond 
           [(>= ceiling
                (first lon))
            (cons
             (first lon)
             (after-n (rest lon) ceiling))]
           [else
            (after-n (rest lon) ceiling)])]))
#;(define (after-n lon ceiling)
  (local [; not-greater-than-ceiling : number -> boolean
          ; Purpose: Determine if a number is not greater than 'ceiling'
          (define (not-greater-than-ceiling n)
            (>= ceiling n))]
    (filter not-greater-than-ceiling lon)))
(define (after-n lon ceiling)
  (filter (lambda (n)
            (>= ceiling n))
          lon))

; filter : (a -> boolean) (listof a) -> (listof a)
(check-expect (filter even? (list 1 2 3 4))
              (list 2 4))

(check-expect (after-n empty 50) empty)
(check-expect (after-n (cons 1 empty) 50) (cons 1 empty))
(check-expect (after-n (cons 1 (cons 54 empty)) 50) (cons 1 empty))
(check-expect (after-n (cons 1 (cons 54 (cons 2 empty))) 50) (cons 1 (cons 2 empty)))

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
#;(define (sum lon)
  (cond [(empty? lon) 0]
        [(cons? lon) 
         (+ (first lon)
            (sum (rest lon)))]))

; foldr : (a b -> b) b (listof a) -> b
(define (sum lon)
  (foldr + 0 lon))

(check-expect (sum (cons 1 empty)) 1)
(check-expect (sum empty) 0)
(check-expect (sum (cons 1 (cons 3 (cons 5 (cons 7 (cons 11 empty))))))
              27)
         
#;(sum (cons 1 (cons 2 (cons 3 empty))))
#;(+ 1 (sum (cons 2 (cons 3 empty))))
#;(+ 1 (+ 2 (sum (cons 3 empty))))
#;(+ 1 (+ 2 (+ 3 (sum empty))))
#;(+ 1 (+ 2 (+ 3 0)))

(define (num+string-append n s)
  (string-append (number->string n) s))
(define (num-append lon)
  (foldr num+string-append "" lon))

(check-expect (num-append (list 1 2 3)) "123")

; other-sum : lon num -> num
; Purpose: Sum the list numbers with the given number
#;(define (other-sum lon current-sum)
  (cond [(empty? lon) current-sum]
        [(cons? lon)
         (other-sum (rest lon)
                    (+ (first lon)
                       current-sum))]))
(define (other-sum lon current-sum)
  (foldl + current-sum lon))

(check-expect (other-sum empty 25) 25)
(check-expect (other-sum (cons 1 empty) 25) 26)
(check-expect (other-sum (cons 1 (cons 2 empty)) 25) 28)
(check-expect (other-sum (cons 1 empty) (+ 2 25)) 28)
         
#;(other-sum (cons 1 (cons 2 (cons 3 empty))) 0)
#;(other-sum (cons 2 (cons 3 empty)) (+ 1 0))
#;(other-sum (cons 3 empty) (+ 2 (+ 1 0)))
#;(other-sum empty (+ 3 (+ 2 (+ 1 0))))
#;(+ 3 (+ 2 (+ 1 0)))

(define (num-appendl lon)
  (foldl num+string-append "" lon))

(check-expect (num-appendl (list 1 2 3)) "321")