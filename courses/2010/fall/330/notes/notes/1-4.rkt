;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 1-4) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "htdp")))))
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

;; Day 3

; A shape is either
;  - circ
;  - square
;  - rect

; I hoped you'd write area like this:
#;(define (area s)
    (cond 
      [(circ? s) (area-circ s)]
      [(square? s) (area-square s)]
      [(rect? s) (area-rect s)]))


; A list-of-numbers is either
;  - empty
;  - (cons number list-of-numbers)

(define lon1 empty)
(define lon2 (cons 1 empty))
(define lon3 (cons 4 (cons 6 empty)))
(define lon2a (cons 1 lon1))
(define lon4 (cons 5 lon3))

(check-expect (first lon2) 1)
(check-expect (rest lon4) lon3)

; sum : list-of-numbers -> number
; Purpose: Compute the sum of all the numbers
#;(define (sum lon)
    ....)
#;(define (sum lon)
    (cond [(empty? lon) ....]
          [(cons? lon) ....]))
#;(define (sum lon)
    (cond [(empty? lon) ....]
          [(cons? lon) 
           ....
           (first lon)
           ...
           (rest lon)
           ...
           ]))
#;(define (sum lon)
    (cond [(empty? lon) ....]
          [(cons? lon) 
           ....
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

(check-expect (sum empty) 0)
(check-expect (sum lon2) 1)
(check-expect (sum lon4) 15)

; contains? : number list-of-numbers -> boolean
; Purpose: Determine if the number is in the list
#;(define (contains? n lon)
    (cond [(empty? lon) ... n ....]
          [(cons? lon) 
           ....
           n
           ....
           (first lon)
           ...
           (contains? n (rest lon))
           ...
           ]))
(define (contains? n lon)
  (cond [(empty? lon) 
         false]
        [(cons? lon) 
         (or
          (= n (first lon))
          (contains? n (rest lon)))]))

(check-expect (contains? 1 lon2) true)
(check-expect (contains? 6 lon4) true)
(check-expect (contains? 27 lon1) false)
(check-expect (contains? 23583 lon1) false)
(check-expect (contains? 0 lon4) false)
(check-expect (contains? 15 lon4) false)

; A list is either
; - empty
; - (cons something list)

; i-has-it? : symbol (listof symbol) -> boolean
(define (i-has-it? s los)
  (cond [(empty? los) false]
        [(cons? los) 
         (or (symbol=? s (first los))
             (i-has-it? s (rest los)))]))

(check-expect (i-has-it? 'NES empty) false)
(check-expect (i-has-it? 'Famicom (cons 'NES empty)) false)
(check-expect (i-has-it? 'NES (cons 'NES empty)) true)
(check-expect (i-has-it? 'PC (cons 'Mac (cons 'Famicom (cons 'PSP empty)))) false)

; append : list list -> list

(check-expect (append (cons 1 empty) empty)
              (cons 1 empty))
(check-expect (append (cons 1 (cons 2 empty)) (cons 5 (cons 0 empty)))
              (cons 1 (cons 2 (cons 5 (cons 0 empty)))))

; An error-code is either
;  - number
;  - symbol

; A test-trace is either
;  - empty
;  - (cons error-code test-trace)

; test-trace? : any -> boolean
; Purpose: Determines if the value is a test-trace (up to error-code)
(define (test-trace? x)
  (cond [(empty? x) true]
        [(cons? x) true]
        [else false]))

(check-expect (test-trace? empty) true)
(check-expect (test-trace? (cons 0 empty)) true)
(check-expect (test-trace? 0) false)
(check-expect (test-trace? 'sym) false)

; A real-error is either a non-zero number or a symbol other than 'hunkydorry

; real-error? : error-code -> boolean
; Purpose: To determine if an error code designates a real error
(define (real-error? ec)
  (cond [(number? ec) (not (zero? ec))]
        [(symbol? ec) (not (symbol=? ec 'hunkydorry))]))

(check-expect (real-error? 2) true)
(check-expect (real-error? 0) false)
(check-expect (real-error? 'computer-exploded) true)
(check-expect (real-error? 'universe-annihilated-in-logic-fault) true)
(check-expect (real-error? 'hunkydorry) false)
(check-expect (real-error? 'zero) true)

; any-failed? : test-trace -> boolean
; Purpose: To determine if any test in the trace exitted with a real-error
(define (any-failed? tt)
  (cond [(empty? tt) false]
        [(cons? tt)
         (or (real-error? (first tt))
             (any-failed? (rest tt)))]))

(check-expect (any-failed? empty) false)
(check-expect (any-failed? (cons 2 empty)) true)
(check-expect (any-failed? (cons 0 (cons 5 empty))) true)
(check-expect (any-failed? (cons 'hunkydorry (cons 0 empty))) false)
(check-expect (any-failed? (cons 'segfault (cons 'exploded (cons 0 empty)))) true)

; A test-tree is a (make-test-tree test test)
(define-struct test-tree (lhs rhs))

; A test is either a
;  - test-tree
;  - test-trace
;  - error-code

; test-tree-failed? : test-tree -> boolean
; Purpose: To determine if any sub-test for a test-tree failed
#;(define (test-tree-failed? tt)
  ...)
#;(define (test-tree-failed? tt)
  ...
  (test-tree-lhs tt)
  ...
  (test-tree-rhs tt)
  ...)
#;(define (test-tree-failed? tt)
  ...
  (test-failed? (test-tree-lhs tt))
  ...
  (test-failed? (test-tree-rhs tt))
  ...)
(define (test-tree-failed? tt)
  (or (test-failed? (test-tree-lhs tt))
      (test-failed? (test-tree-rhs tt))))

; test-failed? : test -> boolean
; Purpose: To determine if any sub-test failed.
#;(define (test-failed? t)
  ....)
#;(define (test-failed? t)
  (cond [(test-tree? t) ....]
        [(test-trace? t) ....]
        [else ....]))
(define (test-failed? t)
  (cond [(test-tree? t) (test-tree-failed? t)]
        [(test-trace? t) (any-failed? t)]
        [else (real-error? t)]))

(check-expect (test-tree-failed? (make-test-tree 0 'exploded)) true)
(check-expect (test-tree-failed? (make-test-tree 0 'hunkydorry)) false)
(check-expect (test-tree-failed? (make-test-tree (make-test-tree -1 5) 'hunkydorry)) true)
(check-expect (test-tree-failed? (make-test-tree (make-test-tree 0 'hunkydorry) 
                                                 (cons 'segfault empty)))
              true)

(check-expect (test-failed? 0) false)
(check-expect (test-failed? 1) true)
(check-expect (test-failed? 'hunkydorry) false)
(check-expect (test-failed? 'exploded) true)

(check-expect (test-failed? empty) false)
(check-expect (test-failed? (cons 0 empty)) false)
(check-expect (test-failed? (cons 'exploded (cons 0 empty))) true)

(check-expect (test-failed? (make-test-tree 0 'exploded)) true)
(check-expect (test-failed? (make-test-tree 0 'hunkydorry)) false)
(check-expect (test-failed? (make-test-tree (make-test-tree -1 5) 'hunkydorry)) true)
(check-expect (test-failed? (make-test-tree (make-test-tree 0 'hunkydorry) 
                                            (cons 'segfault empty)))
              true)

;; 

; price-cut : list-of-numbers [0,1] -> list-of-numbers
; Purpose: To apply a Walmart price cut on every items price
#;(define (price-cut lon %)
  (cond [(empty? lon) .... % ....]
        [(cons? lon)
         .... % ...
         .... (first lon) ....
         .... (price-cut (rest lon) %) ....]))
#;(define (price-cut lon %)
  (cond [(empty? lon) empty]
        [(cons? lon)
         (cons (* % (first lon))
               (price-cut (rest lon) %))]))
; Error: % is not available
#;(define (price-cut-fun n)
  (* % n))
(define (price-cut lon %)
  (local [(define (price-cut-fun n)
            (* % n))]
    (map price-cut-fun lon)))

; map : (a -> b) (listof a) -> (listof b)
#;(map f (cons 1 (cons 2 empty)))
#;(cons (f 1) (cons (f 2) empty))

(check-expect (price-cut empty 0.5) empty)
(check-expect (price-cut (cons 1 empty) 0.5) (cons 0.5 empty))
(check-expect (price-cut (cons 22 (cons 50 empty)) 0.5) 
              (cons 11 (cons 25 empty)))

; after-n : list-of-numbers number -> list-of-numbers
; Purpose: To drop all items that are larger than n
#;(define (after-n lon n)
  (cond [(empty? lon) ... n ...]
        [(cons? lon)
         .... n ....
         .... (first lon) ...
         .... (after-n (rest lon) n) ...]))
#;(define (after-n lon n)
  (cond [(empty? lon) empty]
        [(cons? lon)
         (cond [(< n (first lon))
                (after-n (rest lon) n)]
               [else
                (cons (first lon)
                      (after-n (rest lon) n))])]))
(define (after-n lon n)
  (local [(define (less-than-n? x)
            (>= n x))]
    (filter less-than-n? lon)))

; filter : (a -> boolean) (listof a) -> (listof a)
(check-expect (filter even? (cons 1 (cons 2 (cons 3 (cons 4 empty)))))
              (cons 2 (cons 4 empty)))

(check-expect (after-n empty 10) empty)
(check-expect (after-n (cons 1 empty) 10) (cons 1 empty))
(check-expect (after-n (cons 1 (cons 11 empty)) 10) (cons 1 empty))
(check-expect (after-n (cons 1 (cons 11 (cons 2 empty))) 10) (cons 1 (cons 2 empty)))

; Old sum:
#;(define (sum lon)
  (cond [(empty? lon) 0]
        [(cons? lon) 
         (+ (first lon)
            (sum (rest lon)))]))
(define (sum lon)
  (foldr + 0 lon))

; foldr : (a b -> b) b (listof a) -> b
; foldr : (num num -> num) num (listof num) -> num
  
#;(sum (cons 1 (cons 2 (cons 3 empty))))
#;(identity (+ 1 (+ 2 (+ 3 0))))

; num+string-append : num string -> string
(define (num+string-append n s)
  (string-append (number->string n) s))
; num-append : list-of-numbers -> string
(define (num-append lon)
  (foldr num+string-append "" lon))

(check-expect (num-append empty) "")
(check-expect (num-append (cons 1 (cons 39 empty))) "139")

; other-sum : list-of-numbers number -> number
; Purpose: To add a list of numbers to an ongoing sum
#;(define (other-sum lon current-sum)
  (cond [(empty? lon) ... current-sum ...]
        [(cons? lon)
         ... current-sum ...
         ... (first lon) ...
         .... (other-sum (rest lon) ....) ....]))
#;(define (other-sum lon current-sum)
  #;(+ (sum lon) current-sum)
  (cond [(empty? lon) current-sum]
        [(cons? lon)
         (other-sum (rest lon) 
                    (+ current-sum (first lon)))]))
(define (other-sum lon current-sum)
  (foldl + current-sum lon)
  #;(foldr + current-sum lon))


#;(other-sum (cons 1 (cons 2 (cons 3 empty))) 0)
#;(other-sum (cons 2 (cons 3 empty)) (+ 1 0))
#;(other-sum (cons 3 empty) (+ 2 (+ 1 0)))
#;(other-sum empty (+ 3 (+ 2 (+ 1 0))))
#;(identity (+ 3 (+ 2 (+ 1 0))))

(check-expect (other-sum empty 60) 60)
(check-expect (other-sum (cons 1 (cons 2 empty)) 24) 27)
(check-expect (other-sum (cons 1 (cons 2 empty)) 84) 87)
(check-expect (other-sum (cons 1 (cons 2 (cons 60 empty))) 24) 87)

; other-num+string-append : num string -> string
(define (other-num+string-append n s)
  (string-append (number->string n) s))
; other-num-append : list-of-numbers -> string
(define (other-num-append lon)
  (foldl other-num+string-append "" lon))

(check-expect (other-num-append empty) "")
(check-expect (other-num-append (cons 1 (cons 39 empty))) "391")