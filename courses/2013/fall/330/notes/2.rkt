#lang plai
(print-only-errors #t)
(halt-on-errors #t)

3
"Jay is so great at teaching 330"
'symbols

3.14
(define pie 3.14)

(define twopie (* 2 pie))

(define fourpie (* 2 twopie))

;; 2. CONTRACT: double : number -> number
;; 1. PURPOSE: It computes the value doubleth 
;;;;;;;;;;;;; or raiseth an exceptioneth if the numberth is negativeth
;; 3. Header
(define (double thing-one-goes-here)
  ;; 5. Body
  (* 2 thing-one-goes-here))
;; 4. Example
;; in bsl 'test' is spelled 'check-expect'
(test (double 2) 4)
(test (double 0) 0)
(test (double 21) 42)
;; (test (double 12) 42) bad test
(test (double 12) 24)
(test (= (double 12) 42) false)
(test (double -5) -10)
;; (test/exn (double -5) "posi, bro")
;; (test (double "Jay") doesn't matter) immoral test that violates
;; sacred covenents

;; X is sound for Y means "conclusion of X imply the same conclusion of Y"

;; CONTRACT: rating-of : string -> number
;; PURPOSE: It gives the Jay rating of the entertainment
(define (rating-of ;; the-entertainment-requested
                   e)
  ;; (cond [question answer] ... [else else-answer])
  (cond
    [(equal? e "Twilight")
     8]
    [(or (equal? e "Bayonetta")
         (equal? e "The BoM"))
     10]
    [(string=? e "TMNT")
     1]
    [else
     0]))
(test (rating-of "Twilight") 8)
(test (rating-of "Bayonetta") 10)
(test (rating-of "The BoM") 10)
(test (rating-of "TMNT") 1)
(test (rating-of "New Electric Sound") 0)

(struct game (system name))

(define bayo (game "X360" "Bayonetta"))
bayo
(test (game? bayo) true)
(test (game? "Twilight") false)
(test (game-system bayo) "X360")
(test (game-name bayo) "Bayonetta")

;; grating-of : game -> number
;; To rate the Game
(define (grating-of g)
  ;; 3.5 Template
  ;; ... (game-system g) ....
  ;; ... (game-name g) ....
  ;; Test 1 ... 10
  ;; Test 2 ...
  (rating-of (game-name g)))

(test (grating-of bayo) 10)
(test (grating-of (game "Wii" "Scene It? Twilight")) 0)

;; empty

;; (struct cons (first rest))
;; (define first cons-first)
;; (define rest cons-rest)

(cons 1 empty)
(test (first (cons 1 empty)) 1)
(test (rest (cons 1 empty)) empty)
(test (cons? (cons 1 empty)) true)

;; A list is either empty or cons

;; jlength : list -> number
;; To lengthify the list
(define (jlength l)
  ;; Template for unions
  #;
  (cond
    [(empty? l)
     .... ....]
    [(cons? l)
     .... (first l) ... (rest l) ....])
  ;; Template for unions that reference the thing
  #;
  (cond
    [(empty? l)
     .... ....]
    [(cons? l)
     .... (first l) ... (jlength (rest l)) ....])
  (cond
    [(empty? l)
     0]
    [(cons? l)
     (+ 1 (jlength (rest l)))]))

(test (jlength empty) 0)
(test (jlength (cons 1 empty)) 1)
(test (jlength (cons 1 (cons 1 empty))) 2)
