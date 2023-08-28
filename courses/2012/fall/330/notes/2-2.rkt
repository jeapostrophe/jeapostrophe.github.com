;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
#|

SEMANTICS --- meaning

specifications in English

denotational --- compiler to math
.... "5" means the natural number 5
      "+" means actual addition

axiomatic --- mapping to logic

operational  --- interpreter in math

interpreter --- interpreter in a lang where one of above is available

|#

;; define

(+ 5 5)

(+ (+ 5 5)
   (+ 5 5))

(define sally
  (+ 5 5))

sally
(+ sally sally)

;; functions

;; CONTRACT double : number -> number
;; PURPOSE computes the double of the number
(define (double sally)
  (+ sally sally))

(check-expect (double sally) 20)
(check-expect (double 17) 34)

;; cond

#;(cond
    [question
     answer]
    ...)

(cond
  [(string? sally)
   sally]
  [(number? sally)
   "number"]
  [else
   "who knows?"])

(define (identify sally)
  (cond
  [(string? sally)
   sally]
  [(number? sally)
   "number"]
  [else
   "who knows?"]))

(identify sally)
(identify "Sally")
(identify 'sally)

;; structs

;; A game is a (make-game system title)
;; where system is a string
;;    and title is a string
;; Stores information about Jay's favourite games
(define-struct game (system title))
(game? sally)
(make-game "X360" "Bayonetta")
(define bayo
  (make-game "X360" "Bayonetta"))
bayo
(game? bayo)
(game-system bayo)
(game-title bayo)
(make-game 360 'bayonetta)

;; list
  
;; A list is either
;;    empty
;; or (cons value list)
empty
(cons 1 empty)
(empty? empty)
(empty? 4)
(cons? empty)
(cons? (cons 1 empty))
(cons? 5)
#;(list? empty)
#;(list? (cons 1 empty))
#;(list? 5)
(first (cons 1 empty))
(rest (cons 1 empty))

(define l0 empty)
(define l1 (cons 1 l0))
(first l1) (rest l1)

;; CONTRACT jay-length : list -> number
;; PURPOSE find the length of a list in the jayaverse
(define (jay-length some-list)
  ;; some-list is empty or cons
  (cond
    [(empty? some-list)
     ;; some list is empty
     42]
    [else
     ;; some list is (cons ... ...)
     (- (jay-length (rest some-list)) ; = 42
        1) ; = 1
     ]))

(check-expect (jay-length empty)
              42)
(check-expect (jay-length (cons 1 empty))
              41)
(check-expect (jay-length (cons 2 (cons 1 empty)))
              40)


