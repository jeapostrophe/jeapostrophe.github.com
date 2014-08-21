;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 1-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
#|
SEMANTICS --- meaning

denotational
axiomatic
operational
interpreter

|#

5

;; define

(+ (+ 5 5)
   (+ 5 5))

(define sally (+ 5 5))

(+ sally sally)

;; functions

;; CONTRACT double : number? -> number
;; PURPOSE computes the double of a number
(define (double sally) ; this is called the function header
  (+ sally sally))

(check-expect (double sally) 20)
(check-expect (double 17) 34)

(number? 4)
(number? "4")

;; cond

#;(cond
    [question 
     answer]
    ...)

(cond
  [(string? sally)
   (string-append sally " there")]
  [(number? sally)
   (double sally)])

(define (weird sally)
  (cond
  [(string? sally)
   (string-append sally " there")]
  [(number? sally)
   (double sally)]
  [else
   "no clue"]))

(weird sally)
(weird "Sally")
(weird 'sally)
   
;; struct

;; CONTRACT A game is a system and title
;;   where system is a string
;;      and title is a string
;; PURPOSE Stores information about video games
(define-struct game (system ; string
                     title ; string
                     ))
(game? "Sally")
(define bayo
  (make-game "X360" "Bayonetta"))
bayo
(game-system bayo)
(game-title bayo)

(make-game 360 'bayonetta)

;; list

;; A list is either
;;      empty
;;  or, (cons value list)
empty
(cons 1 empty)
(cons 2 (cons 1 empty))
(first (cons 1 empty))
(rest (cons 1 empty))

;; jay-length :: list -> number
;; find the length of a list in the jayaverse
(define (jay-length some-list)
  ;; some-list is just a list
  (cond
    [(empty? some-list)
     ;; some-list is empty
     42]
    [else
     ;; some-list is (cons value list)
     ;; Example 1. some-list is (cons 1 empty)
     ;; value is 1
     ;; list is empty
     ;; (jay-length list) is 42
     (+ 1
        (jay-length (rest some-list)))]))

(check-expect (jay-length empty)
              42)
(check-expect (jay-length (cons 1 empty))
              43)
(check-expect (jay-length (cons 2 (cons 1 empty)))
              44)





