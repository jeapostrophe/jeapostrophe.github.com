;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; CONTRACT: Input    -> Output
;;           Negative -> Positive
;;
;; Negative things are ASSUMPTIONS
;; Positive things are GUARANTEES
;;
;; X -> Y
;; T -> T == T
;; T -> F == F
;; F -> F == T
;; F -> T == T
;;
;; PURPOSE:
;; HEADER:
#;(define (fun arg1 arg2 ...)
    body)
;; TEST CASES
#;(check-expect (fun input1 ...) output)

;; number = negative number or non-neg number
#;(define (generic-number-fun n)
    (cond
      [tell-if-negative
       ;; AVAILABLE THINGS: n
       ....]
      [else
       ;; AVAILABLE THINGS: n
       ....]))

;; jabs : number -> non-neg number
(define (jabs n)
  ;; EX1 n = 0
  ;; EX2 n = 1
  ;; EX3 n = -5
  (cond
    ;; EX1 = #f
    ;; EX2 = #f
    ;; EX3 = #t
    [(negative? n)
     ;; EX3 gets here
     #;5
     ;; EX4 gets here
     #;6
     ;; 5 != 6
     ;; Generalize f (only n is in scope) s.t. f(-5) = 5 and f(-6) = 6
     (* -1 n)]
    [else
     ;; EX1 gets here
     #;0
     ;; EX2 gets here
     ;; fails, because should 1 but got 0
     #;1
     ;; Generalize from EX1 and EX2
     n]))
(check-expect (jabs 0) 0)
(check-expect (jabs 1) 1)
(check-expect (jabs -5) 5)
(check-expect (jabs -6) 6)

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An MP3 is a (make-mp3 string string)
(define-struct mp3 (artist song))
;; Generates functions for you:
;;  make-mp3   : thing thing -> mp3?
;;  mp3?       : thing -> boolean
;;  mp3-artist : mp3? -> thing
;;  mp3-song   : mp3? -> thing

;; We have the following contracts:
;;  make-mp3   : string string -> MP3
;;  mp3?       : thing -> boolean
;;  mp3-artist : MP3 -> string
;;  mp3-song   : MP3 -> string

;; function-on-MP3 : MP3 -> Answer
(define (function-on-MP3 m)
  ;; AVAILABLE: m : MP3
  ;;            (mp3-artist m) : string
  ;;            (mp3-song m) : string
  ....)

(define-struct name-entry (first last))
;; name-entry-first : name-entry -> thing
;; (name-entry-first (mp3-artist m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A (jlist A) is either
;;  - (make-jempty)
;;  - (make-jcons A (jlist A))
(define-struct jempty ())
(define-struct jcons (first rest))

;; A (list-of A) is either
;;  - empty
;;  - (cons-of A) = (cons A (list-of A))

;; empty : a (list-of A) for any A
;; cons : A (list-of A) -> (cons-of A)
;; first : (cons-of A) -> A
;; rest : (cons-of A) -> (list-of A)
;; empty? : thing -> bool
;; cons? : thing -> bool

;; generic-list-fun : (list-of A) -> Answer
(define (generic-list-fun l1)
  ;; AVAILABLE: l1 : (list-of A)
  ;;            (empty? l1) : bool
  ;;            (cons? l1) : bool
  (cond
    [(empty? l1)
     ;; AVAILABLE: 
     ....]
    [else
     ;; AVAILABLE:                  l1 : (cons-of A)
     ;;                     (first l1) : A
     ;;                      (rest l1) : (list-of A)
     ;;   (generic-list-fun (rest l1)) : Answer
     ;; Why not include
     ;;   (empty? (rest l1)) and so on
     ;; Law of Demeter: Never look at the contents of your contents
     ;;        in Java: Only 1 dot in a row (i.e. o.x is okay o.x.y isn't)
     ;;                 o.m().x
     ....]))

;; BT A = BinaryTree A = Leaf | Branch (BT A) A (BT A)
#;(cond
    [(Leaf? bt)
     ....]
    [(Branch? bt)
     ;; (FUN (branch-left bt))
     ;; (FUN (branch-right bt))
     ;; (branch-data bt)
     ....])

;; Rose A = Leaf | Bush A (Rose A) | Stump (Rose A) A (Rose A)
#;(cond
    [(Leaf? r)
     ....]
    [(Bush? r)
     ;; (Bush-data r)
     ;; (FUN (Bush-inner r))
     ....]
    [(Stump? r)
     ;; (FUN (Stump-left r))
     ;; (FUN (Stump-right r))
     ;; (Stump-data r)
     ....])

;;;;;;;;;;;;;;;;;;;;;;;;

;; wish : number (sorted-list-of num) -> (sorted-list-of num)
;; Put the number in the right spot
(define (wish n l)
  (cond
    [(empty? l)
     (cons n empty)]
    [else
     (cond
       [(< n (first l))
        (cons n l)]
       [else
        (cons (first l)
              (wish n (rest l)))])]))

;; sort : (list-of number) -> (list-of number)
;; Returns a permutation of the input in increasing order
(define (sort l1)
  ;; AVAILABLE: l1 : (list-of A)
  ;;            (empty? l1) : bool
  ;;            (cons? l1) : bool
  (cond
    [(empty? l1)
     ;; AVAILABLE: 
     empty]
    [else
     ;; AVAILABLE:                  l1 : (cons-of A)
     ;;                     (first l1) : A
     ;;                      (rest l1) : (list-of A)
     ;;               (sort (rest l1)) : (sorted-list-of A)

     ;; EX2 : l1 = 2 :: []
     ;;     ; f l1 = 2
     ;;     ; r l1 = []
     ;;     ; sort r l1 = []
     #;(cons (first l1)
           (sort (rest l1)))

     ;; EX3 : l1 = 2 : 1 :: []
     ;;     ; f l1 = 2
     ;;     ; r l1 = 1 :: []
     ;;     ; s r l1 = 1 :: []
     ;; RESULT = 2 : 1 :: []
     ;; EXPECTED = 1 : 2 :: []

     ;; wish : number (sorted-list-of num) -> (sorted-list-of num)
     ;; Put the number in the right spot
     (wish (first l1)
           (sort (rest l1)))
     ]))

(check-expect (sort empty) empty)
(check-expect (sort (cons 2 empty)) (cons 2 empty))
(check-expect (sort (cons 2 (cons 1 empty))) (cons 1 (cons 2 empty)))
(check-expect (sort (cons 1 (cons 2 empty))) (cons 1 (cons 2 empty)))