;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A binary tree is either
;;  - a (make-leaf)
;;  - a (make-branch bt number? bt)
(define (binary-tree? x)
  (or (leaf? x) (branch? x)))

(define-struct leaf ())
(define-struct branch (left data right))

(define ex1
  (make-branch (make-branch (make-leaf) 4 (make-leaf))
               5
               (make-branch (make-leaf) 8 (make-leaf))))

;; generic-tree-func : bt -> Ans
(define (generic-tree-func bt)
  (cond
    [(leaf? bt)
     ....]
    [(branch? bt)
     ;; AVAILABLE: bt : branch
     ;;          (branch-left bt) : bt
     ;;         (branch-right bt) : bt
     ;;         (branch-data bt) : num
     ;;        (gtf (branch-left bt)) : ans
     ;;       (gtf (branch-right bt)) : ans
     ;;    
     ....]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
     ....]))

;;;;;;

;; squares : list-of-num -> list-of-num
;; Compute squares of those given

(define (squares l1)
  ;; AVAILABLE: l1 : (list-of A)
  ;;            (empty? l1) : bool
  ;;            (cons? l1) : bool
  ;; FAIL l1 = 1::[]
  (cond
    [(empty? l1)
     ;; AVAILABLE: 
     empty]
    [else
     ;; AVAILABLE:                  l1 : (cons-of A)
     ;;                     (first l1) : A
     ;;                      (rest l1) : (list-of A)
     ;;   (generic-list-fun (rest l1)) : Answer
     (cons (square (first l1))
           (squares (rest l1)))

     ;; FAIL l1 = 1:[]
     ;;     f = 1
     ;;     r = []
     ;;     sqs r = []
     ;;     sq f = 1
     ;;  ret = 1 : []
     ;; ACTUAL = 2 : []
     
     ]))

;; square : num -> num
;; squares it
(define (square x) (* x x))
(check-expect (square 1) 1)
(check-expect (square 2) 4)
;;(check-expect (square 3) 9)

(check-expect (squares empty) empty)
(check-expect (squares (cons 1 empty)) (cons 1 empty))
(check-expect (squares (cons 2 empty)) (cons 4 empty))
(check-expect (squares (list 1 2 3 4)) (list 1 4 9 16))

;; doubles : list-of-num -> list-of-num
;; Compute doubles of those given

(define (doubles l1)
  ;; AVAILABLE: l1 : (list-of A)
  ;;            (empty? l1) : bool
  ;;            (cons? l1) : bool
  ;; FAIL l1 = 1::[]
  (cond
    [(empty? l1)
     ;; AVAILABLE: 
     empty]
    [else
     ;; AVAILABLE:                  l1 : (cons-of A)
     ;;                     (first l1) : A
     ;;                      (rest l1) : (list-of A)
     ;;   (generic-list-fun (rest l1)) : Answer
     (cons (double (first l1))
           (doubles (rest l1)))

     ;; FAIL l1 = 1:[]
     ;;     f = 1
     ;;     r = []
     ;;     sqs r = []
     ;;     sq f = 1
     ;;  ret = 1 : []
     ;; ACTUAL = 2 : []
     
     ]))

;; double : num -> num
;; doubles it
(define (double x) (+ x x))
(check-expect (double 1) 2)
(check-expect (double 2) 4)
(check-expect (double 3) 6)

(check-expect (doubles empty) empty)
(check-expect (doubles (cons 1 empty)) (cons 2 empty))
(check-expect (doubles (cons 2 empty)) (cons 4 empty))
(check-expect (doubles (list 1 2 3 4)) (list 2 4 6 8))


;;;

(define test-list-1 (list 1 2 3 4))
(check-expect (squares test-list-1) (list 1 4 9 16))
(check-expect (doubles test-list-1) (list 2 4 6 8))

;;;;; LEVEL UP to ISL

;; make-function-like-squares-and-doubles
;;   (num -> num)
;;   ->
;;   ((listof num) -> lon)
(define (make-function-like-squares-and-doubles
         something-like-square-or-double)
  (local
    [;; function-like-squares-and-doubles : (listof num) -> lon
     (define (function-like-squares-and-doubles lon)
       (cond
         [(empty? lon)
          empty]
         [(cons? lon)
          (cons (something-like-square-or-double (first lon))
                (function-like-squares-and-doubles (rest lon)))]))]
    function-like-squares-and-doubles))

;; heavenly-doubles : lon -> lon
(define heavenly-doubles
  (make-function-like-squares-and-doubles double))

(check-expect (heavenly-doubles test-list-1) (list 2 4 6 8))

(define heavenly-squares
  (make-function-like-squares-and-doubles square))

(check-expect (heavenly-squares test-list-1) (list 1 4 9 16))

;; make-function-like-squares-and-doubles is called "higher order"

(require 2htdp/image)
;; circulate : num -> image
(define (circulate r)
  (circle r "solid" "red"))

(define circulates
  (make-function-like-squares-and-doubles circulate))

(circulates test-list-1)

;; A MAP

;; map : (A -> B) (listof A) -> (listof B)
(map circulate test-list-1)

;;;;; LEVEL UP to ISL + λ

;; Command-\ = λ
;; Control-\ = λ
(map (λ (r) (circle r "solid" "red"))
     test-list-1)

;; A FOLD

(define (how-many l)
  (cond
    [(empty? l) 0]
    [(cons? l) (+ 1 (how-many (rest l)))]))
(define (all-of-the-stuff l)
  (cond
    [(empty? l) ""]
    [(cons? l) (string-append (number->string (first l))
                              " "
                              (all-of-the-stuff (rest l)))]))

(how-many test-list-1)
(all-of-the-stuff test-list-1)

;; ??? : B (A B -> B) (listof A) -> B
(define (??? ???-empty ???-cons l)
  (cond
    [(empty? l) ???-empty]
    [(cons? l)
     (???-cons
      (first l)
      (??? ???-empty ???-cons (rest l)))]))

(check-expect
 (??? 0 (λ (f ???-of-rest) (+ 1 ???-of-rest)) test-list-1)
 (how-many test-list-1))
(check-expect
 (??? "" (λ (f ???-of-rest) (string-append (number->string f) " " ???-of-rest)) test-list-1)
 (all-of-the-stuff test-list-1))

(cons 1 (cons 2 empty))
;; (???-cons 1 (???-cons 2 ???-empty))

;; ??? = FOLD or REDUCE


;; MAP-REDUCE (where ???-cons is associative)
;; (C 1 (C 2 (C 3 (C 4 E))))
;; =
;; (C (C (C 1 2)
;;       (C 3 4))
;;    E)





