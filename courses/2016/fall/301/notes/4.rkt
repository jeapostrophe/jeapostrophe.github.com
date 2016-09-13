#lang plai-typed

;; AE (Arithmetic Expressions)
;;   1 + 1
;;     => 2
;;   2 * 3
;;     => 6
;;   (1 + 1) * 3
;;     => 6
;;   1 + 1 * 3
;;     => 4

;; C:    "expr ? expr : expr"
;; C:    "expr + expr"
;; C:    "type name = expr;"

;; C:    "1 + ;"

;; parse : AE -> AST(AE)
;; Ignored in this course.


;; An Abstract Syntax Tree (AST) of AE
;; is either a
;;  - (make-numC <number>)
;;  - (make-plus AST(AE) AST(AE))
;;  - (make-times AST(AE) AST(AE))
(define-type AST-AE
  [numC (n : number)]
  [plus (l : AST-AE) (r : AST-AE)]
  [times (l : AST-AE) (r : AST-AE)])

#;
(cond
  [(numC? a) ... look at (numC-n a) ....]
  [(plus? a) ... look at (plus-l a) (plus-r a) ...]
  [(times? a) ... look at (times-l a) (times-r a) ...])

(define ex1
  ;; "1 + (1 * 3)"
  ;; "(+ 1 (* 1 3))"
  ;; "1 1 3 * +"
  (plus
   (numC 1)
   (times (numC 1)
          (numC 3))))

;; parse : Quasi(AE) -> AST(AE)
;; Use a built-in Racket parser
(define (parse [s : s-expression]) : AST-AE
  (cond
    [(s-exp-number? s)
     (numC (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       ;; ((+ 1 2) 3)
       ;; Good programmers, put checks to see
       ;; if (first sl) is s-exp-symbol?
       (case (s-exp->symbol (first sl))
         ;; (+ 1 2)
         [(+) (plus (parse (second sl))
                    (parse (third sl)))]
         [(*) (times (parse (second sl))
                     (parse (third sl)))]
         [else
          (error 'parse "Not implemented")]))]
    [else
     (error 'parse "Go back to C, newb")]))

4
'+
'times
'jay
'(+ 2 2)
'(+ 3 4 5)

'(+ 3 jay)

(test (parse '1) (numC 1))
(test (parse '3) (numC 3))
(test (parse '(* 1 3)) (times (numC 1) (numC 3)))
(test (parse '(+ 1 (* 1 3))) ex1)
(test (parse '(+ 1 2 3)) (plus (numC 1) (numC 2)))

;; interp : AST(AE) -> number
;; Tell what the AE means
(define (interp [ae : AST-AE]) : number
  (type-case
   AST-AE ae
   #;[(numC? ae) ... (numC-n ae) ...]
   #;[ numC  (n) ...       n     ...]
   [numC
    (n)
    n]
   [plus
    (l r)
    (+ (interp l)
       (interp r))]
   [times
    (l r)
    (* (interp l)
       (interp r))]
   #;
   [else
    (error 'interp "What me worry?")]))

;;          C: 1 + x
;; x could be anything

;; int x = INT_MAX;
;; return x + 1;
;; ==>
;; return uninitialized_memory;

;;       Java: 1 + x
;; x must be number (one kind)
;; wrapping overflow

;;     Racket: (+ 1 x)
;; x must be number (any kind)
;; doesn't overflow

;; Javascript: 1 + x
;; WAT???

;; int data[5][5] = {
;;   {0, 1, 2, 3, 4},
;;   {2, 3, 4, 6, 7}, ...}



;; (test (interp "1 + 1") 2)

(define (pip [s : s-expression]) : number
  (interp (parse s)))

(test (pip '1) 1)
(test (pip '3) 3)
(test (pip '(* 1 3)) 3)
(test (pip '(+ 1 (* 1 3))) 4)
(test (pip '(+ 1 2 3)) 3)
(test (pip '(+ (+ (+ (+ 1 1)
                     (+ 1 1))
                  (+ (+ 1 1)
                     (+ 1 1)))
               (+ (+ (+ 1 1)
                     (+ 1 1))
                  (+ (+ 1 1)
                     (+ 1 1)))))
      16)
(test (pip '(* (* 2 2) (* 2 2)))
      16)

;; Kinds of "Semantics"
;; - Ad-hoc or by implementation
;; - Interpretation

;; - Operational
;;   Define a program state space
;;   Define a transition relation
;;   The meaning is the transitive closure of the transition relation

;; - Denotational







