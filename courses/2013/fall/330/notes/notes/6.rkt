#lang plai-typed
(print-only-errors #t)

;;                "This program" should be (toString (parse "This program")) <-- not so good
;; (toString the-data-structure) should be (toString (parse "This program")) <-- good

;; We want subtraction! Give us subtraction!

;; ArithS = [surface]
;; | num
;; | (+ ArithS ArithS)
;; | (* ArithS ArithS)
;; | (- ArithS ArithS)
;; | (- ArithS)

(define-type ArithS
  [numS (val : number)]
  [addS (lhs : ArithS) (rhs : ArithS)]
  [subS (lhs : ArithS) (rhs : ArithS)]
  [multS (lhs : ArithS) (rhs : ArithS)]
  [usubS (rhs : ArithS)])

;; ArithC = [core]
;; | num
;; | (+ ArithC ArithC)
;; | (* ArithC ArithC)

;;; The MORAL argument... for -
;; Not uglies: It's so convenient!
;; Who are programming languages for?
;; - Programmers (writer)
;; - Other poor, pathetic programmers (reader)
;; - Computers (reader... customer... and the customer's always right!)

;; mmap is a UNIX syscall for "memory mapping"
;; memory-map(path) = pointer

;; AGAINST:
;; - Adding - doesn't make language more "expressive"
;; .. expressive means the same values can be computed (ie same algorithms can be run)
;; .. still "turing complete"
;; - The compiler/language writer is valuable too! Don't waste their time!
;;   .... compilers are written "once" and used many many times
;; - Orthogonality from linear algebra is beautiful
;; - Programming downside of adding subC

;; linear equation... ax + by = c ... x y are basis for the vector space
;; if you add z... ax + by + cz ... z is not orthogonal if z = a'x + b'y

(define-type ArithC
  [numC (val : number)]
  [addC (lhs : ArithC) (rhs : ArithC)]
  ;; [subC (lhs : ArithC) (rhs : ArithC)]
  [multC (lhs : ArithC) (rhs : ArithC)])

;; ArithC has N variants and M functions

;; --- the struct trade-off: when you add a variant, you have to
;; change M functions (each function mentions N variants)

;; --- object/class trade-off: when add a function, you have to change
;; N variants (each variant knows about the M functions)

;; Is the "Expression Problem"

;; adding subC .. requires a change in interp and every other arithC
;; consuming function

;; parse : s-exp -> your representation
(define (parse se)
  (cond
    [(s-exp-number? se)
     (numS (s-exp->number se))]
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 3)
          (equal? '+ (s-exp->symbol (first (s-exp->list se)))))
     (addS
      (parse (second (s-exp->list se)))
      (parse (third (s-exp->list se))))]
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 3)
          (equal? '* (s-exp->symbol (first (s-exp->list se)))))
     (multS
      (parse (second (s-exp->list se)))
      (parse (third (s-exp->list se))))]
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 3)
          (equal? '- (s-exp->symbol (first (s-exp->list se)))))
     (subS
      (parse (second (s-exp->list se)))
      (parse (third (s-exp->list se))))]
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 2)
          (equal? '- (s-exp->symbol (first (s-exp->list se)))))
     (usubS
      (parse (second (s-exp->list se))))]
    [else
     (error 'parse "You are wrong")]))

(test (parse '23)
      (numS 23))
(test (parse '(+ 23 5))
      (addS (numS 23) (numS 5)))
(test (parse '(+ 23 (+ 23 5)))
      (addS (numS 23)
             (addS (numS 23) (numS 5))))
(test (parse '(* 23 5))
      (multS (numS 23) (numS 5)))

;; desugar : ArithS -> ArithC
(define (desugar p)
  (type-case 
   ArithS p
   [numS
    (val)
    (numC val)]
   [addS
    (lhs rhs)
    (addC (desugar lhs) (desugar rhs))]
   [multS
    (lhs rhs)
    (multC (desugar lhs) (desugar rhs))]
   [subS
    (lhs rhs)
    (addC (desugar lhs)
          (multC (numC -1) (desugar rhs)))]
   [usubS
    (rhs)
    ;; -b = 0 - b
    ;; (desugar rhs) ; the template tells us to use "structural recursion"
    ;; this v is "generative recursion"
    ;; (desugar (subS (numS 0) rhs))
    ;; -b = -1 * b
    (multC (numC -1) (desugar rhs))]))

;; interp : ArithC -> num
(define (interp p)
  (type-case
   ArithC p
   [numC
    (val)
    val]
   [addC
    (lhs rhs)
    (+ (interp lhs) (interp rhs))]
   [multC
    (lhs rhs)
    (* (interp lhs) (interp rhs))]))

(define (interp* s)
  (interp (desugar (parse s))))

(test (interp* '23)
      23)
(test (interp* '(+ 23 5))
      28)
(test (interp* '(+ 23 (+ 23 5)))
      51)
(test (interp* '(* 23 5))
      115)
(test (interp* '(- 23 5))
      18)
(test (interp* '(- 5))
      -5)
(test (interp* '(- (+ 23 5)))
      -28)
