#lang plai-typed
(print-only-errors #t)

;; AE =
;; | num
;; | (+ AE AE)
;; | (* AE AE)

(define-type AE
  [numAE (val : number)] ;; variant clause
  [addAE (lhs : AE) (rhs : AE)]
  [multAE (lhs : AE) (rhs : AE)])

;; (define (which-one s)
;;   (if (symbol=? s '+)
;;     +
;;     -))

;; parse : s-exp -> your representation
(define (parse se)
  (cond
    [(s-exp-number? se)
     (numAE (s-exp->number se))]
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 3)
          (equal? '+ (s-exp->symbol (first (s-exp->list se)))))
     (addAE
      (parse (second (s-exp->list se)))
      (parse (third (s-exp->list se))))]
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 3)
          (equal? '* (s-exp->symbol (first (s-exp->list se)))))
     (multAE
      (parse (second (s-exp->list se)))
      (parse (third (s-exp->list se))))]
    [else
     (error 'parse "You are wrong")]))

(test (parse '23)
      (numAE 23))
(test (parse '(+ 23 5))
      (addAE (numAE 23) (numAE 5)))
(test (parse '(+ 23 (+ 23 5)))
      (addAE (numAE 23)
             (addAE (numAE 23) (numAE 5))))
(test (parse '(* 23 5))
      (multAE (numAE 23) (numAE 5)))

;; (parse "x + y")
;; (addAE (idAE 'x) (idAE 'y))

;; (calc (addAE (idAE 'x) (idAE 'y)))

;; Parsed a program and got a representation

;; Run the program
;; Decide if the program halts [analysis]
;; Make it more efficient (optimize it) [analysis]
;; What does it DO? [analysis]
;; Run = Do because you can observe it doing it[*]
;; Test the program - list of inputs and make sure outputs are expected [analysis, run]
;; Testing is a big thing, including type checking, maybe not running?
;; Print it out (even with more whitespace)
;; Compile it -- is this just optimizing? maybe not, localize to arch? [analysis]
;; Most compilers happen to include optimization, but not necessarily
;; Compile is translating to a different language (even to X86)
;; What is an interpreter? - Runs it (program -> value/effect)

;; (define (interp program)
;;   (X86 (compile-to-X86 program)))

;; loop unrolling = pipeline + SIMD (single instruction mutliple data)

;; What a program does, by Jay McCarthy with help from 330 peons
;; ... if you know the formula (formula = math)
;; ... do formulas include time/efficiency/etc?
;; ... how = back channel analysis

;; Meaning = Semantics
;;; Mathematical/Formula = Denotational semantics
;;; What it does (written in Math) = Operational Semantics
;;; What it does (written in meta language) = Interpreter Semantics

;; interp : AE -> num
(define (interp p)
  (type-case
   AE p
   [numAE
    (val)
    val]
   [addAE
    (lhs rhs)
    (+ (interp lhs) (interp rhs))]
   [multAE
    (lhs rhs)
    (* (interp lhs) (interp rhs))]))

(test (interp (parse '23))
      23)
(test (interp (parse '(+ 23 5)))
      28)
(test (interp (parse '(+ 23 (+ 23 5))))
      51)
(test (interp (parse '(* 23 5)))
      115)

;; RTFM
;; + means what it means in Racket

;; "1" + "2" = 3
;; "one" + "two" = "onetwo"
