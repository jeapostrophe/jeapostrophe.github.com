#lang plai-typed

;; Parsing

;; What is parsing?
;; - getting info from strings
;; - breaking one thing into little things
;; - finding patterns in set of data
;; - analyze the pieces of info
;; - determine if a string is in a language
;; - top-down vs bottom-up parsing
;; - LL(k) and LR
;; - yacc or bison or ANTLR
;; - 236
;; - parsing is easy or hard? simple and mundane, boilerplate, unfulfilling?
;; - well-defined grammar => easy for computer (no ambiguity)
;;

;; posn *x;
;; for ( ; ; ;) { }
;; while () { }

;; A program:
"23 + 5 - 6"
"23 + (5 - 6)"
"(23 + 5) - 6"

;; A program representation:

;; A plus function call and a minus function call

;; A plus function call with a 23 on the left and a minus function
;; called with 5 on the left and 6 on the right on the right

;; A plus function call with a 23 on the left and (a minus function
;; called with 5 on the left and 6 on the right) on the right

;; new plus( new num(23), new subtract( new num(5), new num(6)))

;; JSON (2008)
;; { type: '+',
;;   lhs: { type: 'num', val: 23 },
;;   rhs: { type: '-',
;;          lhs: { type: 'num', val: 5 },
;;          rhs: { type: 'num', val: 6 }
;;        }
;; }

;; XML (1994)
;; <+><lhs><num><val>23</val></num></lhs>....

;; S-expression (1960)
(+ 23 (- 5 6))

;; quote
(quote (+ 23 (- 5 6)))
'(+ 23 (- 5 6))

;; An S-expression is either
;; - a list of S-expressions
;; - a symbol
;; - a number

;; (Symbol)(((List)quotedthing)[0])
(test (s-exp->symbol
       (first (s-exp->list '(+ 23 (- 5 6)))))
      '+)
(test (s-exp->number
       (second (s-exp->list '(+ 23 (- 5 6)))))
      23)

'(+ 1 2 3)

;; AE =
;; | num
;; | (+ AE AE)
;; | (- AE AE)

(define-type AE
  [numAE (val : number)]
  [addAE (lhs : AE) (rhs : AE)]
  [subAE (lhs : AE) (rhs : AE)])

;; parse : s-exp -> your representation
(define (parse se)
  (cond
    [(s-exp-number? se)
     (numAE (s-exp->number se))]
    [(s-exp-list? se)
     (cond
       [(equal? '+ (s-exp->symbol
                    (first (s-exp->list se))))
        (addAE
         (parse (second (s-exp->list se)))
         (parse (third (s-exp->list se))))]
       [(equal? '- (s-exp->symbol
                    (first (s-exp->list se))))
        (subAE
         (parse (second (s-exp->list se)))
         (parse (third (s-exp->list se))))]
       [else
        (error 'parse "You are wrong, again. Loser.")])]
    [else
     (error 'parse "You are wrong")]))

(test (parse '23)
      (numAE 23))
(test (parse '(+ 23 5))
      (addAE (numAE 23) (numAE 5)))
(test (parse '(+ 23 (+ 23 5)))
      (addAE (numAE 23)
             (addAE (numAE 23) (numAE 5))))
(test (parse '(- 23 5))
      (subAE (numAE 23) (numAE 5)))
(test (parse '(+ 23 5 7))
      (addAE (numAE 23) (numAE 5)))
