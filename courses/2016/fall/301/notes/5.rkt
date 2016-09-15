#lang plai-typed

;;; Code from Day 4 ;;;
(module+ day4
  ;; AST for the Arithmetic Computation language
  (define-type ArithC
    [numC (n : number)]
    [plusC (l : ArithC) (r : ArithC)]
    [multC (l : ArithC) (r : ArithC)])

  ;; parse : s-expression -> ArithC
  ;; Use a built-in Racket parser
  (define (parse [s : s-expression]) : ArithC
    (cond
      [(s-exp-number? s)
       (numC (s-exp->number s))]
      [(s-exp-list? s)
       (let ([sl (s-exp->list s)])
         (cond
           [(and (= 3 (length sl))
                 (s-exp-symbol? (first sl)))
            (case (s-exp->symbol (first sl))
              [(+)
               (plusC (parse (second sl))
                      (parse (third sl)))]
              [(*)
               (multC (parse (second sl))
                      (parse (third sl)))]
              [else
               (error 'parse "Not implemented")])]
           [else
            (error 'parse "Go back to C, newb")]))]
      [else
       (error 'parse "Go back to C, newb")]))

  (test (parse '1)
        (numC 1))
  (test (parse '3)
        (numC 3))
  (test (parse '(* 1 3))
        (multC (numC 1) (numC 3)))
  (test (parse '(+ 1 (* 1 3)))
        (plusC (numC 1) (multC (numC 1) (numC 3))))

  ;; interp : ArithC -> number
  ;; Tell what the AE means
  (define (interp [ae : ArithC]) : number
    (type-case
     ArithC ae
     [numC
      (n)
      n]
     [plusC
      (l r)
      (+ (interp l)
         (interp r))]
     [multC
      (l r)
      (* (interp l)
         (interp r))]))

  (define (pip [s : s-expression]) : number
    (interp (parse s)))

  (test (pip '1) 1)
  (test (pip '3) 3)
  (test (pip '(* 1 3)) 3)
  (test (pip '(+ 1 (* 1 3))) 4)
  (test (pip '(+ 1 2)) 3)
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
        16))

;; Day 4 with subtraction
(module+ day4-try-one
  ;; AST for the Arithmetic Computation language
  (define-type ArithC
    [numC (n : number)]
    [plusC (l : ArithC) (r : ArithC)]
    [multC (l : ArithC) (r : ArithC)]
    [subtC (l : ArithC) (r : ArithC)])

  ;; parse : s-expression -> ArithC
  ;; Use a built-in Racket parser
  (define (parse [s : s-expression]) : ArithC
    (cond
      [(s-exp-number? s)
       (numC (s-exp->number s))]
      [(s-exp-list? s)
       (let ([sl (s-exp->list s)])
         (cond
           [(and (= 3 (length sl))
                 (s-exp-symbol? (first sl)))
            (case (s-exp->symbol (first sl))
              [(+)
               (plusC (parse (second sl))
                      (parse (third sl)))]
              [(*)
               (multC (parse (second sl))
                      (parse (third sl)))]
              [(-)
               (subtC (parse (second sl))
                      (parse (third sl)))]
              [else
               (error 'parse "Not implemented")])]
           [else
            (error 'parse "Go back to C, newb")]))]
      [else
       (error 'parse "Go back to C, newb")]))

  (test (parse '1)
        (numC 1))
  (test (parse '3)
        (numC 3))
  (test (parse '(* 1 3))
        (multC (numC 1) (numC 3)))
  (test (parse '(+ 1 (* 1 3)))
        (plusC (numC 1) (multC (numC 1) (numC 3))))

  ;; interp : ArithC -> number
  ;; Tell what the AE means
  (define (interp [ae : ArithC]) : number
    (type-case
     ArithC ae
     [numC
      (n)
      n]
     [plusC
      (l r)
      (+ (interp l)
         (interp r))]
     [multC
      (l r)
      (* (interp l)
         (interp r))]
     [subtC
      (l r)
      (- (interp l)
         (interp r))]))

  (define (pip [s : s-expression]) : number
    (interp (parse s)))

  (test (pip '1) 1)
  (test (pip '3) 3)
  (test (pip '(* 1 3)) 3)
  (test (pip '(+ 1 (* 1 3))) 4)
  (test (pip '(+ 1 2)) 3)
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

  (test (pip '(- 8 1))
        7)
  (test (pip '(- (+ 4 4) (* 1 1)))
        7))

;; Idea -> New Feature -> New Parser, AST, and Runtime

;; Day 4 with subtraction and NO new runtime
(module+ main-trytwo
  ;; AST for the Arithmetic Computation language
  (define-type ArithS
    [numS (n : number)]
    [plusS (l : ArithS) (r : ArithS)]
    [multS (l : ArithS) (r : ArithS)]
    [subtS (l : ArithS) (r : ArithS)]
    [negS (o : ArithS)])

  (define-type ArithC
    [numC (n : number)]
    [plusC (l : ArithC) (r : ArithC)]
    [multC (l : ArithC) (r : ArithC)])

  ;; parse : s-expression -> ArithC
  ;; Use a built-in Racket parser
  (define (parse [s : s-expression]) : ArithS
    (cond
      [(s-exp-number? s)
       (numS (s-exp->number s))]
      [(s-exp-list? s)
       (let ([sl (s-exp->list s)])
         (cond
           [(and (= 2 (length sl))
                 (s-exp-symbol? (first sl)))
            (case (s-exp->symbol (first sl))
              [(-)
               (negS (parse (second sl)))]
              [else
               (error 'parse "No other unaries")])]
           [(and (= 3 (length sl))
                 (s-exp-symbol? (first sl)))
            (case (s-exp->symbol (first sl))
              [(+)
               (plusS (parse (second sl))
                      (parse (third sl)))]
              [(*)
               (multS (parse (second sl))
                      (parse (third sl)))]
              [(-)
               (subtS (parse (second sl))
                      (parse (third sl)))]
              [else
               (error 'parse "Not implemented")])]
           [else
            (error 'parse "Go back to C, newb")]))]
      [else
       (error 'parse "Go back to C, newb")]))

  ;; interp : ArithC -> number
  ;; Tell what the AE means
  (define (interp [ae : ArithC]) : number
    (type-case
     ArithC ae
     [numC
      (n)
      n]
     [plusC
      (l r)
      (+ (interp l)
         (interp r))]
     [multC
      (l r)
      (* (interp l)
         (interp r))]))

  (define (desugar [s : ArithS]) : ArithC
    (type-case
     ArithS s
     [numS
      (n)
      (numC n)]
     [plusS
      (l r)
      (plusC (desugar l) (desugar r))]
     [multS
      (l r)
      (multC (desugar l) (desugar r))]

     ;; A form of "stratification"
     ;; Syntax with sub + neg
     ;; Syntax with just sub
     ;; Core Syntax
     
     ;; (- l r) => (+ l (* -1 r))
     [subtS
      (l r)
      (plusC (desugar l)
             (multC (numC -1) (desugar r)))]
     ;; (- o) => (- 0 o)
     [negS
      (o)
      ;; This desugar grows in size
      ;; Not obviously terminating
      #;(desugar
         (subtS (numS 0)
                o))
      ;; (- o) => (* -1 o)
      (multC (numC -1)
             (desugar o))]))

  ;; The best three languages
  ;; 1. Racket
  ;; 2. Coq
  ;; 3. C

  (define (pip [s : s-expression]) : number
    (interp (desugar (parse s))))

  (test (pip '1) 1)
  (test (pip '3) 3)
  (test (pip '(* 1 3)) 3)
  (test (pip '(+ 1 (* 1 3))) 4)
  (test (pip '(+ 1 2)) 3)
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

  (test (pip '(- 8 1))
        7)
  (test (pip '(- (+ 4 4) (* 1 1)))
        7)
  (test (pip '(+ 6 (- 8)))
        -2))

;; Day 4 with subtraction, no Runtime no AST
(module+ day4-try-three
  ;; AST for the Arithmetic Computation language
  (define-type ArithC
    [numC (n : number)]
    [plusC (l : ArithC) (r : ArithC)]
    [multC (l : ArithC) (r : ArithC)])

  ;; subtC
  ;; when you call interp on it, it computes l - r
  (define (subtC [l : ArithC] [r : ArithC]) : ArithC
    (plusC l (multC (numC -1) r)
           ;; Doesn't terminate
           #;(negC r)))
  (define (negC [o : ArithC]) : ArithC
    (subtC (numC 0) o))
  
  ;; parse : s-expression -> ArithC
  ;; Use a built-in Racket parser
  (define (parse [s : s-expression]) : ArithC
    (cond
      [(s-exp-number? s)
       (numC (s-exp->number s))]
      [(s-exp-list? s)
       (let ([sl (s-exp->list s)])
         (cond
           [(and (= 3 (length sl))
                 (s-exp-symbol? (first sl)))
            (case (s-exp->symbol (first sl))
              [(+)
               (plusC (parse (second sl))
                      (parse (third sl)))]
              [(*)
               (multC (parse (second sl))
                      (parse (third sl)))]
              [(-)
               (subtC (parse (second sl))
                      (parse (third sl)))]
              [else
               (error 'parse "Not implemented")])]
           [else
            (error 'parse "Go back to C, newb")]))]
      [else
       (error 'parse "Go back to C, newb")]))

  (test (parse '1)
        (numC 1))
  (test (parse '3)
        (numC 3))
  (test (parse '(* 1 3))
        (multC (numC 1) (numC 3)))
  (test (parse '(+ 1 (* 1 3)))
        (plusC (numC 1) (multC (numC 1) (numC 3))))

  ;; interp : ArithC -> number
  ;; Tell what the AE means
  (define (interp [ae : ArithC]) : number
    (type-case
     ArithC ae
     [numC
      (n)
      n]
     [plusC
      (l r)
      (+ (interp l)
         (interp r))]
     [multC
      (l r)
      (* (interp l)
         (interp r))]))

  (define (pip [s : s-expression]) : number
    (interp (parse s)))

  (test (pip '1) 1)
  (test (pip '3) 3)
  (test (pip '(* 1 3)) 3)
  (test (pip '(+ 1 (* 1 3))) 4)
  (test (pip '(+ 1 2)) 3)
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

  (test (pip '(- 8 1))
        7)
  (test (pip '(- (+ 4 4) (* 1 1)))
        7))

;; Example of desugar + parser extension using Racket
#;(module+ main
  (define-syntax-rule (subtract l r)
    (+ l (* -1 r)))

  (subtract 3 4)
  )
