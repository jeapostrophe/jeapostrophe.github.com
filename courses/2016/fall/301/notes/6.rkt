#lang plai-typed

(define (double x) (+ x x))

;(double (+ y 4)) == 4

;;substitution 


(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [idC (s : symbol)]
  [appC (fun : symbol)  (arg : ExprC)])

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

;; subtC
;; when you call interp on it, it computes l - r
(define (subtC [l : ExprC] [r : ExprC]) : ExprC
  (plusC l (multC (numC -1) r)))

(define (negC [o : ExprC]) : ExprC
  (subtC (numC 0) o))

;; parse : s-expression -> ExprC
;; Use a built-in Racket parser
(define (parse [s : s-expression]) : ExprC
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

;; interp : ExprC -> number
;; Tell what the AE means
(define (interp [ae : ExprC] [fds : (listof FunDefC)]) : number
  (type-case
      ExprC ae
    [numC
     (n)
     n]
    [idC (_) (error 'interp "shouldn't get here")] 
    [plusC
     (l r)
     (+ (interp l fds)
        (interp r fds))]
    [multC
     (l r)
     (* (interp l fds)
        (interp r fds))]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (subst a
                                 (fdC-arg fd)
                                 (fdC-body fd)) fds))]))
    
    ; get-fundef : symbol * (listof FunDefC) -> fdC
    
    (define (get-fundef [s : symbol] [fds : (listof FunDefC)]) : FunDefC 
      (cond
        [(empty? fds) (error 'get-fundef "function definition not found")]
        [(eq? s (fdC-name (first fds))) (first fds)]
        [else (get-fundef s (rest fds))]))
    
    ;subst : ExprC * symbol * ExprC -> ExprC
    (define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
      (type-case ExprC in
        [numC (n) in]
        [idC (s) (cond
                   [(symbol=? s for) what]
                   [else in])]
        [appC (f a) (appC f (subst what for a))]
        [plusC (l r) (plusC (subst what for l)
                            (subst what for r))]
        [multC (l r) (multC (subst what for l)
                            (subst what for r))]))
    
    (test (subst (numC 2) 'x (plusC (idC 'x) (numC 2))) (plusC (numC 2) (numC 2)))


    
    (define (pip [s : s-expression]) : number
      (interp (parse s) empty))
    
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