#lang plai
(print-only-errors #t)

(define-type AE
  [num (n number?)]
  [binop (op procedure?)
         (lhs AE?)
         (rhs AE?)]
  [id (sym symbol?)]
  [with (new-name symbol?)
        (named-thing AE?)
        (body AE?)]
  [app (fun symbol?)
       (arg AE?)])

(define (add lhs rhs)
  (binop + lhs rhs))
(define (sub lhs rhs)
  (binop - lhs rhs))

;; <AE> := <real Racket number>
;;       | (+ <AE> <AE>)
;;       | (- <AE> <AE>)
;;       | <id>
;;       | (with [<id> <AE>] <AE>)
;;       | (<id> <AE>)

;; where <id> is any Racket symbol, except +, -, and with

;; parse :: s-expression -> AE
(define (parse se)
  (cond
    [(and (list? se)
          (= 2 (length se))
          (symbol? (first se))
          (not (member (first se) '(+ - with))))
     (app (first se)
          (parse (second se)))]
    [(and (list? se)
          (= 3 (length se))
          (equal? 'with (first se))
          (list? (second se))
          (= 2 (length (second se)))
          (symbol? (first (second se))))
     (with (first (second se))
           (parse (second (second se)))
           (parse (third se)))]
    [(symbol? se)
     (id se)]
    [(number? se)
     (num se)]
    [(and (list? se)
          (= 3 (length se))
          (equal? '+ (first se)))
     (add (parse (second se))
          (parse (third se)))]
    [(and (list? se)
          (= 3 (length se))
          (equal? '- (first se)))
     (sub (parse (second se))
          (parse (third se)))]
    [else
     (error 'parse "Invalid syntax, dude")]))

(test (parse '1)
      (num 1))
(test (parse '(+ 1 1))
      (add (num 1) (num 1)))
(test (parse '(- 1 1))
      (sub (num 1) (num 1)))
(test (parse 'x)
      (id 'x))
(test (parse '(with [x 27] x))
      (with 'x (num 27) (id 'x)))
(test (parse '(double 5))
      (app 'double (num 5)))

(test/exn (parse "1")
          "Invalid syntax")

(define-type FunDef
  [a-FunDef (name symbol?)
            (arg symbol?)
            (body AE?)])

(define double
  (a-FunDef 'double
            'x
            (parse '(+ x x))))
(define triple
  (a-FunDef 'triple
            'x
            (parse '(+ x (double x)))))

;; lookup-fun : symbol (listof FunDef) -> a-FunDef
(define (lookup-fun the-fun fs)
  (findf (compose (curry symbol=? the-fun)
                  a-FunDef-name)
         fs))

(define-type DefrdSubst
  [mtSub]
  [oneMoreSub
   (name symbol?)
   (named-value number?)
   (everything-else DefrdSubst?)])

(define (lookup-id sym $)
  (type-case
   DefrdSubst $
   [mtSub
    ()
    (error 'calc "You has a bad identifier, bro: ~e" sym)]
   [oneMoreSub
    (name named-value rest)
    (if (symbol=? sym name)
      named-value
      (lookup-id sym rest))]))

;; (define (mtSub)
;;   (hasheq))
;; (define (oneMoreSub name named-value more)
;;   (hash-set more name named-value))
;; (define (lookup-id sym $)
;;   (hash-ref $ sym
;;             (λ ()
;;               (error 'calc "You has a bad identifier, bro: ~e" sym))))

;; calc : AE? (listof FunDef) DefrdSubst -> number?
;; compute the meaning of the AE
(define (calc ae fs $)
  (type-case
   AE ae
   [app
    (fun arg)
    (type-case
     FunDef (lookup-fun fun fs)
     [a-FunDef
      (fun-name arg-name fun-body)
      ;; No programming language:
      ;; (calc (with arg-name arg fun-body) fs (mtSub))
      ;; Dynamic Scope:
      ;; (calc (with arg-name arg fun-body) fs $)
      ;; Static Scope:
      (calc fun-body fs 
            (oneMoreSub arg-name
                        (calc arg fs $)
                        (mtSub)))])]
   [id
    (sym)
    (lookup-id sym $)]
   [with
    (name named-thing body)
    ;; (calc (subst name (num (calc named-thing fs)) body) fs)
    (calc body fs 
          (oneMoreSub name
                      (calc named-thing fs $)
                      $))]
   [num
    (n)
    n]
   [binop
    (op lhs rhs)
    (op (calc lhs fs $)
        (calc rhs fs $))]))

;; calc*/funs : sexpr (listof FunDef) -> number?
(define (calc*/funs se fs)
  (calc (parse se) fs (mtSub)))

;; calc* : sexpr -> number?
(define (calc* se)
  (calc*/funs se empty))

(test (calc* '1)
      1)
(test (calc* '(+ 1 1))
      2)
(test (calc* '(- 0 1))
      -1)

(test (calc* '(with [x (+ 5 5)]
                    (+ x x)))
      20)
(test (calc* '(with [y 7]
                    (with [x y]
                          (+ x x))))
      14)
(test (calc* '(with 
               ;; $_0 = mt
               [x (+ 5 5)]
               ;; $_1 = (oneMoreSub 'x 10 $_0)
               (with [x 7]
                     ;; $_2 = (oneMoreSub 'x 7 $_1)
                     (+ x x))))
      14)
(test (calc* '(with [x (+ 5 5)]
                    (+ x (with [x 7]
                               (+ x x)))))
      24)
(test (calc* '(with 
               ;; $_0 = mt
               [x (+ 5 5)]
               ;; $_1 = (oneMoreSub 'x 10 $_0)
               (+ (with [x 7]
                        ;; $_2 = (oneMoreSub 'x 7 $_1)
                        (+ x x))
                  x)))
      24)
(test (calc* '(with [x 7]
                    (with [y (+ 2 x)]
                          (+ y 3))))
      12)
(test (calc* '(with [y 7]
                    (with [y (+ y 2)]
                          (+ y 3))))
      12)
(test (calc* '(with [x (+ 5 5)]
                    7))
      7)

(test (calc* '(with [x (+ 5 5)]
                    (+ x x)))
      20)
(test (calc* '(with [x (+ 5 6)]
                    (+ x x)))
      22)

(test (calc*/funs '(double (+ 5 5))
                  (list double))
      20)
(test (calc*/funs '(double (+ 5 6))
                  (list double))
      22)

(test (calc*/funs '(triple (+ 5 5))
                  (list triple double))
      30)
(test (calc*/funs '(triple (+ 5 6))
                  (list triple double))
      33)

(test (calc*/funs '(+ 1 (triple (+ 5 6)))
                  (list triple double))
      34)

(test (calc*/funs '(double (triple (+ 5 6)))
                  (list double triple))
      66)


(test (calc*/funs '(with [double 5]
                         (double (triple (+ double 6))))
                  (list double triple))
      66)

;; eval = 111 = n
;; subst = 011 = O(n^2)
(test (calc* '(with [x (+ 5 6)]
                    (+ x x)))
      22)

;; eval = 1011 = n
;; subst = 1100 = O(n^2)
(test (calc* '(with [x (+ 5 6)]
                    (with [y 6]
                          (+ y (+ x x)))))
      28)


(test (calc* '(with [x (with [x (+ 5 6)]
                             (with [y 6]
                                   (+ y (+ x x))))]
                    (with [y 6]
                          (+ y (+ x x)))))
      (+ 6 (* 2 28)))

(test (calc* '(with [x (+ 5 6)]
                    (with [x x]
                          (+ x x))))
      22)

(test/exn (calc* '(with [x 6]
                        y))
          "bro")

(test (calc*/funs '(with [x 5]
                         (f 17))
                  (list (a-FunDef 'f 'x (parse '(+ x 5)))))
      22)
(test (calc*/funs '(with [x 5]
                         (f 17))
                  (list (a-FunDef 'f 'n (parse '(+ n 5)))))
      22)
(test/exn
 (calc*/funs '(with [x 5]
                    (f 17))
             (list (a-FunDef 'f 'n (parse '(+ n z)))))
 "bro")
(test/exn
 (calc*/funs '(with [x 5]
                    (f 17))
             (list (a-FunDef 'f 'n (parse '(+ n x)))))
 "bro")
