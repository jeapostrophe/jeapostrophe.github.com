#lang plai-typed
(print-only-errors #t)

;; Parser

;; ExprC =
;; | num
;; | (+ ExprC ExprC)
;; | (- ExprC ExprC)
;; | (* ExprC ExprC)
;; | id
;; | (ExprC ExprC)
;; | (λ (id) ExprC)
;; | (box ExprC)
;; | (unbox ExprC)
;; | (set-box! ExprC ExprC)
;; | (begin ExprC ExprC)
;; | (set! id ExprC)
;; | (if0 ExprC ExprC ExprC)
;; | undefined
;; <de-sugared stuff>
;; | (let ([id ExprC]) ExprC)
;; | (letrec ([id ExprC]) ExprC)

(define-type ExprC
  [undefinedC]
  [numC (val : number)]
  [binC (op : (number number -> number))
        (lhs : ExprC) (rhs : ExprC)]
  [idC (id : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [boxC (inite : ExprC)]
  [unboxC (boxe : ExprC)]
  [set-boxC (boxe : ExprC) (vale : ExprC)]
  [beginC (fste : ExprC) (snde : ExprC)]
  [setC (var : symbol) (vale : ExprC)]
  [if0C (teste : ExprC) (truee : ExprC) (falsee : ExprC)])

(define (parse [se : s-expression]) : ExprC
  (cond
    [(and (s-exp-symbol? se)
          (eq? (s-exp->symbol se) 'undefined))
     (undefinedC)]
    ;; (let ([id ExprC]) ExprC)
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 3)
          (s-exp-symbol? (first (s-exp->list se)))
          (equal? 'let (s-exp->symbol (first (s-exp->list se))))
          (s-exp-list? (second (s-exp->list se)))
          (= 1 (length (s-exp->list (second (s-exp->list se)))))
          (s-exp-list? (first (s-exp->list (second (s-exp->list se)))))
          (= 2 (length (s-exp->list (first (s-exp->list (second (s-exp->list se)))))))
          (s-exp-symbol? (first (s-exp->list (first (s-exp->list (second (s-exp->list se))))))))
     (appC (lamC (s-exp->symbol (first (s-exp->list (first (s-exp->list (second (s-exp->list se)))))))
                 (parse (third (s-exp->list se))))
           (parse (second (s-exp->list (first (s-exp->list (second (s-exp->list se))))))))]
    ;; (letrec ([id ExprC]) ExprC)
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 3)
          (s-exp-symbol? (first (s-exp->list se)))
          (equal? 'letrec (s-exp->symbol (first (s-exp->list se))))
          (s-exp-list? (second (s-exp->list se)))
          (= 1 (length (s-exp->list (second (s-exp->list se)))))
          (s-exp-list? (first (s-exp->list (second (s-exp->list se)))))
          (= 2 (length (s-exp->list (first (s-exp->list (second (s-exp->list se)))))))
          (s-exp-symbol? (first (s-exp->list (first (s-exp->list (second (s-exp->list se))))))))

     ;; (letrec ([id id-e]) body-e)
     ;; =>
     ;; (let ([id 0]) (begin (set! id id-e) body-e))
     (local [(define id (first (s-exp->list (first (s-exp->list (second (s-exp->list se)))))))
             (define body-e (third (s-exp->list se)))
             (define id-e (second (s-exp->list (first (s-exp->list (second (s-exp->list se)))))))]
            (parse `(let ([,id undefined]) (begin (set! ,id ,id-e) ,body-e))))]
    [(s-exp-number? se)
     (numC (s-exp->number se))]
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 4)
          (equal? 'if0 (s-exp->symbol (first (s-exp->list se)))))
     (if0C
      (parse (second (s-exp->list se)))
      (parse (third (s-exp->list se)))
      (parse (fourth (s-exp->list se))))]
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 3)
          (equal? '+ (s-exp->symbol (first (s-exp->list se)))))
     (binC
      +
      (parse (second (s-exp->list se)))
      (parse (third (s-exp->list se))))]
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 3)
          (equal? '- (s-exp->symbol (first (s-exp->list se)))))
     (binC
      -
      (parse (second (s-exp->list se)))
      (parse (third (s-exp->list se))))]
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 2)
          (s-exp-symbol? (first (s-exp->list se)))
          (equal? 'unbox (s-exp->symbol (first (s-exp->list se)))))
     (unboxC
      (parse (second (s-exp->list se))))]
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 2)
          (s-exp-symbol? (first (s-exp->list se)))
          (equal? 'box (s-exp->symbol (first (s-exp->list se)))))
     (boxC
      (parse (second (s-exp->list se))))]
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 3)
          (equal? 'set-box! (s-exp->symbol (first (s-exp->list se)))))
     (set-boxC
      (parse (second (s-exp->list se)))
      (parse (third (s-exp->list se))))]
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 3)
          (equal? 'set! (s-exp->symbol (first (s-exp->list se))))
          (s-exp-symbol? (second (s-exp->list se))))
     (setC
      (s-exp->symbol (second (s-exp->list se)))
      (parse (third (s-exp->list se))))]
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 3)
          (equal? 'begin (s-exp->symbol (first (s-exp->list se)))))
     (beginC
      (parse (second (s-exp->list se)))
      (parse (third (s-exp->list se))))]
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 3)
          (equal? '* (s-exp->symbol (first (s-exp->list se)))))
     (binC
      *
      (parse (second (s-exp->list se)))
      (parse (third (s-exp->list se))))]
    [(s-exp-symbol? se)
     (idC (s-exp->symbol se))]
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 2))
     (appC
      (parse (first (s-exp->list se)))
      (parse (second (s-exp->list se))))]
    ;; | (λ (id) ExprC)
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 3)
          (symbol=? (s-exp->symbol (first (s-exp->list se))) 'λ)
          (s-exp-list? (second (s-exp->list se))))
     (lamC (s-exp->symbol (first (s-exp->list (second (s-exp->list se))))) (parse (third (s-exp->list se))))]
    [else
     (begin
       (display se)
       (error 'parse "You are wrong"))]))

(test (parse '(λ (x) (+ x 1)))
      (lamC 'x (binC + (idC 'x) (numC 1))))
(test (parse '((λ (x) (+ x x)) 13))
      (appC (lamC 'x (binC + (idC 'x) (idC 'x))) (numC 13)))

(test (parse '23)
      (numC 23))
(test (parse '(+ 23 5))
      (binC + (numC 23) (numC 5)))
(test (parse '(+ 23 (+ 23 5)))
      (binC + (numC 23)
            (binC + (numC 23) (numC 5))))
(test (parse '(* 23 5))
      (binC * (numC 23) (numC 5)))
(test (parse (symbol->s-exp 'x))
      (idC 'x))

(test (parse '(let ([x (box 0)])
                (let ([f
                       (λ (ignored)
                         (begin (set-box! x (+ 1 (unbox x)))
                                (unbox x)))])
                  (+ (f 0) (f 0)))))
      (appC (lamC 'x
                  (appC
                   (lamC 'f
                         (binC +
                               (appC (idC 'f) (numC 0))
                               (appC (idC 'f) (numC 0))))
                   (lamC 'ignored
                         (beginC (set-boxC (idC 'x)
                                           (binC +
                                                 (numC 1)
                                                 (unboxC
                                                  (idC 'x))))
                                 (unboxC (idC 'x))))))
            (boxC (numC 0))))

;; Interpreter
(define-type-alias Address number)

(define-type Binding
  [bind (name : symbol) (addr : Address)])
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Cell
  [cell (addr : Address) (val : Value)])
(define-type-alias Memory (listof Cell))
(define mt-mem empty)
(define extend-mem cons)

(define (extend-mem/replace [c : Cell] [mem : Memory])
  : Memory
  (cond
    [(empty? mem)
     (error 'extend-mem/replace "address not alloced")]
    [(= (cell-addr c) (cell-addr (first mem)))
     (cons c (rest mem))]
    [else
     (cons (first mem)
           (extend-mem/replace c (rest mem)))]))

(define-type Value
  [undefinedV]
  [numV (n : number)]
  [boxV (a : Address)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

(define (lookup [id : symbol] [env : Env]) : Address
  (cond
    [(empty? env)
     (error 'lookup "undefined identifier")]
    [(symbol=? id (bind-name (first env)))
     (bind-addr (first env))]
    [else
     (lookup id (rest env))]))

(test/exn (lookup 'x empty) "undefined")
(test (lookup 'x (list (bind 'x 5))) 5)

(define (deref [a : Address] [mem : Memory]) : Value
  (cond
    [(empty? mem)
     (error 'deref "SEGFAULT")]
    [(= a (cell-addr (first mem)))
     (cell-val (first mem))]
    [else
     (deref a (rest mem))]))

(define (max-address [mem : Memory]) : Address
  (cond
    [(empty? mem)
     0]
    [else
     (max (cell-addr (first mem))
          (max-address (rest mem)))]))

(define (malloc [mem : Memory]) : Address
  (add1 (max-address mem)))

(define (interp [p : ExprC] [env : Env] [mem : Memory])
  : (Value * Memory)
  (type-case
   ExprC p
   [undefinedC
    ()
    (values (undefinedV) mem)]
   [numC
    (val)
    (values (numV val) mem)]
   [binC
    (op lhs rhs)
    (local
     [(define-values (lhs-v lhs-mem) (interp lhs env mem))
      (define-values (rhs-v rhs-mem) (interp rhs env lhs-mem))]
     (values (numV (op (numV-n lhs-v)
                       (numV-n rhs-v)))
             rhs-mem))]
   [idC
    (id)
    (values (deref (lookup id env) mem) mem)]
   [appC
    (fun arg)
    (local
     [(define-values (f f-mem) (interp fun env mem))
      (define-values (arg-v arg-mem) (interp arg env f-mem))
      (define arg-addr (malloc arg-mem))]
     (interp (closV-body f)
             (extend-env
              (bind (closV-arg f) arg-addr)
              (closV-env f))
             (extend-mem
              (cell arg-addr arg-v)
              arg-mem)))]
   [lamC
    (arg body)
    (values (closV arg body env) mem)]
   [boxC
    (inite)
    (local
     [(define-values (init-v init-mem)
        (interp inite env mem))
      (define new-addr (malloc init-mem))]
     (values (boxV new-addr)
             (extend-mem
              (cell new-addr init-v)
              init-mem)))]
   [unboxC
    (boxe)
    (local
     [(define-values (boxe-v boxe-mem)
        (interp boxe env mem))]
     (values (deref (boxV-a boxe-v) boxe-mem)
             boxe-mem))]
   [set-boxC
    (boxe vale)
    (local
     [(define-values (boxe-v boxe-mem)
        (interp boxe env mem))
      (define-values (vale-v vale-mem)
        (interp vale env boxe-mem))]
     (values vale-v
             (extend-mem/replace
              (cell (boxV-a boxe-v)
                    vale-v)
              vale-mem)))]
   [beginC
    (fste snde)
    (local
     [(define-values (fst-v fst-mem) (interp fste env mem))]
     (interp snde env fst-mem))]
   [setC
    (var vale)
    (local
     [(define-values (vale-v vale-mem)
        (interp vale env mem))]
     (values vale-v
             (extend-mem/replace
              (cell (lookup var env)
                    vale-v)
              vale-mem)))]
   [if0C
    (teste truee falsee)
    (local
     [(define-values (test-v test-mem) (interp teste env mem))]
     (if (zero? (numV-n test-v))
       (interp truee env test-mem)
       (interp falsee env test-mem)))]))

;; Tests

;; interp* : s-exp -> number
(define (interp* [s : s-expression]) : Value
  (local
   [(define-values (s-v s-mem)
      (interp (parse s) mt-env mt-mem))]
   s-v))

(test (interp* '(λ (x) 3))
      (closV 'x (numC 3) mt-env))

(test (interp* '23)
      (numV 23))
(test (interp* '(+ 23 5))
      (numV 28))
(test (interp* '(+ 23 (+ 23 5)))
      (numV 51))
(test (interp* '(* 23 5))
      (numV 115))
(test (interp* '((λ (x) 5) 13))
      (interp* '5))
(test (interp* '((λ (x) (+ x x)) 13))
      (interp* '(+ 13 13)))
(test (interp* '(((λ (x)
                    (λ (y)
                      (+ y 2)))
                  4)
                 5))
      (numV 7))
(test (interp* '(((λ (x)
                    (λ (y)
                      (+ y x)))
                  4)
                 5))
      (numV 9))

(test/exn (interp* '(+ x 5))
          "undefined")

(test (interp* '(unbox (box 5)))
      (numV 5))
(test (interp* '(let ([x (box 5)]) (unbox x)))
      (numV 5))
(test (interp* '(let ([xs (box (box 6))]) (unbox (unbox xs))))
      (numV 6))
(test (interp*
       '(let ([x (box 5)]) (begin (set-box! x 6) (unbox x))))
      (numV 6))
(test (interp*
       '(let ([x 5]) (begin (set! x 6) x)))
      (numV 6))
(test (interp*
       '(let ([x (box 0)])
          (let ([f
                 (λ (ignored)
                   (begin (set-box! x (+ 1 (unbox x)))
                          (unbox x)))])
            (+ (f 0) (f 0)))))
      (numV 3))
(test (interp*
       '(let ([x (box 0)])
          (let ([f
                 (λ (ignored)
                   (let ([x (box 0)])
                     (begin (set-box! x (+ 1 (unbox x)))
                            (unbox x))))])
            (+ (f 0) (f 0)))))
      (numV 2))
(test (interp*
       '(let ([x 0])
          (let ([f
                 (λ (ignored)
                   (let ([x 0])
                     (begin (set! x (+ 1 x))
                            x)))])
            (+ (f 0) (f 0)))))
      (numV 2))
(test (interp*
       '(let ([x 0])
          (let ([f
                 (λ (ignored)
                   (begin (set! x (+ 1 x))
                          x))])
            (+ (f 0) (f 0)))))
      (numV 3))

(test (interp* '(if0 0 1 2)) (numV 1))
(test (interp* '(if0 1 1 2)) (numV 2))
(test (interp* '(let ([x 1])
                  (if0 (set! x 0) x 2)))
      (numV 0))

(test/exn (interp* '(let ([x x])
                      x))
          "undefined")
(test (interp* '(let ([x (box 0)])
                  (begin (set-box! x x)
                         (unbox (unbox (unbox x))))))
      (boxV 1))

(test/exn (interp* '(let ([fac
                           (λ (n)
                             (if0 n
                                  1
                                  (* n (fac (- n 1)))))])
                      (fac 4)))
          "undefined")
(test (interp* '(let ([fac-box (box 0)])
                  (let ([fac
                         (λ (n)
                           (if0 n
                                1
                                (* n ((unbox fac-box)
                                      (- n 1)))))])
                    (begin (set-box! fac-box fac)
                           (fac 4)))))
      (numV 24))
(test (interp* '(let ([fac-box (box 0)])
                  (let ([fac
                         (λ (n)
                           (if0 n
                                1
                                (* n ((unbox fac-box)
                                      (- n 1)))))])
                    (begin (set-box! fac-box fac)
                           ((unbox fac-box) 4)))))
      (numV 24))
(test (interp* '(let ([fac 0])
                  (begin (set! fac
                               (λ (n)
                                 (if0 n
                                      1
                                      (* n (fac (- n 1))))))
                         (fac 4))))
      (numV 24))
;; (test (interp* '(letrec ([x (+ 1 x)])
;;                   x))
;;       (numV 1))

;; (test (interp* '((λ (x) (x x))
;;                  (λ (x) (x x))))
;;       (numV 42))


;; Omega
;; ((λ (x) (x x))
;;  (λ (x) (x x)))

;; (x x) [x <- (λ (x) (x x))]

;; ((λ (x) (x x))
;;  (λ (x) (x x)))

(test (interp* '(let ([make-fac
                       (λ (the-real-make-fac)
                         (λ (n)
                           (if0 n
                                1
                                (* n ((the-real-make-fac
                                       the-real-make-fac)
                                      (- n 1))))))])
                  ((make-fac make-fac) 4)))
      (numV 24))
(test (interp* '(let ([make-fac
                       (λ (the-real-make-fac)
                         (let ([fac
                                (λ (n)
                                  ((the-real-make-fac
                                    the-real-make-fac) n))])
                           (λ (n)
                             (if0 n
                                  1
                                  (* n (fac (- n 1)))))))])
                  (let ([fac (make-fac make-fac)])
                    (fac 4))))
      (numV 24))
(test (interp* '(let ([make-rec
                       (λ (make-thing)
                         ((λ (x)
                            (make-thing (λ (v) ((x x) v))))
                          (λ (x)
                            (make-thing (λ (v) ((x x) v))))))])
                  (let ([fac
                         (make-rec
                          (λ (real-fac)
                            (λ (n)
                              (if0 n
                                   1
                                   (* n (real-fac (- n 1)))))))])
                    (fac 4))))
      (numV 24))
(test (interp* '(let ([Y ;; Invented Haskell B. Curry in 40s and 50s
                       (λ (make-thing)
                         ((λ (x)
                            (make-thing (λ (v) ((x x) v))))
                          (λ (x)
                            (make-thing (λ (v) ((x x) v))))))])
                  (let ([fac
                         (Y
                          (λ (real-fac)
                            (λ (n)
                              (if0 n
                                   1
                                   (* n (real-fac (- n 1)))))))])
                    (fac 4))))
      (numV 24))

;; f (Y f) = f

;; Recursion

;; - large problem and break into smaller problems until it is easy
;; and compositional --- quicksort is generative recursion (vs
;; structural recursion or induction)

;; - requires a "base" where it stops

;; - mathematical induction... need base case and have this sorta
;; chain winding thing

;; Induction on Natural Numbers - Peano numbers

;; Num = 0 | Num + 1

;; If you want to prove "Forall N where N is a number, N has property
;; P", then do this:

;; Prove "0 has property P" [base case]

;; Prove "If N has property P, then N + 1 has property P" [inductive step]

;; Done!

;; We want the proof of P for the number 5 =
;; inductive(inductive(inductive(inductive(inductive(base)))))

;; Induction on Trees
;; Prove P on leaf
;; If P on left and P on right, then P on branch

;; Co-Recursion ... is recursion towards infinity rather than towards 0
