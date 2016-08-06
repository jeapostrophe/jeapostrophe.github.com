#lang plai-typed
(print-only-errors #t)

;; Parser

;; ExprC =
;; | num
;; | (+ ExprC ExprC)
;; | (* ExprC ExprC)
;; | id
;; | (ExprC ExprC)
;; | (λ (id) ExprC)
;; | (box ExprC)
;; | (unbox ExprC)
;; | (set-box! ExprC ExprC)
;; | (set! id ExprC)
;; | (begin ExprC ExprC)
;; | (let ([id ExprC]) ExprC)

;; (set! (+ 2 2) 5) ;; (+ 2 2) isn't a variable
;; (let ([x (+ 2 2)]) (set! x 5)) ;; but x is!

;; "2 + 2 = 5;" ;; lhs is not an l-value
;; "*(x+5) = 5" ;; l-value = assignable things
;; "x[5] = 5" ;; l-value = assignable things

;; (set! (first (rest l)) 5)

(define-type ExprC
  [numC (val : number)]
  [binC (op : (number number -> number))
        (lhs : ExprC) (rhs : ExprC)]
  [idC (id : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [boxC (inite : ExprC)]
  [unboxC (boxe : ExprC)]
  [set-boxC (boxe : ExprC) (vale : ExprC)]
  [setC (var : symbol) (vale : ExprC)]
  [beginC (fste : ExprC) (snde : ExprC)])

(define (parse [se : s-expression]) : ExprC
  (cond
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
    [(s-exp-number? se)
     (numC (s-exp->number se))]
    [(and (s-exp-list? se) (= (length (s-exp->list se)) 3)
          (equal? '+ (s-exp->symbol (first (s-exp->list se)))))
     (binC
      +
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
     (list c)]
    [(= (cell-addr c) (cell-addr (first mem)))
     (cons c (rest mem))]
    [else
     (cons (first mem)
           (extend-mem/replace c (rest mem)))]))

(define-type Value
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
   [numC
    (val)
    (values (numV val) mem)]
   [binC
    (op lhs rhs)
    (local
     
     ;; We had this before:
     ;; (op (numV-n (interp lhs env))
     ;;     (numV-n (interp rhs env)))

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
   [beginC
    (fste snde)
    (local
     [(define-values (fst-v fst-mem) (interp fste env mem))]
     (interp snde env fst-mem))]))

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
       ;; Only with set! are these variables
       ;;      |
       ;;      v
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

(interp
 (parse
  '(+ (let ([x (box 0)])
        (let ([f
               (λ (ignored)
                 (let ([x (box 0)])
                   (begin (set-box! x (+ 1 (unbox x)))
                          (unbox x))))])
          (+ (f 0) (f 0))))
      5))
 mt-env
 mt-mem)

;; Optimizations
;; - store only one [better ds]
;; - remove stuff you can't reach [garbage collection]

;; software transactional memory (STM)

;; Fork into A and B
;; A runs and changed P0 to 5
;; B runs and changed P1 to 7
;; Sync A & B
;; Memory has P0 at 5 and P1 at 7

;; Fork into A and B
;; A runs and changed P0 to 5
;; B runs and changed P0 to 7
;; Sync A & B - fail B and take A
;; Memory has P0 at 5
;; B runs and changed P0 to 8
;; Sync A & B 
;; Memory has P0 at 8

;; Does this version of mutation have anything to do with reality? --
;; Not really, but maybe like how the JVM does it

;; Haskell unlike Racket and Java does not have mutation, But you can
;; build it yourself like this (look up) "store passing style"

;; Some things better left unmutated -- invariants help us reason
;; Mutation makes us think about "when"

;; Mutation gives us... new data-structures such as cycles
;; Mutation takes away "functions" and gives us "procedures"

;; Mutation is communication
