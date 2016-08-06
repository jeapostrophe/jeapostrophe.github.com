#lang plai-typed
(print-only-errors #t)

;; Mutation is...
;; - changing something
;; - from one data type to its equivlanet in another data type [int to long?]
;; - changing the "global" environment
;; .... moral comments for ...
;; - all mutants should be registered
;; - i like being able to change values of variables, it's useful,
;; - i don't "have to" create new variables with new values
;; - "automatic" is convenient and possible to hurt yourself
;;   for instance, "4" + 3 = 7 or "43" would be bad
;; - can be confusing to track who and where things changes. the lang doesn't help you
;; - lose original information (int to float loses stuff?)

;; Example of mutation is...

;; int x = 5
;; x = 6
;; return x + 1;

;; Three different programs... in Java...

;; Variable mutation
;; 1.   f = 3 { int f = 7; .... [*]  }
;; Data or structure mutation
;; 2. o.f = 3 { Obj o = ...; .... [*]}
;; 3.   f = 3 { inside of a class with field f}

;; class Box {
;;            Object contained;

;;                   ChangeIt ( Object new ) {
;;                                            contained = new;
;;                                            }

;;                   GetIt () {
;;                             return contained;
;;                             }

;;            }

;; box : a -> box(a)
;; unbox : box(a) -> a
;; setbox : box(a) a -> void

;; An expression evaluates to a value
;; A statement is evaluated for effect

;; Sequencing
;; x = 7 ; x = 8 ; return x

;; Question: Without mutation, does it matter what order statements
;; are evaluated in?

;; Without mutation we don't have "statements"

;; (sequence a b) -> b (unsound)
;; (let ([x a]) b) where x does not appear in b
;; (+ a b) -> order of evaluation doesn't matter

;; (first (cons a b)) -> (begin0 a b)

(test (unbox (box 5))
      5)
(test (let ([x (box 5)]) (unbox x))
      5)
(test (let ([xs (list (box 5) (box 6))]) (unbox (second xs)))
      6)
(test (let ([x (box 5)]) (begin (set-box! x 6) (unbox x)))
      6)
(test (let ([x (box 0)])
        (let ([f
               (λ ()
                 (begin (set-box! x (add1 (unbox x)))
                        (unbox x)))])
          (list (f) (f) (f))))
      (list 1 2 3))
(test (let ([x (box 0)])
        (let ([f
               (λ ()
                 (begin (set-box! x (add1 (unbox x)))
                        (unbox x)))])
          (let ([cached-f (f)])
            (list cached-f cached-f cached-f))))
      (list 1 1 1))
(test (let ([f
             (λ ()
               (let ([x (box 0)])
                 (begin (set-box! x (add1 (unbox x)))
                        (unbox x))))])
        (let ([cached-f (f)])
          (list cached-f cached-f cached-f)))
      (list 1 1 1))
(test (let ([f
             (λ ()
               (let ([x (box 0)])
                 (begin (set-box! x (add1 (unbox x)))
                        (unbox x))))])
        (list (f) (f) (f)))
      (list 1 1 1))

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
;; | (begin ExprC ExprC)
;; | (let ([id ExprC]) ExprC)

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

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Value
  [numV (n : number)]
  [boxV (v : Value)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

(define (lookup [id : symbol] [env : Env]) : Value
  (cond
    [(empty? env)
     (error 'lookup "undefined identifier")]
    [(symbol=? id (bind-name (first env)))
     (bind-val (first env))]
    [else
     (lookup id (rest env))]))

(test/exn (lookup 'x empty) "undefined")
(test (lookup 'x (list (bind 'x (numV 5)))) (numV 5))

;; TODO Add "effects" to output of interp AND to input of interp

(define (interp [p : ExprC] [env : Env]) : Value
  (type-case
   ExprC p
   [numC
    (val)
    (numV val)]
   [binC
    (op lhs rhs)
    (numV (op (numV-n (interp lhs env))
              (numV-n (interp rhs env))))]
   [idC
    (id)
    (lookup id env)]
   [appC
    (fun arg)
    (local
     [(define f (interp fun env))]
     (interp (closV-body f)
             (extend-env
              (bind (closV-arg f) (interp arg env))
              (closV-env f))))]
   [lamC
    (arg body)
    (closV arg body env)]
   [boxC
    (inite)
    (boxV (interp inite env))]
   [unboxC
    (boxe)
    (type-case
     Value (interp boxe env)
     [boxV
      (v)
      v]
     [else
      (error 'interp "No box for you!")])]
   [set-boxC
    (boxe vale)
    (type-case
     Value (interp boxe env)
     [boxV
      (old-v)
      ;; change the value of box that boxe evaluates to to be the
      ;; value that vale evaluates to
      (interp vale env)]
     [else
      (error 'interp "No box for you!")])]
   [beginC
    (fste snde)
    (begin (interp fste env)
           (interp snde env))]))

;; Tests

;; interp* : s-exp -> number
(define (interp* [s : s-expression]) : Value
  (interp (parse s) empty))

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
