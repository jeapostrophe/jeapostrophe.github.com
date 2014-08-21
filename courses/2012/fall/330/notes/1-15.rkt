#lang plai
(print-only-errors #t)

(define-type AE
  [num (n number?)]
  [binop (op procedure?)
         (lhs AE?)
         (rhs AE?)]
  [id (sym symbol?)]
  [app (fun AE?)
       (arg AE?)]
  [fun (param symbol?)
       (body AE?)]
  [if0 (cond-e AE?)
       (true-e AE?)
       (false-e AE?)])

(define (with name named-thing body)
  (app (fun name body) named-thing))
(define (rec name named-thing body)
  (define the-weird-fac
    (gensym 'the-weird-fac))
  (define the-inner-fac
    (gensym 'the-inner-fac))
  (with the-weird-fac
        (fun the-inner-fac
             (with name 
                   (fun 'ν
                        (app (app (id the-inner-fac) (id the-inner-fac))
                             (id 'ν)))
                   named-thing))
        (with name (app (id the-weird-fac) (id the-weird-fac))
              body)))

;; <AE> := <real Racket number>
;;       | (+ <AE> <AE>)
;;       | (- <AE> <AE>)
;;       | (* <AE> <AE>)
;;       | (/ <AE> <AE>)
;;       | <id>
;;       | (with [<id> <AE>] <AE>)
;;       | (rec [<id> <AE>] <AE>)
;;       | (<AE> <AE>)
;;       | (fun (<id>) <AE>)
;;       | (if0 <AE> <AE> <AE>)

;; where <id> is any Racket symbol, except +, -, *, /, fun, if0, rec, and with

(define sym->binop
  (hasheq '+ +
          '- -
          '* *
          '/ /))

;; parse :: s-expression -> AE
(define (parse se)
  (cond
    [(and (list? se)
          (= 2 (length se)))
     (app (parse (first se))
          (parse (second se)))]
    [(and (list? se)
          (= 3 (length se))
          (equal? 'fun (first se))
          (list? (second se))
          (= 1 (length (second se)))
          (symbol? (first (second se))))
     (fun (first (second se))
          (parse (third se)))]
    [(and (list? se)
          (= 3 (length se))
          (equal? 'with (first se))
          (list? (second se))
          (= 2 (length (second se)))
          (symbol? (first (second se))))
     (with (first (second se))
           (parse (second (second se)))
           (parse (third se)))]
    [(and (list? se)
          (= 3 (length se))
          (equal? 'rec (first se))
          (list? (second se))
          (= 2 (length (second se)))
          (symbol? (first (second se))))
     (rec (first (second se))
          (parse (second (second se)))
          (parse (third se)))]
    [(symbol? se)
     (id se)]
    [(number? se)
     (num se)]
    [(and (list? se)
          (= 3 (length se))
          (hash-has-key? sym->binop (first se)))
     (binop (hash-ref sym->binop (first se))
            (parse (second se))
            (parse (third se)))]
    [(and (list? se)
          (= 4 (length se))
          (equal? 'if0 (first se)))
     (if0 (parse (second se))
          (parse (third se))
          (parse (fourth se)))]
    [else
     (error 'parse "Invalid syntax, dude: ~e" se)]))

(test (parse '1)
      (num 1))
(test (parse '(+ 1 1))
      (binop + (num 1) (num 1)))
(test (parse '(- 1 1))
      (binop - (num 1) (num 1)))
(test (parse 'x)
      (id 'x))
(test (parse '(with [x 27] x))
      (with 'x (num 27) (id 'x)))
;; We can't write this test because (gensym) is unpredictable
;;(test (parse '(rec [x 27] x))
;;      (rec 'x (num 27) (id 'x)))
(test (parse '(double 5))
      (app (id 'double) (num 5)))
(test (parse '(fun (x) (+ x x)))
      (fun 'x (binop + (id 'x) (id 'x))))
(test (parse '(if0 0 1 2))
      (if0 (num 0) (num 1) (num 2)))

(test/exn (parse "1")
          "Invalid syntax")

;; (define-type Env
;;   [mtEnv]
;;   [consEnv
;;    (name symbol?)
;;    (named-value AEV?)
;;    (rest Env?)])

;; ;; lookup-id : Env symbol -> value
;; (define (lookup-id $ sym)
;;   (type-case
;;    Env $
;;    [mtEnv 
;;     ()
;;     (error 'calc "You has a bad identifier, bro: ~e" sym)]
;;    [consEnv
;;     (name named-value rest)
;;     (if (symbol=? name sym)
;;       named-value
;;       (lookup-id rest sym))]))

(define (lookup-id $ sym)
  ($ sym))

;; then: Env = (mtEnv + consEnv)
;;  now: Env = symbol -> value

(define (mtEnv)
  (λ (sym)
    (error 'calc "You has a bad identifier, bro: ~e" sym)))
(define (consEnv name named-value rest)
  (λ (sym)
    (if (symbol=? name sym)
      named-value
      (lookup-id rest sym))))

(define Env? procedure?)

;; Church encoding.... Alonzo Church... the creator of λ-calculus and
;; Turing's advisor... the other in the Church-Turing Hypothesis

;; a.k.a

;; Object-oriented programming

;; (define (id1 x) x)
;; (define (id2 x) 
;;   (if (TM-halts? ...)
;;     (sub1 (add1 x))
;;     77))

;; (equal? add1 (λ (x) (+ 1 x)))

;; (define-type AEV
;;   [numV
;;    (n number?)]
;;   [closureV
;;    (param symbol?)
;;    (body AE?)
;;    (env Env?)])

(define AEV?
  (or/c number? procedure?))

(define (numV n) n)
(define numV? number?)
(define (numV-n n) n)

(define (closureV arg-name fun-body fun-$)
  (λ (arg-val)
    (calc fun-body 
          (consEnv
           arg-name
           arg-val
           fun-$))))

;; one method to many via dispatch
;; (λ (msg . more-args)
;;   (cond
;;     [(eq? msg 'inc)
;;      (parse ... more-args ...)]
;;     [(eq? msg 'dec)
;;      (parse ... more-args ...)]
;;     [else
;;      (error 'msg-not-understood)]))

;; many methods via v-table
;; (struct some-interface (method1 method2))
;; (define (constructor field ...)
;;   (some-interface
;;    (λ (one-method)
;;      one-method-body)
;;    (λ (two-method)
;;      two-method-body)))

;; ((some-interface-method1 obj1) ...)

;; v-table as closure
;; (λ (msg)
;;   (cond
;;     [(eq? msg 'inc)
;;      (λ ()
;;        .... something ...)]))

;; (define (some-interface-method1 obj)
;;   (obj 'method1))

;; calc : AE? Env? -> AEV?
;; compute the meaning of the AE
(define (calc ae $)
  (type-case
   AE ae
   [fun
    (param body)
    (closureV param body $)]
   [app
    (fun arg)
    (local
     [(define fun-val (calc fun $))]
     (if (numV? fun-val)
       (error 'calc "Not a function, man")
       (fun-val
        (calc arg $))))

    ;; (type-case
    ;;  AEV (calc fun $)
    ;;  [closureV
    ;;   (arg-name fun-body fun-$)
    ;;   (calc fun-body 
    ;;         (consEnv
    ;;          arg-name
    ;;          (calc arg $)
    ;;          fun-$))]
    ;;  [else
    ;;   (error 'calc "Not a function, man")])
    ]
   [id
    (sym)
    (lookup-id $ sym)]   
   [num
    (n)
    (numV n)]
   [if0
    (cond-e true-e false-e)
    (if (lift-numV zero? (calc cond-e $))
      (calc true-e $)
      (calc false-e $))]
   [binop
    (op lhs rhs)
    (numV
     (lift-numV
      op
      (calc lhs $)
      (calc rhs $)))]))

;; object lang is AE

;; meta lang is Racket

;; calc is now a META interpreter, before it was a SYNTACTIC interpreter

;; META-CIRCULAR

(define (lift-numV f . args)
  (apply f (map numV-n* args)))

(define (numV-n* a)
  (if (numV? a)
    (numV-n a)
    (error 'calc "Not a number: ~e" a)))

;; calc* : sexpr -> number?
(define (calc* se)
  (define res (calc (parse se) (mtEnv)))
  ;; (type-case 
  ;;  AEV res
  ;;  [numV (n) n]
  ;;  [else res])
  res)

(test (calc* '1)
      1)
(test (calc* '(+ 1 1))
      2)
(test (calc* '(- 0 1))
      -1)
(test (calc* '(* 2 1))
      2)
(test (calc* '(/ 4 2))
      2)

(test (calc* '(with [x (+ 5 5)]
                    (+ x x)))
      20)
(test (calc* '(with [y 7]
                    (with [x y]
                          (+ x x))))
      14)
(test (calc* '(with 
               [x (+ 5 5)]
               (with [x 7]
                     (+ x x))))
      14)
(test (calc* '(with [x (+ 5 5)]
                    (+ x (with [x 7]
                               (+ x x)))))
      24)
(test (calc* '(with [x (+ 5 5)]
                    (+ (with [x 7]
                               (+ x x))
                       x)))
      24)
(test (calc* '(with
               [x (+ 5 5)]
               (+ (with [x 7]
                        (+ x x))
                  (with [x 8]
                        (+ x x)))))
      (+ 14 16))
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

(test (calc* '(with [x 5]
                     (with [y 5]
                           (+ (+ x x) y))))
      15)

(test (calc* '(with [x 5]
                     (with [x (+ 1 x)]
                           (+ (+ x x) x))))
      18)

(test/exn (calc* '(with [x 5]
                     (with [x (+ 1 x)]
                           (+ (+ x x) y))))
          "bro")

(test/exn (calc* '(with [x x]
                        5))
          "bro")


;;(test (calc* '(fun (x) (+ x x)))
;;      (closureV 'x (parse '(+ x x)) (mtEnv)))

(test (calc* '(+ 1 (+ 2 3)))
      6)

(test/exn (calc* '(+ 1 (fun (x) x)))
          "Not a number")

(test/exn (calc* '(5 1))
          "Not a function")

(test (calc* '(with [double (fun (x) (+ x x))]
                    (double 5)))
      10)

(test (calc* '(with [double (fun (x) (+ x x))]
                    (with [triple (fun (x) (+ x (double x)))]
                          (triple 5))))
      15)


(test (calc* '(with [double (fun (x) (+ x x))]
                    (with [triple (fun (x) (+ x (double x)))]
                          (with [double (fun (x) (- x 6))]
                                (triple 5)))))
      15)

(test (calc* '(if0 0 1 2))
      1)
(test (calc* '(if0 -1 1 2))
      2)

;; Currying .... after Haskell B. Curry
(test (calc* '(((fun (x)
                     (fun (y)
                          (+ (* x 2) (* 4 y))))
                5)
               7))
      38)

(test (calc* '(rec [fac
                    (fun (n)
                         (if0 n
                              1
                              (* n (fac (- n 1)))))]
                   (fac 5)))
      120)

(test (calc* '(rec [fac
                    (fun (n)
                         (if0 n
                              1
                              (* n (fac (- n 1)))))]
                   (fac 0)))
      1)

(test (calc* '(rec [fac
                    (fun (n)
                         (if0 n
                              1
                              (* n (fac (- n 1)))))]
                   (fac 1)))
      1)

(test (calc* '(rec [fac
                    (fun (n)
                         (if0 n
                              1
                              (* n (fac (- n 1)))))]
                   (fac 2)))
      2)

;; (test (calc* '(rec [x
;;                     6]
;;                   x))
;;      6)


;; (test/exn (calc* '(rec [x (+ 1 x)]
;;                       x))
;;          "bad")

;; (test (calc* '(with [id (fun (x) x)]
;;                     (with [o (fun (f) (f f))]
;;                           (o id))))
;;       (closureV 'x (id 'x) (mtEnv)))

;; (test (calc* '(with [id (fun (x) x)]
;;                     (id id)))
;;       (closureV 'x (id 'x) (mtEnv)))

;; (test (calc* '(with [id (fun (x) x)]
;;                     id))
;;       (closureV 'x (id 'x) (mtEnv)))

(test (calc* '(with [fac
                     (fun (fac)
                          (fun (n)
                               (if0 n
                                    1
                                    (* n ((fac fac) (- n 1))))))]
                    ((fac fac) 5)))
      120)

(test (calc* '(with [the-weird-fac
                     (fun (the-inner-fac)
                          (fun (n)
                               (if0 n
                                    1
                                    (* n
                                       ((the-inner-fac the-inner-fac)
                                        (- n 1))))))]
                    ((the-weird-fac the-weird-fac) 5)))
      120)

(test (calc* '(with [the-weird-fac
                     (fun (the-inner-fac)
                          (fun (n)
                               (if0 n
                                    1
                                    (* n ((the-inner-fac the-inner-fac)
                                          (- n 1))))))]
                    (with [fac (the-weird-fac the-weird-fac)]
                          (fac 5))))
      120)


(test (calc* '(with [the-weird-fac
                     (fun (the-inner-fac)
                          (with [fac (fun (ν) 
                                          ((the-inner-fac the-inner-fac) ν))]
                                (fun (n)
                                     (if0 n
                                          1
                                          (* n (fac (- n 1)))))))]
                    (with [fac (the-weird-fac the-weird-fac)]
                          (fac 5))))
      120)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define ZERO
  (λ (replace-S)
    (λ (replace-O)
      replace-O)))
(define ONE
  (λ (replace-S)
    (λ (replace-O)
      (replace-S
       replace-O))))
(define TWO
  (λ (f)
    (λ (x)
      (f (f x)))))

;; Peano notation
;;.....n = O
;;.....  | S n

(define (nat-foldr replace-O replace-S n)
  (if (zero? n)
    replace-O
    (replace-S (sub1 n))))

(define (append lst1 lst2)
  (if (empty? lst1)
    lst2
    (cons (first lst1)
          (append (rest lst1) lst2))))

(define (PLUS fst snd)
  (λ (f)
    (λ (x)
      ((fst f) ((snd f) x)))))
(define (MULT fst snd)
  (λ (f)
    (λ (x)
      ((fst (snd f)) x))))

(PLUS ZERO ONE) 
(λ (f)
    (λ (x)
      ((ZERO f) ((ONE f) x))))
(λ (f)
  (λ (x)
    ((ZERO f) (((λ (f)
                (λ (x)
                  (f x))) f) x))))
(λ (f)
  (λ (x)
    ((ZERO f) (f x))))
(λ (f)
  (λ (x)
    (((λ (f)
       (λ (x)
         x))
     f) (f x))))
(λ (f)
  (λ (x)
    ((λ (x)
       x)
     (f x))))
(λ (f)
  (λ (x)
    (f x)))
ONE


;; add1 and 0 as replace-S and replace-O convert to Racket numbers
(((PLUS ONE ONE)
  add1) 0)
(((PLUS (PLUS ONE ONE)
        (PLUS ONE ONE))
  add1) 0)

