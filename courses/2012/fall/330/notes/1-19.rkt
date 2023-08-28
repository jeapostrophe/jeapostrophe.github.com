#lang plai
(print-only-errors #t)

;; LAST TIME
;; - store passing style: threads the store linearly (used exactly once)
;; - mutation is inherently anti-parallel: adds dependency and temporality
;; - mutable data structures

;; THIS TIME                              [0:05]
;; - variables
;; -- set!                                [0:10]
;; -- where are variables in the store?   [0:15]
;; -- set! impl
;; --- lvalue                             [0:20]
;; -- interaction with function calls     [0:25]
;; -- ref-fun                             [0:35]
;; -- is ref-fun good? bad?
;; --- swap                               [0:40]
;; - scope vs extent
;; -- C / C++ rules
;; - garbage collection                   [0:50]

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
  [refun (param symbol?)
         (body AE?)]
  [if0 (cond-e AE?)
       (true-e AE?)
       (false-e AE?)]
  [newbox (init-e AE?)]
  [setbox (box-e AE?)
          (val-e AE?)]
  [openbox (box-e AE?)]
  [seqn (fst-e AE?)
        (snd-e AE?)]
  [setvar (var-s symbol?)
          (val-e AE?)])

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
;;       | (refun (<id>) <AE>)
;;       | (if0 <AE> <AE> <AE>)
;;       | (newbox <AE>)
;;       | (openbox <AE>)
;;       | (setbox <AE> <AE>)
;;       | (setvar <id> <AE>)
;;       | (seqn <AE> <AE>)

;; where <id> is any Racket symbol, except +, -, *, /, fun, if0, rec, newbox, openbox, setbox, seqn, and with, setvar

(define sym->binop
  (hasheq '+ +
          '- -
          '* *
          '/ /))

;; parse :: s-expression -> AE
(define (parse se)
  (cond
    [(and (list? se)
          (= 2 (length se))
          (eq? 'newbox (first se)))
     (newbox (parse (second se)))]
    [(and (list? se)
          (= 2 (length se))
          (eq? 'openbox (first se)))
     (openbox (parse (second se)))]
    [(and (list? se)
          (= 3 (length se))
          (eq? 'setbox (first se)))
     (setbox (parse (second se))
             (parse (third se)))]
    [(and (list? se)
          (= 3 (length se))
          (eq? 'setvar (first se))
          (symbol? (second se)))
     (setvar (second se)
             (parse (third se)))]
    [(and (list? se)
          (= 3 (length se))
          (eq? 'seqn (first se)))
     (seqn (parse (second se))
           (parse (third se)))]
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
          (equal? 'refun (first se))
          (list? (second se))
          (= 1 (length (second se)))
          (symbol? (first (second se))))
     (refun (first (second se))
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

(define-type Env
  [mtEnv]
  [consEnv
   (name symbol?)
   ;; was
   ;;(named-value AEV?)
   ;; now
   (ptr number?)
   (rest Env?)])

(define-type Store
  [mtSto]
  [consSto
   (ptr number?)
   (named-value AEV?)
   (rest Store?)])

(define-type AEV
  [numV
   (n number?)]
  [closureV
   (param symbol?)
   (body AE?)
   (env Env?)]
  [reclosureV
   (param symbol?)
   (body AE?)
   (env Env?)]
  [boxV
   (ptr number?)])

(define (lookup-id $ sym)
  (type-case
   Env $
   [mtEnv
    ()
    (error 'calc "You has a bad identifier, bro: ~e" sym)]
   [consEnv
    (name named-value rest)
    (if (equal? name sym)
      named-value
      (lookup-id rest sym))]))

(define (lookup-ptr $ ptr)
  (type-case
   Store $
   [mtSto
    ()
    (error 'calc "SEGFAULT ~a" ptr)]
   [consSto
    (name named-value rest)
    (if (equal? name ptr)
      named-value
      (lookup-ptr rest ptr))]))

(define (malloc some-sto)
  (add1 (store-max some-sto)))
(define (store-max some-sto)
  (type-case
   Store some-sto
   [mtSto
    ()
    -1]
   [consSto
    (some-ptr some-val smore)
    (max some-ptr (store-max smore))]))

;; calc : AE? Env? Store? -> AEV? & Store?
;; compute the meaning of the AE
(define (calc ae $ sto)
  (type-case
   AE ae
   [setvar
    (var-s val-e)

    ;; var-e = 'x (X was 42)
    ;; (calc var-e $ sto)
    ;; returns (numV 42)

    (local
     [(define ptr (lookup-id $ var-s))
      (define-values (val-v val-sto) (calc val-e $ sto))]
     (values val-v
             (consSto ptr val-v val-sto)))]
   [newbox
    (init-e)
    (local
     [(define-values (init-v init-sto) (calc init-e $ sto))
      (define ptr (malloc init-sto))]
     (values (boxV ptr)
             (consSto ptr init-v init-sto)))]
   [openbox
    (box-e)
    (local
     [(define-values (box-v box-sto) (calc box-e $ sto))]
     (type-case
      AEV box-v
      [boxV
       (ptr)
       (values (lookup-ptr box-sto ptr)
               box-sto)]
      [else
       (error 'calc "Not a box")]))]
   [setbox
    (box-e val-e)
    (local
     [(define-values (box-v box-sto) (calc box-e $ sto))]
     (type-case
      AEV box-v
      [boxV
       (ptr)
       (local
        [(define-values (val-v val-sto) (calc val-e $ box-sto))]
        (values val-v
                (consSto ptr val-v val-sto)))]
      [else
       (error 'calc "Not a box")]))]
   [seqn
    (fst-e snd-e)
    (local
     [(define-values (fst-v fst-sto) (calc fst-e $ sto))]
     (calc snd-e $ fst-sto))]
   [fun
    (param body)
    (values (closureV param body $)
            sto)]
   [refun
    (param body)
    (values (reclosureV param body $)
            sto)]
   [app
    (fun arg)
    (local
     [(define-values (fun-v fun-sto) (calc fun $ sto))]
     (type-case
      AEV fun-v
      [closureV
       (arg-name fun-body fun-$)
       (local
        [(define-values (arg-v arg-sto) (calc arg $ fun-sto))
         (define arg-ptr (malloc arg-sto))]
        (calc fun-body
              (consEnv
               arg-name
               arg-ptr
               fun-$)
              (consSto
               arg-ptr
               arg-v
               arg-sto)))]
      [reclosureV
       (arg-name fun-body fun-$)
       (type-case
        AE arg
        [id
         (sym)
         (local
          [(define arg-ptr (lookup-id $ sym))]
          (calc fun-body
                (consEnv
                 arg-name
                 arg-ptr
                 fun-$)
                fun-sto))]
        [else
         (error 'calc "Not an lvalue")])]
      [else
       (error 'calc "Not a function, man")]))]
   [id
    (sym)
    (values (lookup-ptr sto (lookup-id $ sym))
            sto)]
   [num
    (n)
    (values (numV n)
            sto)]
   [if0
    (cond-e true-e false-e)
    (local
     [(define-values (cond-v cond-sto) (calc cond-e $ sto))]
     (if (lift-numV zero? cond-v)
       (calc true-e $ cond-sto)
       (calc false-e $ cond-sto)))]
   [binop
    (op lhs rhs)
    (local
     [(define-values (lhs-v lhs-sto) (calc lhs $ sto))
      (define-values (rhs-v rhs-sto) (calc rhs $ lhs-sto))]
     (values
      (numV
       (lift-numV
        op
        lhs-v
        rhs-v))
      rhs-sto))]))

(define (lift-numV f . args)
  (apply f (map numV-n* args)))

(define (numV-n* a)
  (if (numV? a)
    (numV-n a)
    (error 'calc "Not a number: ~e" a)))

;; calc* : sexpr -> number?
(define (calc* se)
  (define-values (res res-sto) (calc (parse se) (mtEnv) (mtSto)))
  (printf "~e\n" res-sto)
  (type-case
   AEV res
   [numV (n) n]
   [else res]))

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


(test (calc* '(fun (x) (+ x x)))
      (closureV 'x (parse '(+ x x)) (mtEnv)))

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

(test (calc* '(with [id (fun (x) x)]
                    (with [o (fun (f) (f f))]
                          (o id))))
      (closureV 'x (id 'x) (mtEnv)))

(test (calc* '(with [id (fun (x) x)]
                    (id id)))
      (closureV 'x (id 'x) (mtEnv)))

(test (calc* '(with [id (fun (x) x)]
                    id))
      (closureV 'x (id 'x) (mtEnv)))

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

;; 1 + 2;
;; return 2;
(test (calc* '(seqn (+ 1 2) 2))
      2)

;; Illegal C:
;; return 1 + (1 + 2; 2)
(test (calc* '(+ 1 (seqn (+ 1 2) 2)))
      3)

(test (calc* '(newbox 5))
      (boxV 0))

(test (calc* '(openbox (newbox 5)))
      5)
(test/exn (calc* '(openbox 5))
          "Not a box")

;; int x = 5; int z = (x = 7); printf("%d\n", z);;
(test (calc* '(setbox (newbox 5) 7))
      ;; oldvalue
      ;; 5
      ;; ptr
      ;; (boxV 0)
      ;; new value
      7)

(test/exn (calc* '(setbox 5 7))
          "Not a box")

;; int x = 5;
;; int y = x;
;; x = 7
;; return x + y;
(test (calc* '(with [x (newbox 5)]
                    (with [y (newbox (openbox x))]
                          (seqn (setbox x 7)
                                (+ (openbox x)
                                   (openbox y))))))
      12)
(test/exn
 (calc* '(with [x (newbox 5)]
               (with [y (newbox x)]
                     (seqn (setbox x 7)
                           (+ (openbox x)
                              (openbox y))))))
 "Not a number")
(test (calc* '(with [x (newbox 5)] ;; int*
                    (with [y (newbox x)] ;; int**
                          (seqn (setbox x 7)
                                (+ (openbox x)
                                   (openbox (openbox y)))))))
      14)
(test (calc* '(with [x (newbox 5)]
                    (with [y (newbox (seqn (setbox x 7) 42))]
                          (openbox x))))
      7)
(test (calc* '(with [x (newbox 5)]
                    ((seqn (setbox x 7)
                           (fun (y) y))
                     (openbox x))))
      7)
(test (calc* '(with [x (newbox 5)]
                    (with [f (fun (y) (openbox x))]
                          (f (setbox x 7)))))
      7)
(test (calc* '((with [x 7]
                     (fun (y) x))
               5))
      7)


(test (calc* '(with [x 5]
                    (with [z 6]
                          (with [f (fun (y) z)]
                                (f (setvar z 7))))))
      7)

(test (calc* '(with [f (fun (x) (+ x 1))]
                    (f 5)))
      6)
(test (calc* '(with [f (fun (x) (+ x 1))]
                    (seqn
                     (setvar f (fun (x) (+ x 2)))
                     (f 5))))
      7)
;; Mutation can change the type of variables (ugly!)
(test/exn (calc* '(with [f (fun (x) (+ x 1))]
                        (seqn
                         (setvar f 42)
                         (f 5))))
          "Not a function")


;; Valid C is exciting!
;; y = x + 1
;; x + 1 = y

;; L1337: Not an l-value
(test/exn (calc* '(setvar (+ 1 1) 3))
          "Invalid syntax")

(test (calc* '(with [f (fun (y) y)]
                    (f 7)))
      7)
(test (calc* '(with [y 7]
                    (with [f (fun (y) y)]
                          (f y))))
      7)
(test (calc* '(with [y 7]
                    (with [f (fun (y) y)]
                          (+ y (f y)))))
      14)
(test (calc* '(with [y 7]
                    (with [f (fun (y) y)]
                          (+ y (+ (f y) y)))))
      21)
(test (calc* '(with [y 7]
                    (with [f (fun (y) (seqn (setvar y 5) y))]
                          (+ y (+ (f y) y)))))
      ;; pass by "reference"      
      ;; 17
      ;; pass by "copy"
      19)
(test (calc* '(with [y 7]
                    (with [f (fun (x) (seqn (setvar x 5) x))]
                          (+ y (+ (f y) y)))))
      ;; pass by "reference"      
      ;; 17
      ;; pass by "copy"
      19)
(test (calc* '(with [y 7]
                    (with [f (fun (x) (seqn (setvar y 5) x))]
                          (+ y (+ (f y) y)))))
      ;; pass by "reference"      
      ;; 17
      ;; pass by "copy"
      19)

;; Bjarne Stroustroup
;; int f (int &x) {
;;  x = 5;
;;  return x;
;; }
;; f (y);
(test (calc* '(with [y 7]
                    (with [f (refun (x) (seqn (setvar x 5) x))]
                          (+ y (+ (f y) y)))))     
      ;; pass by "copy"
      ;; 19
      ;; pass by "reference"      
      17)

(test (calc* '(with [x 7]
                    (with [y 5]
                          (with [swap (refun (x) 
                                             (refun (y)
                                                    (with [tmp x]
                                                          ;; XXX change to super awesome XOR
                                                          (seqn (setvar x y)
                                                                (setvar y tmp)))))]
                                (seqn ((swap x) y)
                                      (/ x y))))))     
      5/7)

;; int f (int x) {
;;  int z = 0;
;;  (int *)(((int)&z) + 4*i) = 
;; }

;; (int *)(read() * 1024) = 5


;; C lies... that SCOPE (where the name x is valid) is the same as EXTENT (where the value x is pointing to is valid)
;; int f () {
;;  int x = 5;
;;  g(&x)
;;  return &x;
;; }

;; int h () {
;;  int y = 7;
;;  return g2();
;; }
