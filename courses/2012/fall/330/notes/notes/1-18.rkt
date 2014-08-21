#lang plai
(print-only-errors #t)

;; mutation
;; - newbox, setbox, openbox, seqn
;; - where to store "state"
;; - how to plumb "state"
;; - relation to analysis
;; scope vs extent [never got here]
;; NEXT TIME: variables

;; This is a sentence that references as source [2].

;; 2. citation (possibly a url)
;; EOF

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
       (false-e AE?)]
  [newbox (init-e AE?)]
  [openbox (box-e AE?)]
  [setbox (box-e AE?)
          (val-e AE?)]
  [seqn (fst-e AE?)
        (snd-e AE?)])

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
;;       | newbox, setbox, openbox, seqn

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
          (= 2 (length se))
          (eq? 'openbox (first se)))
     (openbox (parse (second se)))]
    [(and (list? se)
          (= 2 (length se))
          (eq? 'newbox (first se)))
     (newbox (parse (second se)))]
    [(and (list? se)
          (= 3 (length se))
          (eq? 'setbox (first se)))
     (setbox (parse (second se))
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
   (named-value AEV?)
   (rest Env?)])

(define-type Store
  [mtSto]
  [consSto
   (name number?)
   (named-value AEV?)
   (rest Store?)])

(define-type AEV
  [numV
   (n number?)]
  [closureV
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
    (error 'calc "NULL ptr exception" ptr)]
   [consSto
    (name named-value rest)
    (if (= name ptr)
      named-value
      (lookup-ptr rest ptr))]))

(define (malloc sto)
  (type-case 
   Store sto
   [mtSto
    ()
    0]
   [consSto
    (some-ptr some-val some-more)
    (add1 (max some-ptr (malloc some-more)))]))

;; calc : AE? Env? Store? -> AEV? & Store?
;; compute the meaning of the AE
(define (calc ae $ sto)
  (type-case
   AE ae
   [openbox
    (box-e)
    ;; evaluate box-e to a boxV
    (local
     [(define-values (box-v box-sto) (calc box-e $ sto))]
     (type-case
      AEV box-v
      [boxV
       (ptr)
       ;; now we know the ptr... look it up
       (values (lookup-ptr box-sto ptr)
               box-sto)]
      [else
       (error 'calc "Not a box")]))]
   [newbox
    (init-e)
    (local
     (;; find out the initial value
      (define-values (init-v init-sto) (calc init-e $ sto))
      ;; getting a new address
      (define ptr (malloc init-sto)))
     ;; returning pointer
     (values (boxV ptr)
             ;; associating address with value
             (consSto ptr init-v
                      init-sto)))]
   [setbox
    (box-e val-e)
    (local
     [(define-values (box-v box-sto) (calc box-e $ sto))
      (define-values (val-v val-sto) (calc val-e $ box-sto))]
     (type-case
      AEV box-v
      [boxV
       (ptr)
       (values val-v
               (consSto
                ptr val-v
                val-sto))]
      [else
       (error 'calc "Not a box")]))]
   [seqn
    (fst-e snd-e)
    (local
     [(define-values (fst-v fst-sto) (calc fst-e $ sto))]
     (calc snd-e $ fst-sto))]
   ;; Old stuff
   [fun
    (param body)
    (values (closureV param body $)
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
        [(define-values (arg-v arg-sto) (calc arg $ fun-sto))]
        (calc fun-body
              (consEnv
               arg-name
               arg-v
               fun-$)
              arg-sto))]
      [else
       (error 'calc "Not a function, man")]))]
   [id
    (sym)
    (values (lookup-id $ sym)
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

;; int y = 6
;; x = (..... + y)

;; Mutation is inherently none parallel

;; Store passing style

(define (lift-numV f . args)
  (apply f (map numV-n* args)))

(define (numV-n* a)
  (if (numV? a)
    (numV-n a)
    (error 'calc "Not a number: ~e" a)))

;; calc* : sexpr -> number?
(define (calc* se)
  (define-values (res res-sto) (calc (parse se) (mtEnv) (mtSto)))
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

(test (calc* '(seqn 1
                    2))
      2)

;; Illegal C (and Java, etc)
;; x = (y = 1; z = 2);

(test (calc* '(+ (seqn 1
                       2)
                 1))
      3)

(test (calc* '(newbox 5))
      (boxV 0))

;; C: int x = 5; z = (x = 7); printf("%d\n", z);
(test (calc* '(setbox (newbox 5) 7))
      7)

(test/exn (calc* '(openbox 5))
          "Not a box")

(test (calc* '(openbox (newbox 5)))
      5)

(test/exn (calc* '(setbox 5 6))
          "Not a box")

(test (calc* '(with [x (newbox 5)]
                    (with [y (newbox (openbox x))]
                          (seqn (setbox x 7)
                                (+ (openbox x) (openbox y))))))
      12)
(test/exn (calc* '(with [x (newbox 5)]
                        (with [y (newbox x)]
                              (seqn (setbox x 7)
                                    (+ (openbox x) (openbox y))))))
          "Not a number")
(test (calc* '(with [x (newbox 5)]
                    (with [y (newbox x)]
                          (seqn (setbox x 7)
                                (+ (openbox x) (openbox (openbox y)))))))
      14)


(test (calc* '((with (x 5)
                     (fun (y) x))
               42))
      5)

(test (calc* '(with [x (newbox 5)]
                    (with [f (fun (y) (openbox x))]
                          (f (setbox x 7)))))
      7)

(test (calc* '(with [x (newbox 5)]
                    ((seqn (setbox x 7)
                           (fun (x) x))
                     (openbox x))))
      7)
