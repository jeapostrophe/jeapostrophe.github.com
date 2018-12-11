#lang racket/base
(require racket/match
         racket/set
         racket/pretty
         racket/list)
(module+ test
  (require chk))

(struct :var (x) #:transparent)
(struct :lam (x t m) #:transparent)
(struct :app (m n) #:transparent)
(define (:let x xt xe b)
  (:app (:lam x xt b) xe))
(struct :con (b) #:transparent)

(struct Base (b) #:transparent)
(struct Fun (dom rng) #:transparent)

(define Num (Base 'Num))
(define Bool (Base 'Bool))

;; Adding forms
(struct :if (c t f) #:transparent)
(struct :fix (m) #:transparent)

;; Adding pairs
(struct :pair (m n) #:transparent)
(struct :fst (m) #:transparent)
(struct :snd (m) #:transparent)
(struct Prod (x y) #:transparent)

;; Adding mutation
(struct :ref (m) #:transparent)
(struct :deref (m) #:transparent)
(struct :set! (m n) #:transparent)
(struct Ref (x) #:transparent)

(module+ test
  (define ex
    (:let 'x Num (:con 5)
          (:app (:app (:con +)
                      (:var 'x))
                (:var 'x)))))

(define (constant->type b)
  (match b
    [(? number?) Num]
    [(? boolean?) Bool]
    [(== zero?) (Fun Num Bool)]
    [(== add1) (Fun Num Num)]
    [(== sub1) (Fun Num Num)]
    [(== +) (Fun Num (Fun Num Num))]
    [(== *) (Fun Num (Fun Num Num))]))

(define (<: x y) (equal? x y))

(define (type-of m)
  (define (step Γ m)
    (define (rec m) (step Γ m))
    (match m
      [(:var x)
       (hash-ref Γ x (λ () (error 'type-of "Free variable: ~e" x)))]
      [(:lam x xt m)
       (Fun xt (step (hash-set Γ x xt) m))]
      [(:con b)
       (constant->type b)]
      [(:app m n)
       (define mt (rec m))
       (define nt (rec n))
       (match mt
         [(Fun dom rng)
          (unless (<: nt dom)
            (error 'type-of "apply: given ~e, expected ~e" nt dom))
          rng]
         [_
          (error 'type-of "Tried to apply non-function")])]
      [(:if c t f)
       (define ct (rec c))
       (unless (<: ct Bool) (error 'type-of "Can't if non-bools"))
       (define tt (rec t))
       (define ft (rec f))
       (unless (<: tt ft) (error 'type-of "if branches don't match"))
       tt]
      [(:fix m)
       (match (rec m)
         [(Fun dom rng)
          (unless (<: dom rng)
            (error 'type-of "fix fun must be A->A for some A"))
          rng]
         [_ (error 'type-of "fix must have fun arg")])]
      ;; Pairs
      [(:pair m n)
       (Prod (rec m) (rec n))]
      [(:fst m)
       (match (rec m)
         [(Prod ft _)
          ft]
         [_ (error 'type-of "Can't fst this!")])]
      [(:snd m)
       (match (rec m)
         [(Prod _ st)
          st]
         [_ (error 'type-of "Can't snd this!")])]
      ;; References
      [(:ref m)
       (Ref (rec m))]
      [(:deref m)
       (match (rec m)
         [(Ref t) t]
         [_ (error 'type-of "can't deref this")])]
      [(:set! m n)
       (match-define (Ref mt) (rec m))
       (match-define (== mt) (rec n))
       Bool]))
  (step (hasheq) m))

(module+ test
  (chk (type-of ex) Num)
  (chk #:x (type-of (:app (:con 5) (:con 3))) "type-of")
  (chk (type-of
        (:let 'fac (Fun Num Num)
              (:fix
               (:lam 'rec (Fun Num Num)
                     (:lam 'n Num
                           (:if (:app (:con zero?) (:var 'n))
                                (:con 1)
                                (:app (:app (:con *) (:var 'n))
                                      (:app (:var 'rec)
                                            (:app (:con sub1) (:var 'n))))))))
              (:app (:var 'fac) (:con 5))))
       Num)
  (chk (type-of (:let 'p (Prod Num (Fun Num Num))
                      (:pair (:con 5) (:lam 'x Num (:var 'x)))
                      (:app (:snd (:var 'p)) (:fst (:var 'p)))))
       Num)
  (chk (type-of (:let 'p (Ref (Prod Num (Fun Num Num)))
                      (:ref (:pair (:con 5) (:lam 'x Num (:var 'x))))
                      (:let '_ Bool
                            (:set! (:var 'p)
                                   (:pair (:con 6) (:snd (:deref (:var 'p)))))
                            (:app (:snd (:deref (:var 'p)))
                                  (:fst (:deref (:var 'p)))))))
       Num))

;;; Inference
(struct Infer (ty cs vs) #:transparent)

(struct Var (a) #:transparent)
(struct Eq (x y) #:transparent)

(define (type-subst v y t)
  (define (rec t) (type-subst v y t))
  (match t
    [(Base _) t]
    [(Fun dom rng)
     (Fun (rec dom) (rec rng))]
    [(Var a)
     (if (eq? a v) y t)]))

(define (type-subst/cs v vt cs)
  (for/list ([c (in-list cs)])
    (match-define (Eq X Y) c)
    (Eq (type-subst v vt X)
        (type-subst v vt Y))))

(define (type-subst* lo-v*t t)
  (for/fold ([t t]) ([v*t (in-list lo-v*t)])
    (match-define (Eq (Var v) y) v*t)
    (type-subst v y t)))

(define (generate-constraints Γ m)
  (define (rec m) (generate-constraints Γ m))
  (match m
    [(:var x) (Infer (hash-ref Γ x) empty (seteq))]
    [(:con b) (Infer (constant->type b) empty (seteq))]
    [(:lam x _ m)
     (define a (gensym x))
     (define A (Var a))
     (match-define (Infer rng cs vs) (generate-constraints (hash-set Γ x A) m))
     (Infer (Fun A rng)
            cs
            (set-add vs a))]
    [(:app m n)
     (define a (gensym 'app)) (define A (Var a))
     (match-define (Infer T1 C1 X1) (rec m))
     (match-define (Infer T2 C2 X2) (rec n))
     (Infer A
            (list* (Eq T1 (Fun T2 A)) (append C1 C2))
            (set-add (set-union X1 X2) a))]
    [(:if c t f)
     (match-define (Infer T1 C1 X1) (rec c))
     (match-define (Infer T2 C2 X2) (rec t))
     (match-define (Infer T3 C3 X3) (rec f))
     (Infer T2
            (list* (Eq T1 Bool) (Eq T2 T3)
                   (append C1 C2 C3))
            (set-union X1 X2 X3))]
    [(:fix m)
     (define a (gensym 'fix)) (define A (Var a))
     (match-define (Infer T1 C1 X1) (rec m))
     (Infer A
            (list* (Eq T1 (Fun A A)) C1)
            (set-add X1 a))]))

(define (unify cs)
  (define (step cs sol)
    (match cs
      [(list) sol]
      [(cons (Eq T T) cs)
       (step cs sol)]
      [(cons (Eq (Fun dom1 rng1) (Fun dom2 rng2)) cs)
       (step (list* (Eq dom1 dom2) (Eq rng1 rng2) cs) sol)]      
      [(cons (and sol1 (Eq (Var a) T)) cs)
       (step (type-subst/cs a T cs)
             (list* sol1 (type-subst/cs a T sol)))]
      [(cons (Eq T (Var a)) cs)
       (step (list* (Eq (Var a) T) cs) sol)]
      [(cons (Eq X Y) cs)
       (error 'unify "Fails to unify: ~v ≠ ~v" X Y)]))
  (step cs empty))

(define (type-infer m)
  (match-define (and res (Infer mt cs vs))
    (generate-constraints (hasheq) m))
  (pretty-print `(generate => ,res))
  (define sol (unify cs))
  (pretty-print `(unify => ,sol))
  (type-subst* sol mt))

(module+ test
  (chk (type-infer
        (:let 'fac #f
              (:fix
               (:lam 'rec #f
                     (:lam 'n #f
                           (:if (:app (:con add1) (:var 'n))
                                (:con 1)
                                (:app (:app (:con *) (:var 'n))
                                      (:app (:var 'rec)
                                            (:app (:con sub1) (:var 'n))))))))
              (:app (:var 'fac) (:con 5))))
       Num))
