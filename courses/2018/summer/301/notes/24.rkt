#lang racket/base
(require racket/match)
(module+ test
  (require chk))

;; Expressions
(struct M ())
(struct :var M (x))
(struct :app M (m n))
(struct :lam M (x t m))

(struct :con M (b))
(struct :prim M (o))
(struct :if M (c t f))

(struct :unit M ())

(struct :pair M (f s))
(struct :fst M (m))
(struct :snd M (m))

(struct :inl M (m ty))
(struct :inr M (ty m))
(struct :case M (m xl tyl nl xr tyr nr))

(struct :fix M (m))

(struct :Lam M (A m))
(struct :App M (m ty))

;; Types
(struct T () #:transparent)
(struct Arr T (Dom Rng) #:transparent)

(struct Prim (p) #:transparent)
(define Num (Prim 'num))
(define Bool (Prim 'bool))

(struct Zero T () #:transparent)
(struct Unit T () #:transparent)
(struct Pair T (F S) #:transparent)
(struct Choice T (L R) #:transparent)

(struct Var T (A) #:transparent)
(struct ∀ T (A T) #:transparent)

(define (type-subst A AT T)
  (define (rec T) (type-subst A AT T))
  (match T
    [(Arr D R) (Arr (rec D) (rec R))]
    [(or (Zero) (Unit) (Prim _)) T]
    [(Pair F S) (Pair (rec F) (rec S))]
    [(Choice L R) (Choice (rec L) (rec R))]
    [(Var B) (if (equal? A B) AT T)]
    [(∀ B Tp)
     (if (equal? A B) T
         (∀ B (rec Tp)))]))

(define (type-compat X Y)
  (match* (X Y)
    [(T T) #t]
    [((Arr XD XR) (Arr YD YR))
     (and (type-compat XD YD) (type-compat XR YR))]
    [((Choice XD XR) (Choice YD YR))
     (and (type-compat XD YD) (type-compat XR YR))]
    [((Pair XD XR) (Pair YD YR))
     (and (type-compat XD YD) (type-compat XR YR))]
    [((∀ A X) (∀ B Y))
     (define C (gensym))
     (type-compat (type-subst A (Var C) X)
                  (type-subst B (Var C) Y))]
    [(_ _) #f]))

(define (type-of-constant b)
  (match b
    [(? number?)  Num]
    [(? boolean?) Bool]))

(define PRIM-TABLE
  (hasheq add1 (Arr Num Num)
          zero? (Arr Num Bool)
          sub1 (Arr Num Num)))
(define (type-of-prim o)
  (hash-ref PRIM-TABLE o))

(define (type-of* Γ m)
  (match m
    [(:con b) (type-of-constant b)]
    [(:prim o) (type-of-prim o)]
    [(:var x) (hash-ref Γ x)]
    [(:app m n)
     (match-define (Arr Dom1 Rng) (type-of* Γ m))
     (match-define Dom2 (type-of* Γ n))
     (unless (type-compat Dom1 Dom2)
       (error 'type-of "~v and ~v are incompatible" Dom1 Dom2))
     Rng]
    [(:lam x Dom m)
     (define Rng (type-of* (hash-set Γ x Dom) m))
     (Arr Dom Rng)]
    [(:if c t f)
     (match-define (== Bool) (type-of* Γ c))
     (define Res (type-of* Γ t))
     (match-define (== Res) (type-of* Γ f))
     Res]
    [(:unit) (Unit)]
    [(:pair m n)
     (Pair (type-of* Γ m) (type-of* Γ n))]
    [(:fst m)
     (match-define (Pair F _) (type-of* Γ m))
     F]
    [(:snd m)
     (match-define (Pair _ S) (type-of* Γ m))
     S]
    [(:inl m R)
     (Choice (type-of* Γ m) R)]
    [(:inr L m)
     (Choice L (type-of* Γ m))]
    [(:case m xl tyl nl xr tyr nr)
     (match-define (Choice (== tyl) (== tyr))
       (type-of* Γ m))
     (define Res (type-of* (hash-set Γ xl tyl) nl))
     (match-define (== Res)
       (type-of* (hash-set Γ xr tyr) nr))
     Res]
    [(:fix m)
     (match-define (Arr (Arr Td Tr)
                        (Arr Td Tr))
       (type-of* Γ m))
     (Arr Td Tr)]
    [(:Lam A m)
     (∀ A (type-of* Γ m))]
    [(:App m AT)
     (match-define (∀ A T) (type-of* Γ m))
     (type-subst A AT T)]))

(define (type-of m)
  (type-of* (hasheq) m))

(module+ test
  (chk (type-of (:unit))
       (Unit))
  (chk (type-of (:lam 'x (Unit) (:unit)))
       (Arr (Unit) (Unit)))
  (chk (type-of (:lam 'x (Unit) (:var 'x)))
       (Arr (Unit) (Unit)))
  (chk (type-of (:app (:lam 'x (Unit) (:var 'x)) (:unit)))
       (Unit))

  (chk (type-of (:app (:lam 'x Num (:var 'x)) (:con 5)))
       Num)
  (chk (type-of (:app (:prim add1)
                      (:app (:lam 'x Num (:var 'x)) (:con 5))))
       Num)
  (chk (type-of (:app (:prim zero?) (:con 10)))
       Bool)

  (chk #:x (type-of (:app (:prim zero?) (:con #t))) exn:fail?)
  (chk (type-of (:if (:app (:prim zero?) (:con 10))
                     (:prim add1)
                     (:prim sub1)))
       (Arr Num Num))
  (chk #:x (type-of (:if (:app (:prim zero?) (:con 10))
                         (:prim add1)
                         (:con 0)))
       exn:fail?)

  (chk (type-of (:pair (:con 10) (:con #t)))
       (Pair Num Bool))
  (chk (type-of (:fst (:pair (:con 10) (:con #t))))
       Num)
  (chk (type-of (:snd (:pair (:con 10) (:con #t))))
       Bool)
  (chk (type-of (:lam 'x (Pair Num Bool) (:snd (:var 'x))))
       (Arr (Pair Num Bool) Bool))

  (chk (type-of (:inl (:con 10) Bool))
       (Choice Num Bool))
  (chk (type-of (:inr Num (:con #t)))
       (Choice Num Bool))
  (chk (type-of (:lam 'x Num
                      (:if (:app (:prim zero?) (:var 'x))
                           (:inl (:var 'x) Bool)
                           (:inr Num (:con #f)))))
       (Arr Num (Choice Num Bool)))

  (chk (type-of (:fix
                 (:lam 'count (Arr Num Num)
                       (:lam 'x Num
                             (:if (:app (:prim zero?) (:var 'x))
                                  (:con 0)
                                  (:app (:prim add1)
                                        (:app (:var 'count)
                                              (:app (:prim sub1)
                                                    (:var 'x)))))))))
       (Arr Num Num))

  (chk (type-of (:Lam 'α (:lam 'x (Var 'α) (:var 'x))))
       (∀ 'α (Arr (Var 'α) (Var 'α))))
  (chk (type-of
        (:app (:lam 'id (∀ 'β (Arr (Var 'β) (Var 'β)))
                    (:if (:app (:App (:var 'id) Bool)
                               (:con #t))
                         (:app (:App (:var 'id) Num)
                               (:con 0))
                         (:con 10)))
              (:Lam 'α (:lam 'x (Var 'α) (:var 'x)))))
       Num))
