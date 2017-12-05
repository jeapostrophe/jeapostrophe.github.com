#lang parenlog
;; Parenlog is an implementation of Prolog (or logic programming)

(type-of Γ true ty:bool)
(type-of Γ false ty:bool)

(type-of Γ zero ty:num)
(:- (type-of Γ (succ M) ty:num)
    (type-of Γ M ty:num))

(? (type-of Γ (succ zero) ty:num))

(:- (type-of Γ (zero? M) ty:bool)
    (type-of Γ M ty:num))

;; Γ ⊢ LHS : Num
;; Γ ⊢ RHS : Num
;; --------------------
;; Γ ⊢ (+ LHS RHS) : Num
(:- (type-of Γ (+ LHS RHS) ty:num)
    (type-of Γ LHS ty:num)
    (type-of Γ RHS ty:num))

(:- (type-of Γ (< LHS RHS) ty:bool)
    (type-of Γ LHS ty:num)
    (type-of Γ RHS ty:num))

(? (type-of Γ (< zero (+ (succ zero) (succ zero)))
            ty:bool))

(:- (type-of Γ (λ (X : T) M) (-> T MT))
    (type-of (ext Γ X T) M MT))

(:- (type-of Γ (var X) XT)
    (in Γ X XT))

(in (ext Γ X XT) X XT)
(:- (in (ext Γ Y YT) X XT)
    (in Γ X XT))

(? (type-of mt
            (λ (x : ty:num)
              (< zero (+ (succ zero) (var x))))
            (-> ty:num ty:bool)))

;; (type-of Input Input Output)
;; You give the environment Γ and the expression M
;; You get the Type

(? (type-of mt M ty:bool))
next
next
next
next

(? (type-of mt
            (λ (x : T)
              (< zero (+ (succ zero) (var x))))
            (-> ty:num ty:bool)))
next

(? (type-of (ext mt x ty:num)
            (< zero (+ (succ zero) (var x)))
            ty:bool))

(? (type-of mt
            (λ (x : ty:num)
              (λ (y : ty:bool)
                (< zero (+ (succ zero) (var SomeVariable)))))
            (-> ty:num
                (-> ty:bool
                    ty:bool))))

(run zero zero)
(:- (run (succ N) (succ A))
    (run N A))

(run (+ zero N) N)
(:- (run (+ (succ M) N) (succ Ans))
    (run (+ M N) Ans))

(? (run (+ (succ (succ zero))
           (succ (succ zero)))
        Ans))

(? (run Program (succ zero)))
next
next
next

#;(? (run (Function zero) zero)
     (run (Function (succ zero)) (succ zero)))
