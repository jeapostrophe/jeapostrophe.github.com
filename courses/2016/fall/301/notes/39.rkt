#lang parenlog

;; datalog

;; it is 'relational'

(adviser neil jay)
(? (adviser neil jay))

(adviser blake jay)
(? (adviser blake Who))

(adviser jay shriram)


;;  (adviser X Y)
;; --------------
;; (ancestor X Y)
(:- (ancestor X Y)
    (adviser X Y))

;;  (adviser X Y)
;; (ancestor Y Z)
;; --------------
;; (ancestor X Z)
(:- (ancestor X Z)
    (adviser X Y)
    (ancestor Y Z))

(? (ancestor neil Who))
next
next

;;;;;;;

(type-of Gamma num t-num)
(? (type-of mt num Ty))

(type-of Gamma true t-bool)
(type-of Gamma false t-bool)
(? (type-of mt true Ty))

(:- (type-of Gamma (iszero Num) t-bool)
    (type-of Gamma Num t-num))

(:- (type-of Gamma (+ LHS RHS) t-num)
    (type-of Gamma LHS t-num)
    (type-of Gamma RHS t-num))

(? (type-of mt (+ num num) t-num))
(? (type-of mt (+ true num) t-num))

(contains (Gamma [X -> Ty]) X Ty)

(:- (contains (Gamma [Y -> Tx]) X Ty)
    (contains Gamma X Ty))

;; Gamma[x] = Ty
;; ---------------------
;; Gamma :- (var x) : Ty

(:- (type-of Gamma (var X) Ty)
    (contains Gamma X Ty))

(? (type-of mt (var x) Ty))
(? (type-of (mt [x -> t-num]) (var x) Ty))

(:- (type-of Gamma (λ (Arg) Body) 
             (t-fun ArgTy BodyTy))
    (type-of (Gamma [Arg -> ArgTy]) Body BodyTy))

(? (type-of mt (λ (x) (+ (var x) (var x))) 
            (t-fun t-num t-num)))
next
(? (type-of mt (λ (x) (+ (var x) (var x))) 
            Ty))
next
(? (type-of mt (λ (x) (var x)) 
            Ty))
next

(? (type-of mt Prog t-num))
next
next
next

(? (type-of mt Prog t-bool))
next
next
next
next

(? (type-of mt Prog (t-fun t-num t-num)))
next

(? (type-of mt Prog (t-fun t-num t-bool)))
next

(? (type-of mt Prog (t-fun Alpha Alpha)))

;; iso nat nat        : identity
;; iso (nat, nat) nat : bit interleaving
;; iso (k-tuple nat) nat : iterate (nat,nat) k times
;; iso (list nat) nat : pair length with length-tuple
;; iso string (list nat)
;; iso (A or B) nat : pair (0 or 1 meaning A or B) nat encoding of A or B
