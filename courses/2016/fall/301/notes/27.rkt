#lang parenlog

;; forall Gamma,
(type-of Gamma (num SomeNum) ty-num)

(? (type-of mt (num five) Ty))
next

#;(type-of mt true ty-bool)

(type-of Gamma true ty-bool)
(type-of Gamma false ty-bool)
(? (type-of mt true Ty))

#;(:- (type-of Gamma (iszero Num) ty-bool)
    (type-of Gamma Num ty-num))

(? (type-of mt (iszero (num seven)) Ty))

(:- (type-of Gamma (+ LHS RHS) ty-num)
    (type-of Gamma LHS ty-num)
    (type-of Gamma RHS ty-num))

(? (type-of mt (+ (num monkey) (num eight)) Ty))

(? (type-of mt (+ (num monkey) true) Ty))

(contains (Gamma [X -> Ty]) X Ty)

(:- (contains (Gamma [Y -> Ty]) X Tx)
    (contains Gamma X Tx))

(:- (type-of Gamma (var X) Ty)
    (contains Gamma X Ty))

(? (type-of mt (var x) Ty))

(? (type-of (mt [x -> ty-num]) (var x) Ty))

(:- (type-of Gamma (λ (Arg) Body) (ty-fun ArgTy BodyTy))
    (type-of (Gamma [Arg -> ArgTy]) Body BodyTy))

(? (type-of mt (λ (x) (+ (var x) (var x))) Ty))

(? (type-of mt (λ (x) (iszero (var x))) Ty))

(? (type-of mt (λ (x) (λ (y) (var x))) Ty))

(:- (type-of Gamma (Fun Arg) RngTy)
    (type-of Gamma Fun (ty-fun ArgTy RngTy))
    (type-of Gamma Arg ArgTy))

(? (type-of mt Prog ty-num))
next
next
next

(? (type-of mt Prog ty-bool))
next
next
next
next
next

(? (type-of mt Prog (ty-fun Alpha Alpha)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#;
(:- (forever X Y)
    (forever Y X))
#;
(? (forever ecks why))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When Prolog works, it runs in EXPTIME
;; Datalog restricts it so it runs in CUBIC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(num zero)

(:- (num (succ Num))
    (num Num))

(? (num (succ (succ zero))))

(plus zero N N)

(? (plus zero (succ (succ zero)) Ans))

;; If N + M = O, then 1 + N + M = 1 + O
(:- (plus (succ N) M (succ O))
    (plus N M O))

(? (plus (succ (succ zero))
         (succ (succ zero))
         Ans))

(<= N N)

(:- (<= N (succ M))
    (<= N M))

(? (<= zero (succ (succ zero))))

(? (<= (succ (succ zero)) zero))

(sort empty empty)

(:- (sort (cons Head Tail) SortedVersion)
    (sort Tail SortedTail)
    (insert Head SortedTail SortedVersion))

(insert Any empty (cons Any empty))

(:- (insert X (cons Y Tail)
            (cons X (cons Y Tail)))
    (<= X Y))

(:- (insert X (cons Y Tail)
            (cons Y X-in-the-right-spot))
    (insert X Tail X-in-the-right-spot))

(? (sort (cons (succ zero) (cons zero empty))
         (cons zero (cons (succ zero) empty))))

(? (sort (cons zero (cons (succ zero) (cons zero empty)))
         Ans))


(? (sort Input
         (cons zero (cons (succ zero) empty))))
next
;; next ;; <- infinite loop
