#lang parenlog

(adviser mccarthy toronto)

(? (adviser mccarthy toronto))

(adviser shriram mccarthy)

(? (adviser shriram mccarthy))

(? (adviser shriram Who))
next
next

(adviser shriram greg)
(adviser shriram arjun)

(adviser matthias shriram)
(adviser matthias matthew)
(adviser matthias robby)
(adviser dan matthias)

(:- (ancestor X Y)
    (adviser X Y))

;; Cond1 ... CondN
;; ---------------
;;   Conclusion

;; in Prolog:

;; (:- Conclusion
;;     Cond1
;;     ...
;;     CondN)

(? (ancestor matthias shriram))

(:- (ancestor X Y)
    (adviser X Z)
    (ancestor Z Y))

(? (ancestor matthias mccarthy))
next
next

(? (ancestor matthias Who))
next
next
next
next
next
next
next

(? (ancestor Who mccarthy))
next
next
next
next
next
next
next

(? (ancestor Who Whom))
next
next
next
next
next
next
next

(:- (sibling X Y)
    (adviser Z X)
    (adviser Z Y)
    (,(compose not equal?) X Y))

(? (sibling matthew robby))

(? (sibling matthew matthew))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(:- (typeof Gamma numConst num))
(:- (typeof Gamma boolConst bool))

(? (typeof mt numConst bool))
(? (typeof mt numConst T))

(:- (typeof Gamma (if Test True False) T)
    (typeof Gamma Test bool)
    (typeof Gamma True T)
    (typeof Gamma False T))

(? (typeof mt (if boolConst numConst numConst) num))
(? (typeof mt (if boolConst numConst numConst) bool))
(? (typeof mt (if boolConst numConst boolConst) num))
(? (typeof mt (if boolConst numConst boolConst) T))

(? (typeof mt (if boolConst numConst Here) num))
(? (typeof mt Program num))
next
next
next

(:- (in (V : T) (cons (V : T) Gamma)))
(:- (in (V1 : T1) (cons (V2 : T2) Gamma))
    (in (V1 : T1) Gamma))

(:- (typeof Gamma V T)
    (in (V : T) Gamma))

(? (typeof mt x T))
(? (typeof (cons (x : num) mt) x T))
(? (typeof (cons (y : bool) (cons (x : num) mt)) x T))
(? (typeof (cons (x : bool) (cons (y : num) mt)) x T))

(:- (typeof Gamma (lambda (V) Body) (Dom -> Rng))
    (typeof (cons (V : Dom) Gamma) Body Rng))

(? (typeof mt (lambda (x) (if x numConst numConst)) T))
(? (typeof mt (lambda (x) (if boolConst x numConst)) T))
(? (typeof mt (lambda (x) (if x x numConst)) T))
(? (typeof mt (lambda (x) x) T))

(:- (typeof Gamma (Fun Arg) Rng)
    (typeof Gamma Fun (Dom -> Rng))
    (typeof Gamma Arg Dom))

(? (typeof mt ((lambda (x) (if x numConst numConst)) boolConst) T))
(? (typeof mt ((lambda (x) (if boolConst x numConst)) numConst) T))
(? (typeof mt ((lambda (x) x) numConst) T))
(? (typeof mt ((lambda (x) (if x numConst numConst)) numConst) T))
(? (typeof mt ((lambda (x) (if boolConst x numConst)) boolConst) T))

(? (typeof mt ((lambda (x) x) Arg) T))
next
next
next
next
next

;; Prolog is...
;; * depth-first search (with unification)
;; * DFS means... that you can have infinite loops (with no answers)
