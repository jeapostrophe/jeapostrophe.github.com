#lang parenlog

(adviser mccarthy toronto)

(? (adviser mccarthy toronto))

(adviser shriram mccarthy)

(? (adviser shriram mccarthy))

(adviser shriram greg)
(adviser shriram arjun)

(adviser matthias shriram)
(adviser matthias robby)
(adviser matthias matthew)

(adviser dan matthias)

(? (adviser matthias Who))
next
next
next

(:- (ancestor X Y)
    (adviser X Y))

(? (ancestor matthias shriram))

(:- (ancestor X Y)
    (adviser Z Y)
    (ancestor X Z))

(? (ancestor matthias mccarthy))

(? (ancestor matthias Who))
next
next
next
next
next
next
next

(? (adviser Who shriram))
next

(? (adviser Adviser Advisee))
next

(? (ancestor Adviser Advisee))
next

(:- (sibling X Y)
    (adviser Z X)
    (adviser Z Y)
    (,(compose not equal?) X Y))

(? (sibling robby matthew))
(? (sibling robby mccarthy))
(? (sibling robby robby))

(? (sibling robby X))
next
next
next

(:- (typeof Gamma numConst num))

(? (typeof mt numConst num))
(? (typeof mt boolConst num))

(:- (typeof Gamma boolConst bool))

(:- (typeof Gamma (if Cond True False) T)
    (typeof Gamma Cond bool)
    (typeof Gamma False T)
    (typeof Gamma True T))

(? (typeof mt (if boolConst numConst boolConst) bool))
(? (typeof mt (if boolConst numConst numConst) bool))
(? (typeof mt (if boolConst numConst numConst) num))
(? (typeof mt (if numConst numConst numConst) num))

(? (typeof mt (if boolConst numConst boolConst) T))
(? (typeof mt (if boolConst numConst numConst) T))
(? (typeof mt (if boolConst numConst numConst) T))
(? (typeof mt (if numConst numConst numConst) T))

(? (typeof mt Program bool))
next
next
next

(? (typeof mt Program num))
next
next

(:- (in (cons (V : T) Gamma)
        V T))
(:- (in (cons (V1 : T1) Gamma)
        V2 T2)
    (in Gamma V2 T2))

(:- (typeof Gamma (var V) T)
    (in Gamma V T))

(? (typeof mt (var x) T))
(? (typeof (cons (x : bool) mt) (var x) T))
(? (typeof (cons (y : num) (cons (x : bool) mt)) (var x) T))

(:- (typeof Gamma (lambda (V) Body) (Dom -> Rng))
    (typeof (cons (V : Dom) Gamma) Body Rng))

(? (typeof mt (lambda (x) (if (var x) numConst numConst)) T))
next

(? (typeof mt (lambda (x) (var x)) T))

(:- (typeof Gamma (app Fun Arg) Rng)
    (typeof Gamma Fun (Dom -> Rng))
    (typeof Gamma Arg Dom))

(? (typeof mt (app (lambda (x) (if (var x) numConst numConst)) boolConst) T))
(? (typeof mt (app (lambda (x) (if (var x) numConst numConst)) numConst) T))
(? (typeof mt (app (lambda (x) (var x)) numConst) T))
(? (typeof mt (app (lambda (id)
                     (if (app (var id) boolConst)
                       (app (var id) numConst)
                       (app (var id) numConst)))
                   (lambda (x) (var x))) T))

(? (typeof mt Program T))
next
next
next

;; Prolog
;; - possible to go into infinite loops
;;   * a choice leads to infinite possibilites, none of which are good
;; + possible to get infinite good solutions
;; - defined as DFS, so not "fair"
;; + DFS is efficient
;; - hard to contort problem into a DFS

