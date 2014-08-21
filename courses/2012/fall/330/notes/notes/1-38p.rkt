#lang parenlog

;; Basic family tree
(advisor barwise feferman)
(advisor feferman tarski)
(advisor tarski lesniewski)
(advisor lesniewski twardowski)
(advisor twardowski brentano)
(advisor brentano clemens)

(? (advisor barwise feferman))
(? (advisor feferman barwise))

(:- (ancestor X Y)
    (advisor X Y))

(? (ancestor barwise feferman))

(:- (ancestor X Y)
    (advisor X Z)
    (ancestor Z Y))

(? (ancestor barwise tarski))

(? (ancestor tarski barwise))

(advisor brentano trendelenburg)

(? (ancestor barwise clemens))
(? (ancestor barwise trendelenburg))

(:- (descendant X Y)
    (ancestor Y X))

(advisee tarski montague)
(advisee tarski mostowski)
(advisee tarski robinson)

(:- (advisor X Y)
    (advisee Y X))

(? (descendant clemens montague))
(? (descendant trendelenburg montague))
(? (descendant feferman montague))
(? (descendant barwise montague))

(? (ancestor barwise X))
next
next
next
next
next
next
next
(? (ancestor X Y))

(type Gamma numConst num)
(type Gamma boolConst bool)

(:- (type Gamma (if Cond Then Else) Tau)
    (type Gamma Cond bool)
    (type Gamma Then Tau)
    (type Gamma Else Tau))

(? (type mt (if numConst numConst numConst) num))

(type (cons (V : T) Gamma)
      (var V)
      T)
(:- (type (cons (V1 : T1) Gamma)
          (var V2)
          T2)
    (type Gamma
          (var V2)
          T2))

(? (type (cons (w : bool) (cons (v : num) mt))
         (var v)
         num))

(:- (type Gamma (lambda (Var) Body) (T1 -> T2))
    (type (cons (Var : T1) Gamma) Body T2))

(? (type mt (lambda (x) (if (var x) numConst boolConst)) T))

(? (type mt (lambda (x) (if (var x) numConst numConst)) T))

(:- (type Gamma (app Fun Arg) T2)
    (type Gamma Fun (T1 -> T2))
    (type Gamma Arg T1))

(? (type mt (app (lambda (x) (if (var x) numConst numConst)) boolConst) T))

(? (type mt (lambda (x) (var x)) T))

(? (type mt (lambda (x) (app (var x) (var x))) num))

;; no occurs:
;; (? (type mt (lambda (x) (app (var x) (var x))) T))
