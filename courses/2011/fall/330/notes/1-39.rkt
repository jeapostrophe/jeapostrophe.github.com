#lang planet jaymccarthy/parenlog

(advisor greg shriram)
(advisor jay shriram)
(advisor arjun shriram)

(advisor shriram matthias)

(advisor matthias dan)

(? (advisor jay shriram))
(? (advisor shriram jay))

(:- (advisee Y X)
    (advisor X Y))

(? (advisee jay shriram))
(? (advisee shriram jay))

(:- (ancestor X Y)
    (advisor X Y))

(:- (ancestor X Z)
    (advisor X Y)
    (ancestor Y Z))

(? (ancestor jay shriram))
(? (ancestor jay dan))

(? (ancestor jay Y))

(? (ancestor X Y))

;;;;;

(type-of Gamma number num)

(type-of Gamma true bool)
(type-of Gamma false bool)

(:- (type-of Gamma (+ LHS RHS) num)
    (type-of Gamma LHS num)
    (type-of Gamma RHS num))

(? (type-of mt (+ number number) num))
(? (type-of mt (+ number true) num))

(lookup (cons (Id => Type) Gamma)
        Id
        Type)

(:- (lookup (cons (Id => Type) Gamma)
            DiffId
            DiffType)
    (lookup Gamma DiffId DiffType))

(:- (type-of Gamma (var Id) Type)
    (lookup Gamma Id Type))

(? (type-of (cons (x => num) mt)
            (var x)
            num))
(? (type-of (cons (x => num) mt)
            (var x)
            bool))

(:- (type-of Gamma (fun Id Body) (Dom -> Rng))
    (type-of (cons (Id => Dom) Gamma) Body Rng))
    
(? (type-of mt
            (fun x (+ number (var x)))
            (num -> num)))

(? (type-of mt
            (fun x (+ number (var x)))
            (bool -> num)))
    
(:- (type-of Gamma (app FExpr AExpr) Rng)
    (type-of Gamma FExpr (Dom -> Rng))
    (type-of Gamma AExpr Dom))

(? (type-of mt
            (app (fun x (+ number (var x)))
                 number)
            num))
(? (type-of mt
            (app (fun x (+ number (var x)))
                 bool)
            num))

(? (type-of mt
            (app (fun x (+ number (var x)))
                 number)
            Type))

(? (type-of mt
            (fun x (var x))
            Type))

(? (type-of mt
            Prog
            num))

(? (type-of mt
            (fun x Prog)
            num))
