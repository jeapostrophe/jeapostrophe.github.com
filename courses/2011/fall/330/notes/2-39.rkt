#lang planet jaymccarthy/parenlog

(advisor jay shriram)

(? (advisor jay shriram))
(? (advisor shriram jay))

(advisor arjun shriram)
(advisor greg shriram)

(advisor shriram matthias)
(advisor matthias dan)

(:- (ancestor X Y)
    (advisor X Y))

(? (ancestor jay shriram))

(:- (ancestor X Z)
    (advisor X Y)
    (ancestor Y Z))

(? (ancestor jay matthias))

(? (ancestor jay X))

(? (ancestor X Y))

;;;;;;;;

(type-of Gamma number num)

(? (type-of mt number num))
(? (type-of mt number bool))

(:- (type-of Gamma (+ LHS RHS) num)
    (type-of Gamma LHS num)
    (type-of Gamma RHS num))

(? (type-of mt (+ number number) num))
(? (type-of mt (+ number number) bool))
(? (type-of mt (+ true number) num))

(lookup (extend Gamma Id Type)
        Id
        Type)

(:- (lookup (extend Gamma Id Type)
            SomeId
            SomeType)
    (lookup Gamma
            SomeId
            SomeType))

(:- (type-of Gamma (var Id) Type)
    (lookup Gamma Id Type))

(? (type-of (extend mt x num) (var x) num))
(? (type-of (extend mt x bool) (var x) num))
(? (type-of (extend mt x num) (var x) bool))

(:- (type-of Gamma (λ Var Body) (Dom -> Rng))
    (type-of (extend Gamma Var Dom) Body Rng))

(? (type-of mt (λ x (+ (var x) number)) (num -> num)))
(? (type-of mt (λ x (+ (var x) true)) (num -> num)))
(? (type-of mt (λ x (+ (var x) number)) (bool -> num)))

(:- (type-of Gamma (app Fexpr Aexpr) Rng)
    (type-of Gamma Fexpr (Dom -> Rng))
    (type-of Gamma Aexpr Dom))

(? (type-of mt (app (λ x (+ (var x) number))
                    number)
            num))
(? (type-of mt (app (λ x (+ (var x) number))
                    true)
            num))

(? (type-of mt (app (λ x (+ (var x) number))
                    number)
            Type))

(? (type-of mt 
            Prog
            num))

(? (type-of mt 
            (λ x Prog)
            num))



