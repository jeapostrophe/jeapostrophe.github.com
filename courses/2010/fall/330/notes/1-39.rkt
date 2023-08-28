#lang planet jaymccarthy/parenlog

(advisor shriram jay)

(advisor shriram arjun)

(? (advisor shriram arjun))

(? (advisor shriram X))

(:- (advisee X Y)
    (advisor Y X))

(advisee greg shriram)

(? (advisee X shriram))

(:- (descendent X Y)
    (advisee X Y))

(:- (descendent X Z)
    (advisee X Y)
    (descendent Y Z))

(advisor matthias shriram)

(advisor matthias mflatt)

(advisee tewk mflatt)

(advisee neil jay)

(? (descendent X matthias))

; Cycles are one way of getting infinite answers
#;(advisee matthias matthias)

;;; Insanity

(type-of Gamma number num)
(type-of Gamma boolean bool)

(:- (type-of Gamma (+ E1 E2) num)
    (type-of Gamma E1 num)
    (type-of Gamma E2 num))

(:- (type-of Gamma (if Eb Et Ef) T)
    (type-of Gamma Eb bool)
    (type-of Gamma Et T)
    (type-of Gamma Ef T))

(? (type-of mt (if boolean (+ number number) number) T))
(? (type-of mt (if boolean (+ number number) boolean) T))

(:- (type-of Gamma (fun X E) (-> I O))
    (type-of (extend Gamma X I) E O))

(? (type-of mt (fun x boolean) T))

(:- (type-of Gamma Var T)
    (in Gamma Var T))

(in (extend Gamma Var T) Var T)

(:- (in (extend Gamma Var0 T0) Var1 T1)
    (!= Var0 Var1)
    (in Gamma Var1 T1))

(? (type-of mt (fun x x) T))

(? (type-of mt (fun x (+ x x)) T))

(:- (type-of Gamma (E1 E2) T)
    (type-of Gamma E1 (-> I T))
    (type-of Gamma E2 I))

(? (type-of mt ((fun x (+ x x)) number) T))
(? (type-of mt ((fun x (+ x x)) boolean) T))

(? (type-of mt ((fun x x) boolean) T))

(? (type-of mt (fun f (+ (f number) (f boolean))) T))

(? (type-of mt P num))
(? (type-of mt P bool))

(? (type-of Gamma x num))

(? (type-of Gamma P T))

