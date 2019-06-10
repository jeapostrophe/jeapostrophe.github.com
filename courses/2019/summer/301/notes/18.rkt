#lang teachlog
;; raco pkg install teachlog

;; Encode "It is wet, if it is raining" "It is raining" "Is it wet? -> Yes"
(relation Raining 0)
(relation Wet 0)
(:- Wet Raining)
(:- Raining)
(? Wet)

;; Family Tree
(relation Parent 2)
(:- (Parent "Daenerys" "Drogon"))
(:- (Parent "Aerys-2" "Daenerys"))
(:- (Parent "Aerys-2" "Viserys"))
(:- (Parent "Aegon-5" "Aerys-2"))
(:- (Parent "Maekar" "Aegon-5"))

(? (Parent "Daenerys" "Drogon"))

(? (Parent "Daenerys" X))
(next)

(? (Parent X "Daenerys"))

(? (Parent X Y))
(next)

(relation Ancestor 2)
(:- (Ancestor X Y)
    (Parent X Y))
(:- (Ancestor X Z)
    (Parent X Y)
    (Ancestor Y Z))

(? (Ancestor "Maekar" "Drogon"))
(? (Ancestor "Maekar" X))
(next)
(next)
(next)
(next)
(next)
(next)

;; Logic Programming! ∈ Declarative Programming = "You write the
;; answer, not how to get it"
