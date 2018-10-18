;; command: raco pkg install iswim
;; DrRacket: File > Install a Package
#lang iswim 
;; #lang iswim (#:new|#:fun|#:parens|#:tree|)

;; Prim = +, *, add1
;; Constants = numbers

zero := (λ (f z) z)
succ := (λ (n)
          (λ (f z)
            (f (n f z))))

to-num := (λ (n) (n add1 0))

(+ 4 (* 2 ((λ (x) x)
           (to-num
            (succ
             (succ
              (succ
               (succ zero))))))))
