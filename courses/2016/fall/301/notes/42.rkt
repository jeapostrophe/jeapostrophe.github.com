#lang plai

;; (#%module-begin ....)

;; Hoare Logic

;; Hoare Triple:
;;; { Pre-Condition } Program { Post-Condition }

;; { True } "X = num(Y)" { X = Y }

;; { Pre } Code { Post' }
;; Post' => Post
;; ----------------------
;; { Pre } Code { Post }

;; {   Cond }  True { P }
;; { ~ Cond } False { P }
;; -------------------------------------
;; { True } "If Cond Then True Else False" { P }

;; { Pre } First { Middle }
;; { Middle } Rest { Post }
;; -----------------------------------
;; { Pre } (begin First Rest) { Post }

;; vs declarative:

;; HasProp First
;; HasProp Second
;; ------------------------
;; HasProp (+ First Second)

;; http://en.wikipedia.org/wiki/Esoteric_programming_language
;; QVICKBASIC
;; AOP - Aphorism oriented programming
;; 2D

;; Expressiveness: technical or aesthetic? just taste?

;; adviser(Shriram Krishnamurthi, Matthias Felleisen)
;; adviser(Jay McCarthy, Shriram Krishnamurthi)

;; "On the Expressive Power of Programming Languages"

;; A language L = {F_0, ..., F_n}
;; L' = L + F_n+1

;; Improve code size:
;; forall X
;; Program P in L it does X and has measure M
;; Program P' in L' also does X and has measure M' s.t. M' < M

;; Distinguish languages
;; Exists X & Y. Forall E, In L, E[X] = E[Y]
;; In L', exists E', such that E'[X] != E'[Y]

;; X
(lambda (x) 
  (if (x) 
    (if (x) 0 1)
    (if (x) 1 0)))

;; Y
(lambda (x)
  0)

;; E'
(let ([called? #f])
  (λ ()
    (if called?
      #f
      (begin (set! called? #t)
             #t))))

;; Compile L' to L?
;; Idea one (local compilation): E[L'] = E[compile(L')]
;; Idea two (global) = compile(L')

;; Local compilations MEANS "can be implemented by a Racket macro"
;; Racket macros CAN do global compilation as well

'lazy
'racket
'racket/base
'pre-racket
'#%kernel
'VM ;; --- +, modulo, expand

;; ----------------------------------------

;; What different ways are programs implemented? What are the
;; costs/benefits of each way?

;; Is it truly useful to implement a language in another
;; higher-level language, e.g. Python in C?

;; Now that we have discussed many different principles and
;; paradigms of programming languages, what principles are
;; expressed in the common languages we know and use frequently?

;; Nitty-gritty of how Racket is actually implemented. Interesting
;; aspects of actually creating a real-world language.

;; In "Beating the Averages" Graham talked about Macros. I think we
;; touched on these when we talked about defining syntax rules...but I'm
;; not sure. They sounded interesting though.
