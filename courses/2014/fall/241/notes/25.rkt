#lang racket/base

;; 34
;; - The concept of the verification certificate is very important
;; - We know P \subseteq NP, the question is whether it's \subset or \eq
;; - What do you need to know about this?
;; - MANY "simple" problems are NP
;; - If your problem is, try not to have that problem
;; - Or do approximations.
;; - Does P = NP matter?

;; - Very new techniques in this chapter

;; - Optimization vs decision problems
;; - Reductions (Fig 34.1)

;; Approximiation
;; === You want 2 * Minimum and you can do that in n^4 but the Minimum is NP

;; Approximiation scheme
;; === You want (1+e) * Minimum and you can do that in n^f(e) but the Minimum is NP


;; 34.1
;; - The closure properties of polynomials are a big deal.
;; - These definitions are nice, but don't bother memorizing them
;; - The text claims you should watch out for cases when the standard encoding is not obvious... can you think of any?

;; 34.1-1
;; - The most important words here are "Define". You aren't solving it, you're just defining the problem

;; 34.1-5
;; - What do these calls look like in math?
;; - Solve the formula


;; P(X) = P_0*X^0 + ... + P_n*X^n
;; T(X) = T_0*X^0 + ... + T_n*X^n

;; R_i = K*P_i + R_i

;; R(X) = K * P(X) + T(X)
;; R(X) = R_0*X^0 + ... + R_n*X^n

;; R'(X) = X * P(X) + T(X)

;; R'(X) = X^K * P(X) + T(X)

;; R'(X) = 2^X * P(X) + T(X)
;; R'(X) = P(2^X) + T(X)

;; R'(X) = X * P(X) + R'(X-1)

;; 34.2
;; - "(It is always nice to know that an important set is nonempty.)"
;; - Verification is the key idea

;; 34.2-1
;; - http://en.wikipedia.org/wiki/Graph_isomorphism
;; - How would you check this? Look at the definition of the property

;; 34.3
;; - Reducibility
;; - You have to do a lot of proofs of reducibility huh?

;; - Recall that we are proving the first membership by considering other members that we don't know about yet

;; 34.3-1
;; - I suggest writing it out as boolean formulae and looking for a contradiction

;; 34.4
;; - The catalog of NPC problems is a big deal
;; - SAT - Name and conquer!

;; 34.4-1
;; - The smallest thing with 2-fan out

;; 34.5
;; - A catalog
;; - The "all -> specific" reduction technique is standard
;; - Widget concept is crazy
;; - Don't feel like you need to read every one

;; 34.5-2
;; A is MxN
;; x is Nx1
;; b is Mx1
;; Ax <= b
;; - n = 3, + = OR, * = and, b

;; 3-CNF-SAT...
;; (A || B || C) /\ (A || B || C) /\ (A || B || C)
;; C = Clauses
;; every Clause mentions 3 variables
;; every Clause will be a Row
;; every Row * Column (x) is going to be an Answer
;; Answer must be "True"
;; "True" is "> 0"

;; x is going to be at least one spot for every variable plus one 1 at end
;; {A} ... {B} ... {C} > 0

;; x_1 \/ !x_2 \/ x_3
;; x_1 + (1 - x_2) + x_3 > 0
;; x_1 + -x_2 + x_3 + 1 > 0
;; (1, -1, 1, 1) (x) (x_1, x_2, x_3, 1) > 0
;; -1 * (1, -1, 1, 1) (x) (x_1, x_2, x_3, 1) <= 0
;; (-1, 1, -1, 0, -1,) (x) (x_1, x_2, x_3, x_4 1) <= 0
