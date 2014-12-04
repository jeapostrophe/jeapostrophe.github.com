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

;; 34.1
;; - The closure properties of polynomials are a big deal.
;; - These definitions are nice, but don't bother memorizing them
;; - The text claims you should watch out for cases when the standard encoding is not obvious... can you think of any?

;; 34.1-1
;; - The most important words here are "Define". You aren't solving it, you're just defining the problem

;; 34.1-5
;; - What do these calls look like in math?
;; - Solve the formula

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
;; - n = 3, + = OR, * = and, b

