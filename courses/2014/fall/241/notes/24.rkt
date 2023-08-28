#lang racket/base

;; 24
;; - Think about the variants, they matter a lot
;; - Think about why optimal substructure exists
;; - In the "real world", negative-weight edges are like time-travel
;; - Cycles help us realize how many edges the path may contain
;; - Relaxation is like "safe edges" in defining the means by which all the algorithms work
;; - At this point, don't bother going into depth what the properties do

;; Notice that I assign problems only for 24.1 and 24.3--skim the rest.

;; 24.1
;; - Why do we need to look at each edge V times?

;; 24.1-1
;; - Practice!

;; 24.1-3
;; - Fixed point!

;; 24.1-4
;; - How do you detect this situation?

;; 24.3
;; - The code hides the decrease-key operation of managing the queue's order
;; - How can you apply this to situations where the set of vertexes is not known in advance?
;; - Read the proof
;; - Notice the use of aggregate analysis in the runtime

;; 24.3-1
;; - Practice

;; 24.3-2
;; - Think of a really small one
;; - Consider the example for the Bellaman-Ford and what happened along the negative edges

;; 24.3-4
;; - There are basic checks that you can do
;; - But really think about 24.1-4


