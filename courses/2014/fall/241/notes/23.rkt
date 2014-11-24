#lang racket/base

;; Ch 23
;; - MST matters
;; - Good example of optimization problem
;; - Good example of greedy algorithm (which we skipped the chapter about)

;; 23.1
;; - Very funny meta-algorithm
;; - Cuts are a very useful concept independent of this problem
;; - Understand this proof in detail
;; - You will probably flip back and forth to revisit the definition of terms like "respects"
;; - Corollary 23.2 is subtle due to the mass of text for a simple idea, watch out!

;; 23.1-1
;; - The key word is "some MST"
;; - Interpret "minimum-weight edge" as an edge at the bottom of a <= chain (min in the sorted list of edges)

;; 23.1-3
;; - Think about the corollary

;; 23.1-10
;; - Think about the details of the proof that mention w

;; 23.2 - Kruskal
;; - The code here should be readable to you
;; - Steps f and h are informative
;; - The running time calculation on page 654 is subtle and not SUPER important

;; 23.2 - Prim
;; - I find the code much more complicated than Kruskal's
;; - As you look at the figure, try to decide which node/edge will be looked at next

;; 23.2-1
;; - Look at the example and where it broke ties
;; - Show how to "fix" just one edge via sorting

;; 23.2-2
;; - What does a connected graph look like in a adj mat
;; - Where is the light edge that Prim tries to find?
;; - Use your knowledge that the runtime is going to be V^2 to decide the algorithm

;; 23.2-4
;; - Choose a representation that uses this fact and see how to make the algorithm simpler with it
;; - Realize that this just improves the disjoint-set operations
