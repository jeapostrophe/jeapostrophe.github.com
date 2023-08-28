#lang racket/base

;; Chapter 22

;; 22.1
;; - G = (V, E) where E = (V,V)
;; - Adj list vs matrix
;; - One bit in a matrix really means you need on ceil(lg |V|)^2 space, not |V|^2
;; - Also can represent as objects + pointers

;; 22.1-1
;; - Express interms of |V| or |E|

;; 22.1-6
;; - What would the matrix look like?

;; 22.1-8
;; - Go back to the hash table chapter and work through the numbers

;; 22.2
;; - Look at Figure 22.3
;; - Why is the queue a FIFO?
;; - What if it were something different?
;; - The proof of BFS finding shortest paths is simple and shows you the structure of proofs about graphs

;; if v.color == GRAY
;;  v.d = min(v.d, u.d+1)
;;  if min(v.d, u.d+1) = u.d+COST(u,v)
;;   v.π = u

;; 22.2-1
;; - Practice the algorithm

;; 22.2-4
;; - Think about how to change the algorithm first
;; - What information was given before and is gone and how do you get it now?

;; 22.2-7
;; - There are many complicated ideas you could come up with, but think of one that uses BFS first

;; 22.3
;; - Can you think of this as BFS with a different queue?
;; - What good can imagine being done with a DFS forest?
;; - Why should we care about these different kinds of edges?

;; 22.3-1
;; - This will reinforce your understanding of the algorithm and the edge types

;; 22.3-8
;; - A three node graph is sufficient to show this

;; 22.3-11
;; - Again three is sufficient, just play with the edges and the order the vertices are visited

;; 22.4
;; - Why is this useful?

;; 22.4-4
;; - In this exercise, a "bad" edge refers to when you put on your shoes before your socks.

;; 22.5
;; - Try to believe that this problem matters, later in the book or examples of use, but you may just need faith.
;; - Why this works is non-intuitive, so understand the proof
;; - The discussion above Theorem 22.16 is the most clear, IMHO

;; 22.5-1
;; - Can two components merge? Can two components split? Why and when?

