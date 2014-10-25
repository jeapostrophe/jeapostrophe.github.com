#lang racket/base
;; 9.1
;; - What's the problem
;; - Isn't the double way cute?
;; - Is there an analogy to decision trees?

;; 9.1-1
;; - Which elements CAN be the second smallest?
;; - How many are there?
;; - How could you find the smallest of THOSE?

;; 9.2
;; - Recall that after running, partition puts ONE element in the right spot
;; - So, just run quicksort until partition puts the iTH element in the right spot
;; - And don't bother running it on the sides of the partition where k isn't
;; - Lots of math, huh?

;; 9.2-1
;; - This is a code question: look at the pseudo code where this could
;; happen and work backwards

;; 9.2-4
;; - This is to convince you that the algorithm really works. Go
;; through the example to see it.

;; 9.3
;; - Perhaps the most complex algorithm we've see so far. (Maybe
;; Strassen's was more complicated.)
;; - The baffling (7n/10 + 6) is (N - (3n/10 - 6)) which the text
;; computes.
;; - I love this insane stuff like the constant 140
;; - Why do we never say what 'a' is? What would it really be?
;; - Why is this "of more theoretical interest"? I hope someone
;; experiments on this.

;; 9.3-1
;; - You should have asked this question to yourself when reading
;; - Part 1: Change the 5s to 7s and check.
;; - Part 2: Change the 5s to 3s and check.

;; 9.3-5
;; - Do you udnerstand the point of the weird wrangling in the SELECT
;; algorithm?

;; 9.3-8
;; - Does this problem have any real-world relevance?
;; - Think about how to VERIFY if a particular entry is the answer
;; - Then think about how to SEARCH for that entry

;; 9-1
;; - I hope that this problem is easy for you because you're applying
;; other things.
;; - What can you learn about the world of algorithm design from this
;; experiment?
