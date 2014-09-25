
Kelp claims the algorithm PWI produces all permutations except
identity uniformly at random.

Kelp is forall N, "PWI(N) produces all permutations except identiy uniformly at random."

Kelp is wrong.

(not (forall x (P x)))
=
(exists x (not (P x)))

exists an N such NOT "PWI(N) produces all permutations except identiy uniformly at random."

How many permutations are produced?
Which ones? At what ratio?
Which are left?

for n = 2,
 for i = 1 to 1
  swap A[1] with A[2]

[2,1]

for n = 3
 for i = 1 to 2
  ...

 swap A[1] with A[2 OR 3]
 swap A[2] with A[3]

A = (1, 2, 3)
case 1.
 swap A[1] with A[2]
  A = (2, 1, 3)
 swap A[2] with A[3]
  A = (2, 3, 1)

case 2.
 swap A[1] with A[3]
  A = (3, 2, 1)
 swap A[2] with A[3]
  A = (3, 1, 2)

Produces:
231
312

123 I
132 ?
213 ?
231 X
312 X
321 ?



    1/n for someone to match Jay
(n-1)/n for someone NOT match jay

1 - 1/n = (n-1)/n

;;; k/n =
Pr(For 1 person in k to match Jay)
=
1 - Pr(For k persons to NOT match Jay)
=
1 - Pr(For 1 person to NOT match Jay)^k
=
1 - (n-1/n)^k

...........

1 - (Pr(1 Born on the 4th of July aka Tom Cruise) + Pr(0 Born))
= 

Pr(0 Born) = (n-1/n)^k
Pr(1 Born) = Pr(1 Born & k-1 people Not) = k*(1/n)(n-1/n)^{k-1}

