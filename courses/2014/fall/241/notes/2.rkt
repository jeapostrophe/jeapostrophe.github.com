loopinv : index -> proposition

loopinv(i) = True
loopinv(i) = i > 0

Initial:
 |= loopinv(start)
Maintenance:
 \forall i, loopinv(i) => loopinv(i+1)
Termination:
 loopinv(end) -> success!

for i = start to end
 stmt1
 stmt2
 stmt3

loopinv(j) = v \nin A[1..j-1]
loopinv(1) = v \nin A[1..0]
           = v \nin mt
           = True

if you return idx, then A[idx] = v
or you return NIL, if v isn't there

for j =1 to A.length
 if A[i] = v
  return j
return NIL

Hoare Logic

loopinv(j) = C[j..C.length] = A[j-1..A.length] + B...

1     for j = 1 to (A.length + 1)
2         C[j] = 0
3     for j = A.length down to 1
4         x = A[j] + B[j] + C[j+1]
5         if x > 1
6            C[j] = 1
7            x = x - 2
8         C[j+1] = x

----

Peano numbers

nat = 0
    | nat + 1

P : nat -> Prop

Goal: \forall n \in nat . P (n)

Induction:
 zero = You give me: P(0)
 succ = Next       : forall n, P(n) -> P(n+1)

P(5) = P(0+1+1+1+1+1)
     = succ (succ (succ (succ (succ zero))))

2.3-3:

 2^n+1

 2N = 2
    | 2 * 2N

 T(n) = 2                if n = 2
      | 2 * T(n/2) + n   o.w.

 Prop: T(n) == n lg n

 P(n) = T(n) == n lg n
 
 subgoal 1: P(2)
 T(2) == 2 lg 2
 T(2) == 2 * 1
 T(2) == 2
    2 == 2

 subgoal 2: P(n) -> P(2n)
 hypo: T(n) == n lg n
 T(2n) == 2n lg 2n
 2 * T(2n/2) + 2n == 2n lg 2n
 2 * T(n) + 2n == 2n lg 2n
 2 * (n lg n) + 2n == 2n lg 2n
 2n lg n + 2n == 2n lg 2n
 2n lg n + 2n == 2n (lg n + 1)
 2n lg n + 2n == 2n lg n + 2n

2.3-4:
 T(n) = 1          if n = 1
      | T(n-1) + 1 o.w.
