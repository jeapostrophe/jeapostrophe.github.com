;; 3.2-8
k ln k = \theta(n) => k = \theta(n / ln n)

;; Mushfiq
k ln k = \theta(n)
;; by symmetry
n = \theta(k ln k)
;; taking ln
ln n = \theta(ln (k ln k))
     = \theta(ln k + ln (ln k))
     = ... ??? ...
     = \theta(ln k)

n = \theta(k ln k)
;; divide by ln n
n        \theta(k ln k)
------ = --------------
ln n      \theta(ln k)
       = \theta( k ln k / ln k)
       = \theta( k )


;; Other way

 H => G

H : k ln k = \theta(n) =>
G : k = \theta(n / ln n)

H : (exists (c_1 c_2 n_0)
 forall n,
  n >= n_0 ->
  0 <= c_1 * n <= k ln k <= c_2 * n)
=>
G : (exists (c'_1 c'_2 n'_0)
 forall n',
  n' >= n'_0 ->
  0 <= c'_1 * (n'/ln n') <= k <= c'_2 * (n'/ln n'))
 
Focus on left ineq.

 H1:  c_1 * n          <= k ln k
 H1:  c_1 * n / ln k   <= k
SG1: c'_1 * (n'/ln n') <= k

Transitivity of <=
SG1': X <= Y ->
  H1: Y <= Z ->
 SG1: X <= Z

SG1': X <= Y
      c'_1 * (n'/ln n') <= c_1 * (n / ln k)

Assume c'_1 = c_1

Compatibility of * and <=
    R > 0
    X <= Y ->
R * X <= R * Y

SG1': (n' / ln n') <= (n / ln k)

  H7: n' >= n'_0

Principle of + >=
 X >= Y ->
 exists i. i >= 0 && X = Y + i

Apply the idea that theta works for all n GREATER than n_0
so we can choose n'_0 to be at least n_0

SG1': ((n'_0 + i) / ln (n'_0 + i)) <= ((n'_0 + i) / ln k)

Because when i gets larger, the sides don't get smaller, then we can
prove this for i = 0 and have a stronger theorem.

SG1': (n'_0 / ln n'_0) <= (n'_0 / ln k)

    Y >= Z    ->
X / Y <= X / Z

SG1': ln n'_0 >= ln k

SG1': n'_0 >= k

So any n'_0 greater than k makes the whole thing true
and c'_1 = c_1 and c'_2 = c_2

Definition of <=
  (X = Y \/ X < Y) ->
  X <= Y

P -> Q
if P contains \/ then you have to deal with both
if Q contains \/ then you get to choose which
This is called CONTRAVARIANCE

Liskov Substitution Principle
- An object, B, that is inheriting from A can take the place of A anywhere

M : Animal -> Number

N : Number -> Animal

Inheritance IS an OR

Animal is either a Dog or Cat
