;; Yo!

4.3-1: T(n) = T(n-1) + n is O(n^2)

T(n)   = T(n-1) + n
T(n-1) = T(n-2) + (n-1)
T(n-2) = T(n-3) + (n-2)
...
T(2)   = T(1) + 2
T(1)   = T(0) + 1
T(0)   = 0

T(n)   = n + (n-1) + (n-2) + ... + 2 + 1
       = geometric series
       = (n * (n - 1)) / 2
       = n^2

4.3-1: T(n) = T(n-1) + n is O(n^2)
       cn^2 <= c(n-1)^2 + n
       cn^2 <= c(n-1)(n-1) + n
       cn^2 <= c(n^2-2n+1) + n
       cn^2 <= cn^2 - 2cn + c + n
       cn^2 <= cn^2 - (2c - 1)n + c
       cn^2 <= cn^2 - (2c - 1)n
       cn^2 <= cn^2

forall f,
 f(n) = Theta(f(floor(n)))

4.3-5:

T(n) = T(ceil(n/2)) + T(floor(n/2)) + Theta(n)

Theta(n lg n) = T(ceil(n/2)) + T(floor(n/2)) + Theta(n)

Theta(n lg n) = Theta(ceil(n/2) lg ceil(n/2)) 
              + Theta(floor(n/2) lg floor(n/2)) 
              + Theta(n)

Theta(n lg n) <= c(ceil(n/2) lg ceil(n/2)) 
               + c(floor(n/2) lg floor(n/2)) 
               + c(n)

Theta(n lg n) <= c((n/2) lg (n/2)) 
               + c((n/2) lg (n/2)) 
               + c(n)

Theta(n lg n) <= cn lg (n/2)
               + c(n)

Theta(n lg n) <= cn (lg n - lg 2)
               + c(n)

Theta(n lg n) <= cn lg n - lg 2 cn
               + c(n)

Theta(n lg n) <= cn lg n + (1 - lg 2)cn

Theta(n lg n) <= cn lg n + (1 - 1)cn

Theta(n lg n) <= cn lg n + 0cn

Theta(n lg n) <= cn lg n


4.5-1b

T(n) = 2T(n/4) + n^(1/2)

a = 2
b = 4
f(n) = n^(1/2)

n^(log_b a) = n^(log_4 2) = n^(1/2)

f(n) = Theta(n^(1/2))
via Case 2
T(n) = n^(1/2) * lg n

4.5-4

T(n) = 4T(n/2) + n^2 lg n

a = 4
b = 2
f(n) = n^2 lg n

n^(log_b a) = n^(log_2 4) = n^2

n^2 lg n ~~~ n^2
n^2 lg n = Omega(n^(2+e))
n^2 lg n >= c(n^(2+e))

// After class

We failed at doing this because it won't work. 

n^(2+e) = n^2 n^e

so we need to prove

lg n >= c n^e

but that's not necessarily true

So. Master Method fails.

The true answer is Theta(n^2 lg^2 n)

T(n) <= 4T(n/2) + (n^2 (lg^2 n))
     <= 4c(n/2)^2lg^2(n/2) + n^2 lg n
     <= c n^2 lg(n/2) lg n - c n^2 lg(n/2) lg 2 + n^2 lg n
     <= c n^2 n - c n^2 lg n lg 2 - c n^2 lg (n/2) + n^2 lg n
     <= c n^2 lg^2 n + (1 - c)n^2 lg n - c n^2 lg (n/2)
     <= c n^2 lg^2 n - c n^2 lg (n/2)
     <= c n^2 lg^2 n
