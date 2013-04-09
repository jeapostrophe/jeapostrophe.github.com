#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/unit
                     racket/match
                     racket/list
                     data/heap)
          "../post.rkt")

@title{Branch and Bound in Racket}
@categories["Racket" "Algorithms"]

Based on the fun I had with the last post on tree search, I thought
this week I would do a generic branch and bound algorithm in
Racket. We'll make it generic by using Racket's unit system.

@(the-jump)

@link["http://en.wikipedia.org/wiki/Branch_and_bound"]{Branch and
bound} is an optimization algorithm that is based on being able to
have accurate estimates of the bounds on the utility of different sets
of choices. The key is to be able to provide the following functions:

@chunk[<bb-client-sig>
       (define-signature bb-client^
         (option? split upper-bound lower-bound inject extract))]

where @racket[split] takes an option set and returns some number of
subsets, while the @racket[upper-bound] and @racket[lower-bound]
functions return the respective bounds on the subset. For convenience
with option set representations that are different than the "natural"
representation of the problem, @racket[inject] converts the latter to
the former and @racket[extract] does the reverse.

@section{Trival problem instance}

A trivial instance of this unit is based on binary search trees, where
the option set data structure explicitly records the current bounds
based on the binary search invariant. (This example is especially
trivial because the branch-and-bound will just go all the way to the
left, because it will find the minimal value in the search space,
i.e. the tree.)

@chunk[<binary-bb>
       (struct node (key left right))

       (define-unit binary-bb@
         (import) (export bb-client^)
         
         (struct option (lower node upper))

         (define (inject n) (option -inf.0 n +inf.0))
         (define extract option-node)

         (define (split os)
           (match os
             [(option lower #f upper) 
              empty]
             [(option lower (node key left right) upper)
              (list (option lower left key)
                    (option key (node key key key) key)
                    (option key right upper))]))

         (define upper-bound option-upper)
         (define lower-bound option-lower))]

The branch and bound algorithm itself is parameterized over this
interface and is a single function that takes a search space and
returns the optimal solution.

Here's an example of how it should work:

@chunk[<binary-test>
       (define (binary-insert k bt)
         (match bt
           [#f
            (node k #f #f)]
           [(node nk left right)
            (if (< k nk)
              (node nk (binary-insert k left) right)
              (node nk left (binary-insert k right)))]))
       
       (define the-tree 
         (foldr binary-insert #f
                (shuffle (build-list 1000 add1))))

       (check-equal? (node-key (find binary-bb@ the-tree)) 1)]

@section{Branch and bound}

The shell of the branch and bound function is:

@chunk[<bb>
       (define (find client@ start)
         (define count 0)
         (define-values/invoke-unit client@ (import) (export bb-client^))
         (define minimum-upper-bound +inf.0)
         (define best #f)
         <bb-body>
         (printf "visited ~a candidates\n" count)
         (extract best))]

We initialize the client and maintain the best upper bound found so
far and the candidate which had that bound. We store these in the
mutable variables @racket[minimum-upper-bound] and @racket[best]. At
the end of the algorithm, we @racket[extract] the underlying solution
from this best option. (For curiousity's sake, we'll keep a count of
how many options we looked at.)

We update these values whenever we discover something better:

@chunk[<bb-found-better>
       (when (<= candidate-upper minimum-upper-bound)
         (set! minimum-upper-bound candidate-upper)
         (set! best candidate))]

We keep track of the possible candidates, in order of the best lower
bound, using a priority queue.

@chunk[<bb-body>
       (define queue 
         (make-heap 
          (λ (x y)
            (<= (lower-bound x) 
                (lower-bound y)))))
       (heap-add! queue (inject start))
       (for ([candidate (in-heap/consume! queue)])
         (set! count (add1 count))
         <bb-loop-body>)]

The body of the loop simply determines if the candidate should be
pruned because its lower bound is worse than the minimum upper
bound. If the candidate is worth visiting, then we determine if it is
a leaf, i.e. when its upper and lower bounds match. Otherwise, we add
all its children to the queue.

@chunk[<bb-loop-body>
       (define candidate-lower (lower-bound candidate))
       (when (<= candidate-lower minimum-upper-bound)
         (define candidate-upper (upper-bound candidate))
         (cond
           [(= candidate-lower candidate-upper)
            <bb-found-better>]
           [else
            (heap-add-all! queue (split candidate))]))]

When we run our binary tree test, indeed it works. I find that it
averages about 25 candidates, which represents the average depth.

@section{A more interesting problem}

A far more interesting use of branch and bound is to solve something
like the
@link["http://en.wikipedia.org/wiki/Knapsack_problem"]{knapsack
problem}. This problem is when you have a set of items, each with a
value and a weight, and you must select the optimal set of items,
given a weight constraint. In defining this client, we will
parameterize the unit with the weight constraint.

@chunk[<knapsack>
       (struct item (value weight) #:transparent)

       (define (knapsack-bb@ W)
         (unit
          (import) (export bb-client^)
          <knapsack-options>
          <knapsack-split>
          <knapsack-bounds>))]

The key here is to represent options as a set of fixed items, that all
children of the option must use, and a set of free items, that
children may or may not use. Initially, all items are free and at the
end, we only take the fixed items.

@chunk[<knapsack-options>
       (struct option (fixed free) #:transparent)
       
       (define (inject l) (option empty l))
       (define (extract o) (option-fixed o))]

Since our branch and bound finds the minimum, we have to swap the sign
when we compute values, because we want the most value. The lower
bound is the sum of the values of the fixed and free, while the upper
bound is just the fixed, because we have to choose them.

@chunk[<knapsack-bounds>
       (define (upper-bound o) 
         (match-define (option fixed free) o)
         (* -1 (items-value fixed)))
       (define (lower-bound o)
         (match-define (option fixed free) o)
         (* -1 (+ (items-value fixed) (items-value free))))]

We can enforce the constraint on the weight inside of the splitting
function by not producing children that violate the constraint.

@chunk[<knapsack-split>
       (define (split o)
         (match-define (option fixed free) o)
         (match free
           [(list)
            empty]
           [(list-rest fst more)
            (define next-fixed (cons fst fixed))
            (list* 
             (option fixed more)
             (if (> (items-weight next-fixed) W)
               empty
               (list (option next-fixed more))))]))]

We can test this use of branch and bound like this, using a list of 30
items with values from 1 to 10 and weights of either 10 or 5:

@chunk[<knapsack-test>
       (define items
         (append (build-list 10 (λ (i) (item (add1 i) 10)))
                 (build-list 10 (λ (i) (item (add1 i) 5)))
                 (build-list 10 (λ (i) (item (add1 i) 5)))))
       (check-equal? (find (knapsack-bb@ 10) items)
                     (list (item 10 5) (item 10 5)))]

Our branch and bound finds an optimum by only looking at 1,709
candidates out of the 1,073,741,824 (2^30) options.

If you'd like to use this code at home, you should put it in this
order:

@chunk[<*>
       (require rackunit
                racket/unit
                racket/match
                racket/list
                data/heap)
       
       <bb-client-sig>
       <bb>
       
       <binary-bb>       
       <binary-test>

       (define (items-value is)
         (for/sum ([i (in-list is)]) (item-value i)))
       (define (items-weight is)
         (for/sum ([i (in-list is)]) (item-weight i)))
       <knapsack>
       <knapsack-test>]

@(the-end)
