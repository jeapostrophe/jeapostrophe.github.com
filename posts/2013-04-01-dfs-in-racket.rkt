#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/list
                     racket/sandbox)
          "../post.rkt")

@title{Tree Search in Racket}
@categories["Racket"]

A student came into my office the other day and asked how you would do
a tree search (specifically breadth-first search) in Racket. His
assumption was that it would be difficult or strange in a primarily
functional paradigm. This post recapitulates that experience.

@(the-jump)

I asked the student for the basic idea of breadth-first search. His
answer was, "You have a queue, which you add the root to. You keep
taking things off the queue and if you find the node you are looking
for, you return it. Otherwise, you add its children to the end of the
queue and continue. If the queue is ever empty when you try to remove
from it, return an indication of failure."

This algorithm can be directly translated into Racket code,
parameterized over the notion of a node:

@chunk[<imperative>
       (define (bfs ? root)
         (let/ec return
           (define q (list root))
           (while (not (empty? q))
             (define cur (first q))
             (set! q (rest q))
             (cond
               [(? (node-data cur))
                (return cur)]
               [else
                (set! q (append q (node-children cur)))]))
           #f))]

As an example test, let's represent nodes as @racket[cons] cells,
where the @racket[car] is the data and the @racket[cdr] is a list of
the children. Here's a definition of that with an example tree:

@chunk[<cons-node>
       (define node-data car)
       (define node-children cdr)
       (define example-tree
         '(0
           (1 
            (3
             (7)
             (8))
            (4
             (9)
             (10)))
           (2
            (5
             (11)
             (12))
            (6
             (13)
             (14)))))]

Given this tree, we would expect the following tests to succeed:

@chunk[<example-tests>
       (check-false 
        (bfs (curry = 15) example-tree))
       (check-equal?
        (bfs (curry = 11) example-tree)
        '(11))]

This version of the algorithm, while correct, is a bit unsatisfying
because it is extremely imperative and uses an escape continuation,
which is a bit complicated. A simpler version is:

@chunk[<functional>
       (define (bfs ? root)
         (define (bfs/queue q)
           (cond
             [(empty? q)
              #f]
             [else
              (match-define (cons cur next-q) q)
              (cond
                [(? (node-data cur))
                 cur]
                [else
                 (bfs/queue 
                  (append next-q (node-children cur)))])]))
         (bfs/queue (list root)))]

This purely functional version is nice, because it more neatly
expresses what is going on. Unfortunately, it is inefficient, because
@racket[append] is O(n) in time and space where n is the length of the
queue.

A nicer way to do this is to observe that the queue is private state
that can't be observed by the clients of @racket[bfs], so it is okay
to use an imperative data-structure. But, while doing that, keep the
rest of the code structured as the functional version.

@chunk[<simple-imperative>
       (define (bfs ? root)
         (define q (make-queue))
         (define (bfs/queue)
           (cond
             [(queue-empty? q)
              #f]
             [else
              (define cur (dequeue! q))
              (cond
                [(? (node-data cur))
                 cur]
                [else
                 (for-each (curry enqueue! q) 
                           (node-children cur))
                 (bfs/queue)])]))
         (enqueue! q root)
         (bfs/queue))]

Finally, a more Racket-y way to write this would be to use sequences
and the @racket[for/or] macro, rather than write the loop explicitly.

@chunk[<simpler-imperative>
       (define (bfs ? root)
         (define q (make-queue))
         (enqueue! q root)
         (for/or ([cur (in-queue q)])
           (cond
             [(? (node-data cur))
              cur]
             [else
              (for-each (curry enqueue! q) 
                        (node-children cur))
              #f])))]

One nice thing about this code is that it is parameterized over the
notion of a node. This allows exotic nodes like:

@chunk[<binary-node>
       (define (node-data x) x)
       (define (node-children x)
         (list (+ (* 2 x) 0)
               (+ (* 2 x) 1)))]

and tests like:

@chunk[<binary-tests>
       (check-equal?
        (bfs (curry = 11) 1)
        '11)]

Of course, since this tree is infinite, you shouldn't try searches that fail, like

@chunk[<binary-tests-infinite>
       (check-false
        (bfs (curry = 0) 1))]

Finally, it should be natural for you to adapt this code to use the
@racket[data/heap] library to turn it into a best-first search or a
depth-first search.

@chunk[<*>
       (require racket/list
                rackunit
                racket/function
                racket/match
                data/queue)

       (define-syntax-rule (while cond body ...)
         (let loop ()
           (when cond
             body ...
             (loop))))

       (let ()
         <cons-node>
         (let ()
           <imperative>
           <example-tests>)
         (let ()
           <functional>
           <example-tests>)
         (let ()
           <simple-imperative>
           <example-tests>)
         (let ()
           <simpler-imperative>
           <example-tests>))

       (let ()
         <binary-node>
         (let ()
           <simpler-imperative>
           <binary-tests>))]

@(the-end)
