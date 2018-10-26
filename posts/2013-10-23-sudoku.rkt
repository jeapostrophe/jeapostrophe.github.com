#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/list
                     racket/generator
                     racket/file)
          "../post.rkt")

@title{Solving Sudoku in Racket}
@categories["Racket" "Puzzles" "Algorithms"]

The other week I was teaching my aging father-in-law about Sudoku and
I needed to explain to him the rules and what techniques I use when I
solve one. I like to avoid search as much as possible, because I think
it is @postlink["2013-03-19-pap3"]{inhumane}. In this post, I encode
my technique as a Racket program that avoids search as much as
possible.

@(the-jump)

I assume that you know what
@link["http://en.wikipedia.org/wiki/Sudoku"]{Sudoku} is, but if you
don't. You are solving a graph coloring completion problem where the
graph is always the same but the initial coloring varies from game to
game. Here is an example board in ASCII art:

@chunk[<board1>
       (define b1
         (board
          "53 | 7 |   "
          "6  |195|   "
          " 98|   | 6 "
          "-----------"
          "8  | 6 |  3"
          "4  |8 3|  1"
          "7  | 2 |  6"
          "-----------"
          " 6 |   |28 "
          "   |419|  5"
          "   | 8 | 79"))]

This example comes from the Wikipedia page. My first task is
implementing the solver was to turn this into a data-structure. I
decided to represent the board as a homogeneous list of cells, rather
than the actual graph, because I was lazy and wanted to use a
data-structure that was amenable to pure functional programming. Each
cell stores its X and Y position, plus a set of the available choices
for its value. For example, blank cells in the input can be anything,
but cells that can only be one thing are "solved".

@chunk[<data-def>
       (define anything (seteq 1 2 3 4 5 6 7 8 9))

       (struct cell (x y can-be) #:transparent)       
       (define (grid? l)
         (and list? (andmap cell? l)))

       (define (cell-solved? c)
         (= 1 (set-count (cell-can-be c))))]

The parsing of this list of strings into a list of cells is pretty
boring and ugly code. I think this is a common experience: the nicer
the format is for humans, the uglier the parser is. In general, that
seems like a good trade.

@chunk[<parser>
        (define (board . ss)
          (for*/fold ([cells null]
                      #:result (reverse cells))
                     ([str (in-list ss)]
                      [c (in-port read-char (open-input-string str))]
                      #:unless (memv c '(#\- #\|)))
            (define-values (row col) (quotient/remainder (length cells) 9))
            (cons (cell col row (cond
                                  [(string->number (string c)) => seteq]
                                  [else anything])) cells)))]

The only cute thing in the parser is the way I use @racket[flatten]
and @racket[empty] to ignore the vertical rules in the ASCII
art. Normally, my performance sense would prevent me from writing code
like that, but because I know the lists are small and the code is only
run once, it's not a big deal.

In order to help appreciate what the solver does, I wrote some code to
visualize a sequence of grids into an animation. There's nothing
particularly interesting about it, so I will just put it here for you
to skip over to the real interesting stuff of how I do the solving:

@chunk[<visualize>       
       (require 2htdp/image
                2htdp/universe)

       (define (fig s) (text/font s 12 "black" #f 'modern 'normal 'normal #f))
       (define MIN-FIG (fig "1"))
       (define CELL-W (* 3 (image-width MIN-FIG)))
       (define CELL-H (* 3 (image-height MIN-FIG)))

       (struct draw-state (i before after))
       (define (draw-it! gs)
         (define (move-right ds)
           (match-define (draw-state i before after) ds)
           (cond
             [(empty? (rest after))
              ds]
             [else
              (draw-state (add1 i)
                          (cons (first after) before)
                          (rest after))]))
         (define (draw-can-be can-be)
           (define (figi i)
             (if (set-member? can-be i)
               (fig (number->string i))
               (fig " ")))
           (place-image/align
            (if (= 1 (set-count can-be))
              (scale 3 (fig (number->string (set-first can-be))))
              (above (beside (figi 1) (figi 2) (figi 3))
                     (beside (figi 4) (figi 5) (figi 6))
                     (beside (figi 7) (figi 8) (figi 9))))
            0 0
            "left" "top"
            (rectangle CELL-W CELL-H
                       "outline" "black")))
         (define (draw-draw-state ds)
           (match-define (draw-state i before after) ds)
           (define g (first after))
           (for/fold ([i
                       (empty-scene (* CELL-W 11)
                                    (* CELL-H 11))])
               ([c (in-list g)])
             (match-define (cell x y can-be) c)
             (place-image/align
              (draw-can-be can-be)
              (* CELL-W
                 (cond [(<= x 2) (+ x 0)]
                       [(<= x 5) (+ x 1)]
                       [   else  (+ x 2)]))
              (* CELL-H
                 (cond [(<= y 2) (+ y 0)]
                       [(<= y 5) (+ y 1)]
                       [   else  (+ y 2)]))
              "left" "top"
              i)))
         (big-bang (draw-state 0 empty gs)
                   (on-tick move-right 1/8)
                   (on-draw draw-draw-state)))]

The key idea of the solver is to do naive propagation of constraints
until it is impossible to move forward, at which point I devolve into
searching. The core piece of this is a function to iterate a
propagation function until it stops having an impact:

@chunk[<iterate>       
       (define (until-fixed-point f o bad? end-f)
         (define-values (changed? no) (f o))
         (if changed?
           (cons
            no
            (if (bad? no)
              (end-f no)
              (until-fixed-point f no bad? end-f)))
           (end-f o)))]

The function, @racket[_f], will be repeatedly called starting with the
object @racket[_o]. The function is expected to return a boolean to
indicate if there was a "change". If there was, then the iteration
continues. However, it is possible, during search, that propagation
discovers an inconsistency, so the solution becomes "bad" and
propagation should stop. For whatever reason, if the iteration stops,
then a function is called with the last object.

It is easy in Sudoku to describe a bad solution: it's one where a cell
has no options for its value.

@chunk[<bad>       
       (define (failed-solution? g)
         (ormap (λ (c) (= (set-count (cell-can-be c)) 0)) g))]

The function that will be iterated, however, is where the real
interesting stuff happens. My personal technique of solving Sudokus is
to pick a cell and then consider a few different situations. The most
basic is, "What cells interfere with this one and prevent it from
taking certain values?". Which cells are possibility interfering?
From a graph perspective, it is the set of neighbors. In the world of
Sudoku, it is cells that are in the same row, column, or box.

@chunk[<neighbor>
       (define (neighbor-of? l r)
         (or (same-row? l r)
             (same-col? l r)
             (same-box? l r)))
       (define (same-row? l r)
         (= (cell-x l) (cell-x r)))
       (define (same-col? l r)
         (= (cell-y l) (cell-y r)))
       (define (same-box? l r)
         (and (= (floor3 (cell-x l)) (floor3 (cell-x r)))
              (= (floor3 (cell-y l)) (floor3 (cell-y r)))))
       (define (floor3 x)
         (floor (/ x 3)))]


I actually find it easier to think about this from the dual
perspective though: I look at a solved cell and remove possibilities
from everything that it interferes with. I call the solved cell
the "pivot". So, the next part of the solving algorithm looks for such
pivots.

@chunk[<pivot>
       (define (find-pivot f l)
         (let loop ([tried empty]
                    [to-try l])
           (match to-try
             [(list)
              (values #f l)]
             [(list-rest top more)
              (define-values (changed? ntop nmore)
                (f top (append tried more)))
              (if changed?
                (values #t (cons ntop nmore))
                (loop (cons top tried) more))])))]

This function takes a function that tries out a pivot and receives a
boolean indicating if a change happened, a new pivot value and a new
list of objects. The idea here is that it goes through each cell
looking for one where the propagation function can do something. What
does this propagation function actually do? It's now possible to talk
about it.

The code has a pretty nice structure. We use an escape continuation to
get an early return from one of three different techniques. If every
technique fails, then we return the pivot and the rest of the cells
unmodified with an indication there was no change.

@chunk[<propagate>
       (define (propagate-one top cs)
         (let/ec return
           <technique1>
           <technique2>
           <technique3>
           (values #f
                   top
                   cs)))]

The first technique is the one mentioned before. We only use it if the
cell is solved, and if it is, we go through each of the cell's
neighbors and remove the solved cell's values from their
possibilities. If any set gets smaller, then we observed a change.

@chunk[<technique1>
       (when (cell-solved? top)
         (define-values (changed? ncs)
           (for/fold ([changed? #f] [ncs empty])
               ([c (in-list cs)])
             (cond
               [(neighbor-of? top c)
                (define before
                  (cell-can-be c))
                (define after
                  (set-subtract before (cell-can-be top)))
                (if (= (set-count before)
                       (set-count after))
                  (values changed?
                          (cons c ncs))
                  (values #t
                          (cons (struct-copy cell c
                                             [can-be after])
                                ncs)))]
               [else
                (values changed? (cons c ncs))])))
         (return changed? top ncs))]

If you only use this technique, it works surprisingly well, but it
fails on the example I showed before, @racket[<board1>]. So, I had to
deploy my next technique.

This technique looks a single set of neighbors (row, column, or box),
which I inappropriately call a clique, and sees if the given cell is
the only one that can take on a certain value. For instance, suppose a
cell could be 3 and 4, but no other cell in its row could be 3, then
the cell must be 3. In code, this means that if you take the cell's
possibilities and remove all the possibilities of everything else in
one dimension of neighborness, then the set of possibilities is unit
sized. This is easy to express in code:

@chunk[<technique2>
       (define (try-clique same-x?)
         (define before (cell-can-be top))
         (define after
           (for/fold ([before before])
               ([c (in-list cs)])
             (if (same-x? top c)
               (set-subtract before (cell-can-be c))
               before)))
         (when (= (set-count after) 1)
           (return #t
                   (struct-copy cell top
                                [can-be after])
                   cs)))

       (try-clique same-row?)
       (try-clique same-col?)
       (try-clique same-box?)]

This is all you need to solve the example from before and most Sudokus
that @link["http://www.websudoku.com/"]{WebSudoku} categorizes as Easy
and Medium. However, if we look at a Hard example, like the one below,
then it will fail.

@chunk[<board2>       
       (define b2
         (board
          " 7 | 2 |  5"
          "  9| 87|  3"
          " 6 |   | 4 "
          "-----------"
          "   | 6 | 17"
          "9 4|   |8 6"
          "71 | 5 |   "
          "-----------"
          " 9 |   | 8 "
          "5  |21 |4  "
          "4  | 9 | 6 "))]

The next technique that I employ is when two cells in a clique both
are limited to the same two possibilities, such as both being
constrained to be 3 or 4. In this case, you can remove 3 and 4 from
all other cell possibility sets in the clique. This is a bit more
awkward to program, because there are two iterations of all the
cells. You could do it in one, but I think it be more obtuse than is
worth it.

@chunk[<technique3>
       (define (only2-clique same-x?)
         (define before (cell-can-be top))
         (when (= (set-count before) 2)
           (define other
             (for/or ([c (in-list cs)])
               (and (same-x? top c) (equal? before (cell-can-be c)) c)))
           (when other
             (define changed? #f)
             (define ncs
               (for/list ([c (in-list cs)])
                 (cond
                   [(and (not (eq? other c)) (same-x? top c))
                    (define cbefore
                      (cell-can-be c))
                    (define cafter
                      (set-subtract cbefore before))
                    (unless (equal? cbefore cafter)
                      (set! changed? #t))
                    (struct-copy cell c
                                 [can-be cafter])]
                   [else
                    c])))
             (return changed? top
                     ncs))))

       (only2-clique same-row?)
       (only2-clique same-col?)
       (only2-clique same-box?)]

Once this technique is in place, the aforementioned puzzle is
solved. However, when I went to "Evil" puzzles, like the following
one, I needed to use search.

@chunk[<board3>
       (define b3
         (board
          "  8|   | 45"
          "   | 8 |9  "
          "  2|4  |   "
          "-----------"
          "5  |  1|76 "
          " 1 | 7 | 8 "
          " 79|5  |  1"
          "-----------"
          "   |  7|4  "
          "  7| 6 |   "
          "65 |   |3  "))]

The main impact of using searching is that we have to interleave the
previous constraint propagation steps with arbitrarily picking the
values of certain cells. I use a backtracking technique, where if
after a fixed point is reached on the constraint solving, there are
three possibilities. In one case, the puzzle is solved so we're done
and the rest of the trace is @racket[empty]. In the next, the solution
is bad and we need to backtrack. In the last case, we need to run
search.

@chunk[<search>
       (define (solved? g)
         (andmap (λ (c) (= (set-count (cell-can-be c)) 1)) g))
       (define (solve-it g)
         (let solve-loop
             ([g g]
              [backtrack!
               (λ (i)
                 (error 'solve-it "Failed!"))])
           (define (done? g)
             (cond
               [(solved? g)
                empty]
               [(failed-solution? g)
                (backtrack! #f)]
               [else
                (search g)]))
           <search-core>
           (until-fixed-point propagate g failed-solution? done?)))]

The search function that actually manages search will pick a cell and
set it to solved and see if the rest of the puzzle can be solved using
that choice. I think it is probably best to try cells that have fewer
options, so first I sort them by their number of possibilities. Then,
we try to find non-solved cell and set them to a particular answer and
re-run the solver loop. If that stage of solving fails, then we
backtrack, using an escape continuation, to the choice point and make
another. If a given cell runs out of options, then we try a different
cell, and if all cells fail, then we backtrack even earlier.

@chunk[<search-core>
       (define (search g)
         (define sg (sort g < #:key (λ (c) (set-count (cell-can-be c)))))
         (let iter-loop ([before empty]
                         [after sg])
           (cond
             [(empty? after)
              (backtrack! #f)]
             [else
              (define c (first after))
              (define cb (cell-can-be c))
              (or (and (not (= (set-count cb) 1))
                       (for/or ([o (in-set cb)])
                         (let/ec new-backtrack!
                           (define nc
                             (struct-copy cell c
                                          [can-be (seteq o)]))
                           (solve-loop
                            (cons
                             nc
                             (append before (rest after)))
                            new-backtrack!))))
                  (iter-loop (cons c before)
                             (rest after)))])))]

I think it is delightfully simple to do this backtracking because I
was sure to use purely functional data-structures everywhere else in
the solver. If I didn't, then I would have to careful undo every
change after the arbitrary choice, which would be complicated.

After I added this step, every puzzle I've given it is solved fairly
quickly. I know that the NP-Completeness proof for generalized Sudoku
is out there, so in the worst case this is the only stage of the
algorithm that is really useful, but nevertheless, implementing the
other kinds of reasoning for the solver was a lot of fun!

@section{Yo! It's almost time to go!}

But first let's remember what we learned today!

Parsing ASCII art is ugly.

Backtracking and purely functional data-structures are a match made in
heaven.

Certain kinds of backtracking just needs escape continuations, which
are cheaper than full continuations.

When @racket[_n] is @racket[81], all exponentials are constant.

Finally, Sudoku is a boring game when you have to search.

If you'd like to run this exact code at home, you should put it in
this order:

@chunk[<*>
       (require racket/match
                racket/list
                racket/set)

       <data-def>            
       <neighbor>
       <parser>
       <propagate>
       <pivot>

       (define (propagate g)
         (find-pivot propagate-one g))

       <iterate>

       <bad>
       <search>

       <visualize>

       <board1>
       <board2>
       <board3>

       (draw-state-i
        (draw-it!
         (solve-it
          b3)))]

Or just download the
@link["https://github.com/jeapostrophe/exp/blob/master/sudoku.rkt"]{raw
version}.

@(the-end)
