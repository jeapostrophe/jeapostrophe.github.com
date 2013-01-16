#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/list)
          "../post.rkt")

@title{A Boggle Solver}
@categories["Puzzles" "Racket"]

Boggle is a classic word game that lends itself well to algorithmic
attacks. A student of mine wanted to show me his solution (as it is an
assignment in one of BYU early classes), but I have a hard time
evaluating things like this unless I've done them myself. So, I
decided to make an attempt. I was able to do it in about 19 lines of
code, minus the 24 to set up the data-structures. Let's see how it
goes...

@(the-jump)

Let's review the rules of Boggle. You have a square board of
characters. You are trying to find words from a dictionary of valid
words. The standard word list does not have words under three letters,
etc. The word may be constructed from any string of adjacent
characters---including the diagonals---provided it does not use the
same board position twice.

The two most important decision we make in the algorithm are
representing the dictionary and representing the board. Once these are
in place, the result is pretty obvious.

@section{The board}

Let's start with the board. We'll represent it as a hash table mapping
coordinates, like (0,0), to the letter at that coordinate. The program
will generate a random board configuration before solving it.

@chunk[<board>
       (define board-n 4)
       (define board
         (for*/fold ([cell->char (hash)])
             ([row (in-range board-n)]
              [col (in-range board-n)])
           (hash-set cell->char (cons row col) (random-letter))))]

The standard game of Boggle is played on a 4x4 gird, but we'll be
parameterized over @racket[board-n].

The hash table doesn't have any particular order, but that's fine
because we'll be using the coordinates directly. Still, printing out
the board is pretty convenient:

@chunk[<printing>
       (for ([row (in-range board-n)])
         (for ([col (in-range board-n)])
           (display (hash-ref board (cons row col))))
         (newline))]

At this point, we have six essential lines of code. (I don't count the
printer.)

@section{The dictionary}

The more interesting decision comes from how we'll represent the
dictionary. The core idea is to use a regular expression derivative,
where the regular expression is accepting when the string is in the
dictionary. The representation will be tabled and gradually
constructed by adding the words one at a time.

We'll define a dictionary as a hash mapping characters to a boolean
and another dictionary. The boolean will describe if the string is
accepted (i.e. corresponds to a word) and the dictionary will be the
transitions from this string prefix. For example, the dictionary that
only contains "cat" and "cats" is:

@chunk[<dict-example>
       (hasheq #\c
               (cons #f
                     (hasheq #\a
                             (cons #f
                                   (hasheq #\t
                                           (cons #t
                                                 (hasheq #\s
                                                         (cons #t (hasheq)))))))))]

The following provdes the necessary function for extending an empty
dictionary like this:

@chunk[<dict>
       (define empty-dict (hasheq))
       (define empty-entry (cons #f empty-dict))
       (define (dict-add d w)
         (if (empty? w)
           d
           (hash-update d (first w)
                        (match-lambda
                         [(cons word? rest-d)
                          (cons (or word? (empty? (rest w)))
                                (dict-add rest-d (rest w)))])
                        empty-entry)))
       (define (dict-add* d s)
         (dict-add d (string->list s)))]

@racket[dict-add*] breaks a string into a list of characters, which
are read one-by-one extending the dictionary gradually. If the rest of
the list after a given character is ever empty, then the dictionary
entry corresponds to a complete word.

We can test to make sure this function works by comparing to our
manually constructed example:

@chunk[<dict-test>
       (define cat-dict <dict-example>)
       (check-equal? 
        (dict-add* (dict-add* empty-dict "cat") "cats")
        cat-dict)
       (check-equal? 
        (dict-add* (dict-add* empty-dict "cats") "cat")
        cat-dict)]

We can build the whole dictionary from a standard word list, like so:

@chunk[<dict-parse>
       (define dict-pth "/usr/share/dict/words")
       (define the-dictionary
         (for/fold ([d empty-dict])
             ([w (in-lines (open-input-file dict-pth))])
           (dict-add* d w)))]

The standard dictionary is not Boggle-legal, because it contains words
under three letters, apostrophes, proper names, etc. But the algorithm
wouldn't change with a different list.

At this point, we have 18 more lines of essential code, bringing
the total to 24. (I don't count the test.)

@section{The solver}

Now that we have our data-structures ready, it's a pretty straight
path. We'll be exploring the board like a graph, looking for paths
where the nodes correspond to accepting strings. After visiting a
node, we'll remove it from the graph and proceed to all the neighbors,
provided that the dictionary is not empty from the current path.

The main loop simply starts this process from every possible square
with the complete board, the complete dictionary, and an empty path:

@chunk[<main>
       (for ([k (in-hash-keys board)])
         (solutions-from board the-dictionary k empty))]

We make use of a slight pun by iterating through the board's hash
keys, which correspond to the cell coordinates.

(Solution so far: 2 lines)

The @racket[solutions-from] function is a bit more complicated.

Its first task will be to determine if a cell is actually on the
board (i.e. it has not been removed already and was there in the first
place):

@chunk[<solutions-from>
       (define (solutions-from board dict k path)
         (define c (hash-ref board k #f))
         (when c
           <step-one>
           <step-two>
           <step-three>))]

(Solution so far: 5 lines)

If it was there, then we'll want to know if the new path is a
word, what the new state machine is, and what the new path is:

@chunk[<step-one>
       (match-define (cons word? new-dict)
                     (hash-ref dict c empty-entry))
       (define new-path (cons c path))]

(Solution so far: 8 lines)

If the current path is a word, then we can print it out, which is a
bit complicated since we're just storing the path backwards, so we
have to reverse the list (to make it forwards) and then turn the list
of characters into a string:

@chunk[<step-two>
       (when word?
         (displayln (list->string (reverse new-path))))]

(Solution so far: 10 lines)

If it possible to have any more words from this path (i.e. if the new
dictionary isn't empty), then we'll want to remove this node from the
board and vist all adjacent positions:

@chunk[<step-three>
       (unless (zero? (hash-count new-dict))
         (define new-board (hash-remove board k))
         (match-define (cons row col) k)
         (for* ([drow (in-list '(-1 0 1))]
                [dcol (in-list '(-1 0 1))])
           (solutions-from new-board new-dict
                           (cons (+ row drow)
                                 (+ col dcol))
                           new-path)))]

We make some fun abuses for the sake of simplicity. For example, this
will re-visit the current node, but we've already removed it from the
board, so the body of the function will be skipped. Similarly, we
don't care about going off the board, because the same test will find
those. This is by far the largest block of the solution, but it is
still quite simple.

This actually concludes the solution, which is a whopping 19 lines!

@section{The whole program}

The whole program is a mere 43 lines of essential code but is a
complete and efficient Boggle solver.

Regarding the efficiency, it takes about a 56x56 board to take more
than 1 second, but that time is dominated by printing. When I turn off
printing (but not the construction of the string to be printed), it is
under a second until about 110x110.

The major wins are:

1. A good data-structure for the dictionary, corresponding to
top-notch regular expression matching theory.

2. A functional representation of the board, so we can safely remove
nodes from the board without doing any bookkeeping or undo-ing.

3. A snoc-list of the path backwards to maximize sharing. (Exercise:
Change the code so that it doesn't need to do any allocation for
successes. Right now it has to allocate another list and then convert
it into a string. Try to print out a list backwards without
allocation.)

4. Eliminating duplicate work: it never explores any path more than
once, but will discover the same word through multiple paths, if
possible.

Returning to the student, he sent me about 700 lines of complicated
and inefficent C++ code and was working on a new version that was
currently 500 lines and broken. Maybe this will be a good push in the
right direction?

By the way, if you use this code at home, make sure you put the code in this
order:

@chunk[<*>
       (require racket/list
                racket/match
                rackunit)

       (define letters
         (string->list "abcdefghijklmnopqrstuvwxyz"))
       (define (random-list-ref l)
         (list-ref l (random (length l))))
       (define (random-letter)
         (random-list-ref letters))

       <board>
       <printing>
       (newline)

       <dict>
       <dict-test>
       <dict-parse>

       <solutions-from>
       (time <main>)]

@(the-end)
