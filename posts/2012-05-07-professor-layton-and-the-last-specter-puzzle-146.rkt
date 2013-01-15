#lang scribble/lp
@(require "../post.rkt"
          (for-label racket/base
                     rackunit
                     racket/list))

@title{Professor Layton and the Last Specter, Puzzle #146}
@categories["Puzzles" "Racket"]

This is for @link["http://tinyurl.com/7yefk76"]{Puzzle 146}. Here is
the puzzle:

> A bookcase has seven shelves each filled with 10 books of the same
size.

> When you choose a book, you must also take the two books above,
below, to the left, and to the right of the one you actually want. If
there are fewer than two books in any of these directions, you cannot
choose that book.

> Choosing as many books as possible, how many books will you end up
taking?

@(the-jump)

I encoded the bookcase as a hash table in Racket, where the key is the
coordinate of the book and the value is whether the book has been
taken.

@chunk[<bookcase>
       (define bookcase
         (for*/hash ([shelf (in-range 7)]
                     [book (in-range 10)])
                    (values (cons shelf book) #t)))]

I didn't know when I wrote it, but I assumed that it would not matter
what order you picked the books in, so I just considered taking them
from left to right, and top to bottom. After consider each book, I
would return the new bookcase, after having removed some books. The
number of books in this bookcase, subtracted from 70, would give me the
number selected:

@chunk[<selecting>
       (define final-bookcase
         (for*/fold ([bookcase bookcase])
             ([shelf (in-range 7)]
              [book (in-range 10)])
           <loop-body>))
       (- 70 (hash-count final-bookcase))]

Inside the loop, it will be convenient to bind an escape continuation
to return early. The actual loop body is divided into three
parts. First, we'll make sure that the book we're consider is actually
there. Then, we'll make sure it has two books in every
directions. Finally, we'll return the updated bookcase. The code will
look like this:

@chunk[<loop-body>
       (let/ec return
         <check-this-book>
         <check-others>
         <update-bookcase>)]

The first part is really simple: just call our predicate and if it
isn't there, call the escape continuation to jump past the rest of the
code and leave the bookcase unchanged.

@chunk[<check-this-book>
       (unless (hash-has-key? bookcase (cons shelf book))
         (return bookcase))]

The middle part is the most complicated. Here's the idea: we'll loop
over the eight other different books (the two above, below, to the
left, and to the right) and remove them from the bookcase, if they are
there. If any book isn't there, we'll return from the outer loop with
the original bookcase, because the conditions aren't met, using the
escape continuation.

@chunk[<check-others>
       (define other-book-offsets
         (list (cons 1 0) (cons 2 0)
               (cons -1 0) (cons -2 0)
               (cons 0 1) (cons 0 2)
               (cons 0 -1) (cons 0 -2)))
       (define new-bookcase
         (for*/fold ([new-bookcase bookcase])
             ([diff (in-list other-book-offsets)])
           (define new-book
             (cons (+ shelf (car diff))
                   (+ book (cdr diff))))
           (if (hash-has-key? new-bookcase new-book)
             (hash-remove new-bookcase new-book)
             (return bookcase))))]

If the earlier two tests haven't returned, then when we get to the
third, we'll know all the right conditions are met, so we can remove
the current book from the bookcase (the one after the other books were
removed):

@chunk[<update-bookcase>
       (hash-remove new-bookcase (cons shelf book))]

This technique makes inherent use of functional data-structures,
because the bookcase is not destructively modified during the trial
deletions of the eight books. If it were, then we couldn't just return
the original bookcase. Instead, we'd have to undo the changes, or do
the inner loop twice: once to check if we should and once to actually
remove them. Either way, we'd be doing about twice as much work.

This program also demonstrates how useful early return, escape
continuations can be. If we didn't have them, then in the first case,
we'd just have to change the syntactic structure of the program, by
putting the rest of the loop body on the false side of @racket[if]. In
the second case, however, it would be more complicated, because we'd
have to continue the loop past the point of usefulness and check if it
was successful at the end---maybe seeing how many books were removed
or by keeping a boolean on the side. Either way, escape continuations
saved the day.

In retrospect, I think a good way to solve this would be to think of
it has a packing problem where you have a plus sign where the legs are
each two units and you are trying to fit as many as possible on a 7x10
grid.

Was this faster or slower than doing it the old fashion way...? Who
knows.

Can you work out what the answer is...?

By the way, if you use this code at home, make sure you put the code in this
order:

@chunk[<*>
       <bookcase>
       <selecting>]
