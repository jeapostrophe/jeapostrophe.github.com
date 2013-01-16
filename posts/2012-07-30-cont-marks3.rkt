#lang scribble/lp
@(require "../post.rkt"
          (for-label racket/base
                     rackunit
                     racket/list))

@title{Continuation Marks, part III: Marks themselves}
@categories["Racket" "Continuations" "Continuation Marks"]

The last two weeks we've seen the basic ideas behind continuation
marks, but we've never actually used them. This week we show the
actual feature.

@(the-jump)

As usual, we'll return to the example from last time:

@chunk[<example>
       (define show-structure
         (match-lambda
          [(list e ...)
           (with-indentation
            (λ () (for-each show-structure e)))]
          [(? number? x)
           (displayln/indent x)]))
       (show-structure
        (list (list 1 2) 3 (list 4 5) 6 (list 7 (list 8) 9)))]

which prints as:

@verbatim{
  1
  2
 3
  4
  5
 6
  7
   8
  9
  }

where we relied on the parameter feature of Racket to implement the
indentation tracking:

@chunk[<params>
       (define indent-level (make-parameter 0))
       (define (with-indentation t)
         (define originally (indent-level))
         (parameterize ([indent-level (add1 originally)])
           (t)))
       (define (displayln/indent x)
         (for ([i (in-range (indent-level))])
           (display " "))
         (displayln x))]

We can read @racket[parameterize] as annotating the context of the
@racket[t] evaluation with information that says "the indentation
level is now 3" (or whatever.) But parameters wrap that annotation in
a convenient interface.

The annotation could be expressed directly by "marking" (annotating)
the "continuation" (context):

@chunk[<marks>
       (define (indent-level)
         (continuation-mark-set-first
          (current-continuation-marks)
          'indent-level
          0))
       (define (with-indentation t)
         (define originally (indent-level))
         (with-continuation-mark
          'indent-level (add1 originally)
          (t)))
       (define (displayln/indent x)
         (for ([i (in-range (indent-level))])
           (display " "))
         (displayln x))]

The only apparent differences are that we use
@racket[with-continuation-mark] rather than @racket[parameterize], use
the symbol @racket['indent-level], and have to write the accessor
function ourselves. Indeed, you can almost imagine that parameters are
little more than these differences macro-ized. (As mentioned before,
there are other differences with parameters and concurrency that this
leaves out.)

One crucial difference between parameters and continuation marks,
however, is that with parameters you can only observe the *last*
value, whereas with marks you can observe *all* values. Our example
only looks at the last one, so we'll have to change something else to
see them all.

Here's a factorial function, with continuation marks annotating its
arguments:

@chunk[<fac>
       (define (fac n)
         (cond
           [(zero? n)
            (displayln
             (continuation-mark-set->list*
              (current-continuation-marks) '(fac)))
            1]
           [else
            (with-continuation-mark
             'fac n
             (* n (fac (sub1 n))))]))
       (fac 3)]

If you run this code, it prints out a list containing 1, 2, and
3---the annotations around the call to @racket[(fac 0)]. This is
similar to the indentation program, except in that case we'd only be
able to observe 1, the last annotation.

Continuation marks like this (noting the name of the function and its
arguments) are the majority of stack traces. Other kinds of marks
could be used for other runtime inspection purposes.

Continuation marks behave specially when the annotation is in tail
position. (Recall that this is why @racket[parameterize] is more
efficient than @racket[dynamic-wind].) Since in tail position there is
not a @emph{new} continuation, there is only space for one mark, so any new
marks destroy old marks. A tail-recursive version of factorial
demonstrates the difference:

@chunk[<fac-tr>
       (define (fac-tr n acc)
         (cond
           [(zero? n)
            (displayln
             (continuation-mark-set->list*
              (current-continuation-marks) '(fac)))
            acc]
           [else
            (with-continuation-mark
             'fac n
             (fac-tr (sub1 n) (* n acc)))]))
       (fac-tr 3 1)]

This program only prints out 1, because the single continuation only
has space for one mark and each recursion annihilates it. 

This property preserve the Safe-for-Space guarantees of Racket in the
presence of continuation marks.

I find continuation marks very useful when a program must observe the
path that led to it. For example, it can be a convenient way to detect
cycles in a search space. Sometimes I will write a "custom" stack
trace like this just for experimenting purposes to track the
evaluation of the program better.

Continuation marks are also the basis of the stateless
continuation-based Web server that comes with Racket, but that's
another story.

By the way, if you use this code at home, make sure you put the code
in this order:

@chunk[<*>
       (require racket/match)

       (let ()
         (printf "Params\n")
         <params>
         <example>)

       (let ()
         (printf "Marks\n")
         <marks>
         <example>)

       <fac>
       <fac-tr>

       (printf "Done\n")]

@(the-end)
