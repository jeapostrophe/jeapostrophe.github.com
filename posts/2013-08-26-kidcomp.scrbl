#lang scribble/manual
@(require (for-label racket/base
                     rackunit
                     racket/bool
                     racket/list)
          "../post.rkt")

@title{Racket Helps Kids Use a Mouse and Keyboard}
@categories["Racket" "Family" "Education"]

My kids (5 and 3) have a hard time doing basic things with a
computer. I think the big reason is that they almost never have any
reason to do anything, but in the rare circumstances it would be
useful for them to do so, they aren't very comfortable with the input
devices.

A lot of programs and sites that I can find online for this sort of
thing are very complicated and do a lot more than necessary. So, I
decided to solve this problem with a little Racket program.

@(the-jump)

The software,
@link["https://github.com/jeapostrophe/kidcomp"]{kidcomp}, is a
200-line GUI program that gives the child a sequence of "tasks":
either, they have to type a key or click on a circle with the
mouse. (In the future, I plan on adding different types of clicks and
dragging, but didn't feel like finishing the implementation for this
post.)

The goal of the program is very simple and the "algorithm" it needs to
implement is quite simple too, but what I love about it is how simple
the code turned out.

You can tell almost everything that the program does from its data
structures:
@racketblock[
(struct state:start (key-n mouse-n))
(struct state:key (time typed to-type k))
(struct state:mouse (time done to-do k))
(struct state:report (key mouse))
(struct state:end ())
]

The program has four states:
@itemlist[

@item{At the beginning, it stores how many tasks of each type to do.}

@item{First it does the key tasks while storing: how long the current
task has been active, all the keys already pressed (mistakes and
correct presses), the list of keys remaining to be typed, and a
continuation representing the details of the next state.}

@item{After the keys are all done, the continuation is used to create
the mouse task state where we store the time as before, along with the
tasks done and to do, plus a continuation.}

@item{After both kinds of tasks are finished, it displays a report
about the activities and then ends.}

]

The continuations in the middle two states are respectively the number
of mouse tasks to generate and the report on the key tasks.

The reports are just lists of "result" structures:

@racketblock[
(struct result:mouse (task time))
(struct result:key (actual expected time))
]

The key tasks are just the keys and the mouse tasks are tagged
circles, where the tag is the kind of click expected on the circle:

@racketblock[
(struct mouse-task:click (kind x y r))
]

Everything about the program is so obvious.  For instance, here's the
keyboard input handler:

@racketblock[
(define (state-key s ke)
  (match s
    [(state:start key-n mouse-n)
     (state:key-start key-n mouse-n)]
    [(state:key then typed to-type k)
     (cond
       [(valid-key? ke)
        =>
        (λ (c)
          (define now (current-inexact-milliseconds))
          (define time (- now then))
          (define new-typed
            (cons (result:key c (first to-type) time) typed))
          (if (char=? c (first to-type))
            (state:key* now new-typed (rest to-type) k)
            (state:key* then new-typed to-type k)))]
       [else
        s])]
    [(state:report _ _)
     (state:end)]
    [s
     s]))
]

The only interesting things here are:

@itemlist[

@item{@racket[state:key-start] is a nice constructor for
@racket[state:key] states that generates the tasks from a random list
of characters.}

@item{@racket[valid-key?] returns the character of a key press that
isn't a special key like @onscreen{Esc}.}

@item{@racket[state:key*] constructs a @racket[state:mouse] if the
@racket[to-type] list is @racket[empty].}

]

My favourite thing about programming is being able to create little
elegant programs to smooth over the little challenges in my
life. Racket and its beautiful interfaces help me make sure the
programming doesn't become its own problem.

@section{Yo! It's almost time to go!}

But first let's remember what we did today!

Programmers have super-powers when compared to mere mortals who can't
use their computer to do things it didn't start off able to do.

Racket boosts your super-powers further with its elegance and beauty.

@(the-end)
