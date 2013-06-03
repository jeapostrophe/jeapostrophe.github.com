#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/unit
                     racket/match
                     racket/list
                     data/heap)
          "../post.rkt")

@title{Towers of Hanoi in Racket}
@categories["Racket" "Graphics" "Algorithms"]

I was explaining Towers of Hanoi to a young person at church the other
day and decided to make a short version in Racket with a GUI to
display the movements.

@(the-jump)

@section{Towers of Hanoi}

The @link["http://en.wikipedia.org/wiki/Towers_of_hanoi"]{Towers of
Hanoi} is a classic physical puzzle. The objective is to move a stack
of discs from one pole to another using a third pole as an
intermediate stack. You can only move one disc at a time, you can only
take the top disc, and you can't put a large disc on a small disc.

It is a classic recursive problem, as well, because to move three
discs from A to C, you can first move two discs from A to B then from
B to C after moving the biggest disc.

Here's a solution in Racket that returns a lazy stream of movements:

@chunk[<towers>
       (struct move (from to))

       (define (towers n from to extra)
         (if (= n 0)
           empty-stream
           (stream-append
            (towers (sub1 n) from extra to)
            (stream-cons (move from to)
                         (towers (sub1 n) extra to from)))))]

It's important, using @racketmodname[racket/stream], to use the
@racket[stream-cons] because if you wrote a @racket[stream-append]
with three arguments, then the second recursive call wouldn't be
lazily evaluated and you'd save nothing by using streams over
lists. That's because @racket[stream-append] is a function, while
@racket[stream-cons] is a macro that thunks its arguments.

@section{Visualization}

The program is interesting enough, but the more fun thing to do is
display it graphically. I decided to use
@racketmodname[2htdp/universe], because it's so simple.

When using @racketmodname[2htdp/universe], you need to define a
structure to represent the state of your simulation, called the
world. I used a list of lists of disc diameters, a list of moves to go
forward in the puzzle, and a list of moves to go backwards. 

I defined a function called @racket[show] that shows the solution for
any given n. It uses @racket[towers] to lazily generate all the
forward moves and a few helper functions to display and browse the
state.

@chunk[<show>
       (struct world (stacks forward backward))

       (define (show n)
         (define ms (towers n 0 2 1))
         (define s
           (list (build-list n add1)
                 empty
                 empty))
         (define (draw-world w)
           (draw-stacks n (world-stacks w)))
         (define (move-disc w k)
           (cond
             [(equal? k "left")
              (move-disc/backward w)]
             [(equal? k "right")
              (move-disc/forward w)]
             [else
              w]))
         (big-bang (world s ms empty)
                   (on-key move-disc)
                   (to-draw draw-world)))]

Since we do things lazily, you can very easily view huge problems,
like @racket[(show 40)].

@section{Displaying}

The display of a bunch of stacks is very simple. A disc is just a
rectangle. A stack is a bunch of discs above one another. A set of
stacks is a bunch of stacks next to one another. The only weird thing
is creating the blank space for the portion of the stack that is
empty. This is necessary, because otherwise the alignment on the
picture doesn't look nice.

@chunk[<display>
       (define disc-width 10)
       (define disc-height 10)
       (define (draw-disc d)
         (rectangle (* d disc-width) disc-height 'solid "black"))

       (define (draw-stack s)
         (apply above
                empty-image empty-image 
                (map draw-disc s)))

       (define (draw-stacks n ss)
         (apply
          beside/align
          'top
          (map
           (λ (s)
             (define m (- n (length s)))
             (above (rectangle (* n disc-width)
                               (* m disc-height)
                               'solid "white")
                    (draw-stack s)))
           ss)))]

@section{Browsing}

When the user press the left and right key, we must act on the next
move and move it to the other list so we can go back. There's nothing
spectacular about the code, but it's nice in its simplicity.

@chunk[<browse>     
       (define (move-disc/forward w)
         (match-define (world ss fs bs) w)
         (cond
           [(stream-empty? fs)
            w]
           [else
            (match-define (and m (move from to)) (stream-first fs))
            (world (list-move ss from to) (stream-rest fs) (cons m bs))]))

       (define (move-disc/backward w)
         (match-define (world ss fs bs) w)
         (cond
           [(empty? bs)
            w]
           [else
            (match-define (and m (move to from)) (first bs))
            (world (list-move ss from to) (stream-cons m fs) (rest bs))]))]

I think it is a bit ugly that the two programs are so close but not
clearly abstractable. I tried to write a macro to avoid this:

@chunk[<browse/macro>     
       (define-syntax-rule 
         (define-move-disc move-disc/forward 
           fs bs 
           cond:e
           m from to mfrom mto
           first:e
           nfs nbs)
         (define (move-disc/forward w)
           (match-define (world ss fs bs) w)
           (cond
             [cond:e
              w]
             [else
              (match-define (and m (move from to)) first:e)
              (world (list-move ss mfrom mto) nfs nbs)])))

       (define-move-disc move-disc/forward
         fs bs 
         (stream-empty? fs)
         m from to from to
         (stream-first fs)
         (stream-rest fs)
         (cons m bs))
       (define-move-disc move-disc/backward
         fs bs 
         (empty? bs)
         m to from from to
         (first bs)
         (stream-cons m fs)
         (rest bs))]

But this is an example of a really bad macro, it doesn't really
represent an abstraction that is meaningful, and instead inherently
relies on the user understanding exactly what the expansion will
be. So, I don't think you should use it.

Finally, to actually move the disc from one stack to another, I came
up with a cute little thing to make sure the list of stacks stayed in
the same order:

@chunk[<move>
       (define (list-move ss from to)
         (define from-disc (first (list-ref ss from)))
         (for/list ([s (in-list ss)]
                    [i (in-naturals)])
           (cond
             [(= i from)
              (rest s)]
             [(= i to)
              (list* from-disc s)]
             [else
              s])))]

@section{Yo! It's almost time to go!}

But first let's remember what we did today!

We learned that simple recursive algorithms are short and elegant in
Racket and you can easily use laziness where appropriate.

We saw that using @racketmodname[2htdp/image] to render things algebraically is
simple and great for little visualizations like this.

Finally, we saw that @racketmodname[2htdp/universe] lets us create an
elegant little GUI in just 18 lines.

There wasn't anything especially deep in this week's post, but I think
it is useful to remember that Racket doesn't just blow your mind with
crazy stuff you've never thought of. It @emph{also} blows your mind
with cute little elegant versions of classic ideas.

If you'd like to run this exact code at home, you should put it in
this order:

@chunk[<*>
       (require racket/stream
                racket/list
                racket/match
                2htdp/image
                2htdp/universe)

       <towers>
       <display>
       <move>
       (let () <browse> (void))
       <browse/macro>
       <show>

       (show 40)]

@(the-end)
