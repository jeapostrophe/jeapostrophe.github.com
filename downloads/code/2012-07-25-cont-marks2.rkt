#lang scribble/lp
@(require (planet ryanc/scriblogify/scribble-util)
          (for-label racket/base
                     rackunit
                     racket/list))
@literal{
---
layout: post
title: "Continuation Marks, part II: Parameters"
comments: true
categories:
- Racket
- Continuations
- Continuation Marks
---
}

Last week I started discussing continuation marks with a medium-sized
diversion into @racket[dynamic-wind] as a way to delimited the effects
of a mutation to the body of a dynamic context so a program's context
can affect its behavior indirectly.

The pattern we used in the last post is so common that a slightly
improved form of it is integrated into Racket as the concept of
parameters.

@(the-jump)

As a reminder, suppose we have this program:

@chunk[<show-structure>
       (define show-structure
         (match-lambda
          [(list e ...)
           (with-indentation
            (λ () (for-each show-structure e)))]
          [(? number? x)
           (displayln/indent x)]))]

And with this example

@chunk[<example1>
       (show-structure 
        (list (list 1 2) 3 (list 4 5) 6 (list 7 (list 8) 9)))]

We want it to print as:

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

Last week, we implemented this with

@chunk[<mutation-control-dw>
       (define indent-level 0)
       (define (with-indentation t)
         (define originally indent-level)
         (dynamic-wind
             (λ ()
               (set! indent-level (add1 originally)))
             (λ () 
               (t))
             (λ ()
               (set! indent-level originally))))
       (define (displayln/indent x)
         (for ([i (in-range indent-level)])
           (display " "))
         (displayln x))]

This code is practically equivalent to the Racket feature of a
parameter.

@chunk[<mutation-control-params>
       (define indent-level (make-parameter 0))
       (define (with-indentation t)
         (define originally (indent-level))
         (parameterize ([indent-level (add1 originally)])
           (t)))
       (define (displayln/indent x)
         (for ([i (in-range (indent-level))])
           (display " "))
         (displayln x))]

Parameters would like this. The @racket[make-parameter] function takes
the initial value of a parameter and returns a function that returns
the "current" value of the parameter in whatever dynamic context it is
inside. The @racket[parameterize] form wraps its context with a new
value for the the parameter. The wrapping takes place in a way safe
with respect to control, just like dynamic wind.

(Parameters also have special behavior with respect to threads: when
you start a thread, it inherits its parent context's values for all
parameters, whereas the mutation behind @racket[dynamic-wind] would
completely ignore the fact that a new thread was around... the
mutations in the finally part would happen even if the thread was
still live when the rest of the @racket[dynamic-wind] body completed.)

(Parameters also provide the ability to capture a @tech{parameterization}
which captures the current values of *all* parameters. Such a value
can later be installed to restore a captured context. The Racket Web
server uses this to restore parameters for Web programs after user
interactions.)

Parameters and @racket[dynamic-wind] with mutation have slightly
different performance strengths and weaknesses. Namely, parameters are
better for space, whereas @racket[dynamic-wind] is better for speed.

We'll first show the space difference by increasing the indentation
one hundred times and then measuring how much memory was used to store
the changes (and the rests to the changes.)

@chunk[<memory-test>
       (define before (current-memory-use*))
       (let loop ([i 100])
         (cond
           [(zero? i)
            (define after (current-memory-use*))
            (printf "~a MBs\n"
                    (real->decimal-string (/ (- after before) 1024)))]
           [else
            (with-indentation
             (λ () (loop (sub1 i))))]))]

In this program, parameters use about 2.30 MBs, whereas
@racket[dynamic-wind] uses 6.61 MBs, on average. The reason is that
@racket[parameterize] is safe for space---it does not leave residue on
the stack when called in tail-position, as it is here. In contrast,
the @racket[dynamic-wind] finally handlers are all called to unwind
the state change, as so they must be recorded on the stack.

This reveals, and the threading issue, reveal that
@racket[dynamic-wind] doesn't @emph{really} make a scoped variable
change, it actually makes a global variable and undoes it properly. In
contrast, @racket[parameterize] really is a local change and when the
context that change is active for is gone, such as when another
@racket[parameterize] overwrites and the call was in tail-position,
it's no longer necessary to "remember" the intermediate value.

Next, let's see the difference in speed by checking the current
indentation level one hundred thousand times.

@chunk[<speed-test>
       (define before (current-inexact-milliseconds))
       (let loop ([il 0] [i 100000])
         (cond
           [(zero? i)
            (define after (current-inexact-milliseconds))
            (printf "~a in ~a ms\n"
                    il
                    (real->decimal-string (- after before)))]
           [else
            (loop (+ il (get-indent-level)) (sub1 i))]))]

In this program, @racket[dynamic-wind] uses just 0.98 milliseconds,
whereas @racket[parameterize] uses 16.35 milliseconds: a major
difference in speed. That's because the mutation-based version just
embeds a reference to a global variable and can easily look it up,
whereas the parameter-based version has to consult the context.

In general, the parameter-based version is much better for typical use
and it's easier to use. But, sometimes when you have tight loops that
would consult a parameter, it is important to cache the value. This is
most relevant for code that does input or output in a tight loop,
because those functions consult parameters for the default
input/output ports.

Continuation marks are a lower-level feature than parameters, but
understanding parameters is really useful for understanding
marks. Next time, we'll finally see what marks are all about.

By the way, if you use this code at home, make sure you put the code
in this order:

@chunk[<*>
       (require racket/match)

       (define (current-memory-use*)
         (collect-garbage)
         (collect-garbage)          
         (current-memory-use))

       (let ()
         (printf "Dynamic Wind\n")
         <mutation-control-dw>
         <show-structure>
         <example1>
         (let () <memory-test>)
         (define (get-indent-level) indent-level)
         (let () <speed-test>))

       (collect-garbage) (collect-garbage)

       (let ()
         (printf "Parameters\n")
         <mutation-control-params>
         <show-structure>
         <example1>
         (let () <memory-test>)
         (define get-indent-level indent-level)
         (let () <speed-test>))

       (printf "Done\n")]
