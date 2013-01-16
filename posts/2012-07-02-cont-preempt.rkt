#lang scribble/lp
@(require (for-label (except-in racket/base
                                thread
                                exit
                                printf)
                     (prefix-in racket: racket/base)
                     rackunit
                     racket/list)
          "../post.rkt")

@title{Preemptive Threads with Continuations}
@categories["Racket" "Continuations" "Threads" "Concurrency"]

Last week, we covered cooperative threading with continuations. This
week we'll change the infrastructure to mimic preemption.

@(the-jump)

Before we start, make sure you've read last week's post.

We'll be working from almost the same example program:

@chunk[<example>
       (define (main)
         (define N 5)
         (thread
          (λ ()
            (for ([i (in-range (+ N 2))])
              (printf "iter: ~a\n" i) )))
         (thread
          (λ ()
            (for/fold ([sum 0]) 
                ([i (in-range N)])
              (printf "adder: ~a\n" (+ i sum))
              (+ i sum)))))]

The only difference is that I've removed the calls to @racket[yield]
after the calls to @racket[printf].

Recall that this program has the following output:

@verbatim{
adder: 0
iter: 0
adder: 1
iter: 1
adder: 3
iter: 2
adder: 6
iter: 3
adder: 10
iter: 4
iter: 5
iter: 6
}

We'll also be using the same basic threading system:

@chunk[<threading-system>
       (define ts empty)
       (define (yield)
         (match ts
           [(list)
            (void)]
           [(cons next rest)
            (let/cc k
              (set! ts (snoc rest k))
              (next))]))

       (define (thread t)
         (set! ts 
               (cons (λ () 
                       (abort-current-continuation
                        (default-continuation-prompt-tag)
                        (λ ()
                          (t)
                          (exit))))
                     ts)))
       (define (exit)
         (match ts
           [(list)
            (void)]
           [(cons next rest)
            (set! ts rest)
            (next)]))
       
       (call-with-continuation-prompt
        (λ ()
          (main)
          (exit)))]

Now, this system is about modeling concurrency through threading, so
there is no actual real concurrency in the system. In contrast, in a
real operating system there is true concurrency because the computer
interacts with concurrently-running pieces of hardware... such as disk
devices, the network card, or an alarm device. When this concurrent
devices message the kernel, through interrupts, it can take control
from the user programs and potentially choose a different user program
to re-use, without the permission of the user process.

This option is not available at the user level, particularly if you do
not assume the pre-existence of a lower-level threading system. Since
I'm a good academic, whenever you face a problem that seems
unsolvable, the one sure path is to redefine success. Thus, we'll
focus on the "without permission" part of preemptive
concurrency---removing the need of threaded programs from calling
@racket[yield], but having it called for them, without their
permission, periodically.

The simplest way to realize this is to choose a set of "primitive"
functions provided by the OS and have them call @racket[yield] on
behalf of the process. For example, @racket[printf] is a naturally
choice.

@chunk[<primitive-printf>
       (define (printf . args)
         (begin0 (apply racket:printf args)
                 (yield)))]

When we take this approach, we need to ensure that the process has no
other way of getting to these primitives. This is not a very hard
thing to do if we're implementing a language tower, like Racket, but
is more difficult if we are simply writing a library atop Racket.

This approach has a fundamental trade-off with regards to fairness:
the fewer primitive functions, the more likely it is that the other
processes will starve, as the current thread may not ever call a
primitive, such as @racket[printf]. A typical solution to this is to
call @racket[yield] on every function return, including
tail-calls (i.e. loops), because every program must do this very
often.

However, if primitive functions always call @racket[yield] and there
are many such primitive functions, then we're likely to have too many
context switches. In that case, it's wise to use some sort of "fuel"
counter that indicates how many function calls are allowed before
switching. We can realize this in the @racket[printf] code:

@chunk[<fuel-printf>
       (define INITIAL-FUEL 2)
       (define FUEL INITIAL-FUEL)
       (define (printf . args)
         (begin0 (apply racket:printf args)
                 (set! FUEL (sub1 FUEL))
                 (when (zero? FUEL)
                   (set! FUEL INITIAL-FUEL)
                   (yield))))]

In this example I used a fuel of two, which renders the following
output:

@verbatim{
adder: 0
adder: 1
iter: 0
iter: 1
adder: 3
adder: 6
iter: 2
iter: 3
adder: 10
iter: 4
iter: 5
iter: 6
}

A nice side-effect of a fuel system like this is that you can give
different processes different amounts of fuel to represent priorities.

This is actually how the Racket threading system is implemented,
although all the continuation capturing, context switching, primitive
functions, and fuel manipulation is done in the C virtual machine. You
can see the macros that manage fuel around line 1581 of
include/scheme.h in the Racket VM source.

Next week, we'll look at system calls in this infrastructure.

By the way, if you use this code at home, make sure you put the code
in this order:

@chunk[<*>
       (require racket/list
                racket/match
                (prefix-in racket: racket/base))

       (define (snoc l x)
         (append l (list x)))

       <fuel-printf>
       <example>
       <threading-system>]

@(the-end)
