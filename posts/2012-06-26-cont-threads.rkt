#lang scribble/lp
@(require (for-label (except-in racket/base
                                thread
                                exit
                                printf)
                     (prefix-in racket: racket/base)
                     rackunit
                     racket/list))
@literal{
---
layout: post
title: "Cooperative Threads with Continuations"
comments: true
categories:
- Racket
- Continuations
- Threads
- Concurrency
---
}

After the last post about continuations, I wanted to show one of the
classic uses of them: implementing threads in user-space.

@(the-jump)

Let's look at an example program first:

@chunk[<example>
       (define (main)
         (define N 5)
         (thread
          (λ ()
            (for ([i (in-range (+ N 2))])
              (printf "iter: ~a\n" i) 
              (yield))))
         (thread
          (λ ()
            (for/fold ([sum 0]) 
                ([i (in-range N)])
              (printf "adder: ~a\n" (+ i sum))               
              (yield)
              (+ i sum)))))]

In this thread system, there are a few things to notice:

- You define a @racket[main] function that starts off the computation.

- You create threads by calling @racket[thread] with a thunk.

- Threads are cooperative and must @racket[yield] to one another.

Since we are yielding in a deterministic way, this program has a
deterministic output as well:

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

The threading system will be very simple: it keeps a list of
threads (represented as thunks) that can run and @racket[yield]
captures the context of the currently running thread, adds it to the
end of that list, and then executes the top of the list.

This is straight-forward to express in code:

@chunk[<thread-basics>
       (define ts empty)
       (define (yield)
         (match ts
           [(list)
            (void)]
           [(cons next rest)
            (let/cc k
              (set! ts (snoc rest k))
              (next))]))]

The final four lines are the essence of a context switch:

1. Choose the next context

2. Capture the current context (its stack, registers, etc.)

3. Save the current context in the thread queue

4. Re-establish the next context as the current context

Every threading system must do each of these four things, although
perhaps in a different order. In Racket, we can express each one in a
single line.

The only remaining thing is invoking the @racket[main] function and
implementing @racket[thread]. 

You might think that this is very obvious and want to write:

@chunk[<broken-thread>
       (define (thread t)
         (set! ts (cons t ts)))
       (main)]

Unfortunately, this is not correct. The problem is that it fails to
ever call @racket[yield] and actually invoke the threads. The program
would have no output.

Another idea is to automatically @racket[yield] every time a thread is
created:

@chunk[<yield-thread>
       (define (thread t)
         (set! ts (cons t ts))
         (yield))
       (main)]

The problem with this is that when the main program is finished
creating the two threads, it has nothing else to do, so it just ends
without calling @racket[yield] again. We could add a large number of
calls to @racket[yield] to the end of main, but that's a little
absurd, obviously brittle, and immoral.

What we really need is something like @racket[yield] but that doesn't
add the current context back on the queue, something that does a
context switch but @racket[exit]s the current context. For
convenience, we shouldn't require the programmer to ever call
this (although they may if they want), so we'll implicitly add it to
the end of every thread, including the main program.

@chunk[<exit-thread>
       (define (thread t)
         (set! ts (cons (λ () (t) (exit)) ts)))
       (define (exit)
         (match ts
           [(list)
            (void)]
           [(cons next rest)
            (set! ts rest)
            (next)]))
       
       (main)
       (exit)]

This version seems like it should be correct, but it actually has a
very strange output:

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
adder: 1
adder: 3
adder: 6
adder: 10
}

For some reason, after the adder ends, and the iterator ends (it has
two more steps), the adder starts again, but now from one step into
its computation. How could this be?

Here's what happens:

- The main thread creates the iterator, then the adder, then exits,
invoking the adder (because we add new threads to the front.)

- The adder does its first round and then invokes the iterator in its
call to @racket[yield.]

- At this point, the continuation of the iterator is the code after
the first round of the adder. In most cases, the continuation of the
call to @racket[yield] is only available in the thread queue, but this
case is different, because the initial thread thunk is not a
continuation that aborts its invoking context.

- Now, later on, when the iterator loop returns (by returning
@racket[void] inside the first case of the @racket[exit]), it returns
back to its initial continuation, or the second round of the adder.

In order to fix this, we could make the final call abort the
current context, so that code wasn't run, but it would be better to
ensure that it wasn't there in the first place.

@chunk[<best-thread>
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

The function @racket[abort-current-continuation] destroys the current
context and goes back to the beginning of the program. This is like
calling the normal OS exit() in a program: the program ends and
returns you to the command prompt. However,
@racket[abort-current-continuation] is a generalization of that
idea. It is more general in two ways:

1. Rather than there being only one "command prompt" that you can
return to, there are many that can be dynamically created, so you need
to name the one you return to. Our code returns to the default prompt.

2. Rather than only being able to return a number, this function has
you return a function that will be invoked to discover the value that
should be returned. This allows you to do something like trampolining,
where you destroy the current context and start up the computation
again with a "fresh" context (or stack.)

The other small tweak is that we have to explicitly wrap the call to
@racket[main] in the continuation prompt that we'll abort to. I think
of this little piece as the "kernel" that is the start and end of the
life of the program.

And there you have it! A small cooperative threading system
implemented in user-space using continuations!

Next time, we'll take this as a base and remove the cooperative aspect
then elaborate the kernel to add system calls! Aren't you excited?

By the way, if you use this code at home, make sure you put the code
in this order:

@chunk[<*>
       (require racket/list
                racket/match)

       (define (snoc l x)
         (append l (list x)))

       <thread-basics>

       <example>        
       
       <best-thread>]
