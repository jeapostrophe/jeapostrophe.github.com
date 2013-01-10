#lang scribble/lp
@(require "post.rkt"
          (for-label (except-in racket/base
                                thread
                                exit
                                printf)
                     (prefix-in racket: racket/base)
                     rackunit
                     racket/list))

@title{Domain-Specific Operating Systems: Threads, System Calls, and Continuations}
@categories["Racket" "Continuations" "Threads" "Concurrency" "Domain-Specific Operating Systems"]

In the past few posts I've been writing about threading system that
are based on continuations in user-land. This post I'll extend that
system to provide system calls that control access to sensitive
resources, such as files and the thread pool.

@(the-jump)

We'll be working from the same example program as last week, except
that we'll be changing @racket[printf] from a "primitive" function to
a system call. Here's the program:

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

In the original threading system, global mutable variables were used
to handle the state of the threading system:

@chunk[<basic-threading-system>
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

       (define (printf . args)
         (begin0 (apply racket:printf args)
                 (yield)))
       
       (call-with-continuation-prompt
        (λ ()
          (main)
          (exit)))]

Notice that the "logical" system calls---@racket[thread],
@racket[exit], @racket[yield] and @racket[printf]---all mutate the
thread system's state (@racket[ts]). This makes the system difficult
to test and analyze.

Our goal is to tease out all this code into a single "kernel" that
actually @emph{is} the threading system and exists independently from the
state of the various threads. Here's a sketch of the kernel:

@chunk[<kernel>
       (struct kernel (threads))

       (define (boot main)
         (define initial (kernel (list main)))
         (let loop ([ks initial])
           (unless (empty? (kernel-threads ks))
             (loop (step-one-thread ks)))))]

The state of the kernel will simply be the list of threads and all the
kernel really does is continuously call @racket[step-one-thread] to
advance the state of the kernel until all the threads exit. All the
work will, of course, take place in @racket[step-one-thread]:

@chunk[<step-one-thread>
       (define (step-one-thread ks)
         (match-define (kernel (cons top-thread other-threads)) ks)
         (define syscall (run-thread-until-syscall top-thread))
         (execute-syscall syscall (kernel other-threads)))]

Its job is simply to select the first thread, run it until it reaches
a system call and then deal with the system call.

Let's represent each system call as structure:

@chunk[<syscalls>
       (struct syscall (user-context))

       (struct syscall:thread syscall (child-thunk))
       (struct syscall:exit syscall ())
       (struct syscall:printf syscall (fmt arg))]

The thing that all system calls have in common is that the context of
the user program is preserved, but other than that, each is distinct
in the data that it carries.

Once this is in place, we can pretty easily write the code to handle
the system calls and update the kernel's state:

@chunk[<execute-syscall>
       (define (execute-syscall call kernel-state)
         (match-define (kernel threads) kernel-state)
         (match call
           [(syscall:thread user-ctxt child-t)
            (kernel (list* user-ctxt child-t threads))]
           [(syscall:exit user-ctxt)
            (kernel threads)]
           [(syscall:printf user-ctxt fmt arg)
            (racket:printf fmt arg)
            (kernel (snoc threads user-ctxt))]))]

A new thread just needs to push both contexts (the parent and the
child) onto the thread queue. (We add them to the top of the queue to
preserve compatibility with the previous versions of this code.) When
a thread exits, the user context is thrown away. When a
@racket[printf] occurs, the string is displayed and the user context
is placed back on the queue, at the end.

We only need to do two more things: implement
@racket[run-thread-until-syscall] and implement the code that the user
programs call to return system call structures to the kernel. These
are tightly intertwined.

The second is actually simpler, so we'll do that first. The basic idea
is to capture the continuation going back to the kernel and throw it
back to the kernel as something like an exception (really, an abort,
which you can think of as an exception that can only be caught by
privileged code.)

@chunk[<syscall-throw:thread>
       (define (thread child-t)
         (call-with-composable-continuation
          (λ (user-ctxt)
            (abort-current-continuation
             kernel-prompt-tag
             (syscall:thread user-ctxt child-t)))
          kernel-prompt-tag))]

The other code will be very similar to this, so we'll write a macro to
simplify it:

@chunk[<syscall-throw>
       (define-syntax-rule 
         (define-syscall-throw user-id syscall-id)
         (define (user-id . syscall-args)
           (call-with-composable-continuation
            (λ (user-ctxt)
              (abort-current-continuation
               kernel-prompt-tag
               (apply syscall-id user-ctxt syscall-args)))
            kernel-prompt-tag)))

       (define-syscall-throw thread syscall:thread)
       (define-syscall-throw exit syscall:exit)
       (define-syscall-throw printf syscall:printf)]

Now that we know how the "thrower" works, we can easily implement
the "catcher":

@chunk[<syscall-catch>
       (define kernel-prompt-tag 
         (make-continuation-prompt-tag 'kernel))
       (define (run-thread-until-syscall thread-ctxt)
         (call-with-continuation-prompt
          (λ ()
            (thread-ctxt)
            (exit))
          kernel-prompt-tag
          values))]

This code says that you just invoke the thread context like a
function, but you wrap it in a new prompt. When the code inside aborts
with a value, then it is returned to the caller of
@racket[run-thread-until-syscall] (that's what the @racket[values]
means). We always put in call to @racket[exit], in case the code just
ends.

I love this code because it so elegantly separates all the different
features of an operating system: capturing system calls and program
contexts, evaluating the impact on the kernel state for the call, and
running the OS itself. From this foundation you could add other system
calls and scheduling behavior. I call this a "domain-specific
operating system."

One of my pet projects is to build video games in Racket. In that
project, I'm using this idea to implement each agent in the game
world (the enemies, the player, etc) as processes with custom system
calls to interact with each other and the audio/video resources. The
kernel there maintains two separate lists of threads: ones that need
to do more work on this frame and ones that are done until the next
frame needs to be rendered, with a system call to indicate to the OS
that all work for a frame is done. It's really quite beautiful. Maybe
I'll write more about it in the future.

By the way, if you use this code at home, make sure you put the code
in this order:

@chunk[<*>
       (require racket/list
                racket/match
                (prefix-in racket: racket/base))

       (define (snoc l x)
         (append l (list x)))

       <kernel>
       <step-one-thread>
       <syscalls>
       <execute-syscall>
       <syscall-throw>
       <syscall-catch>

       <example>

       (boot main)]
