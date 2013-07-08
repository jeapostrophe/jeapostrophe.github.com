#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/unit
                     racket/match
                     racket/list
                     data/heap)
          "../post.rkt")

@title{What is a Stack?}
@categories["Computer Science"]

It is common for programming language implementations to use stacks to
implement function call and return. It is also common for novice
programmers to misunderstanding the meaning of stacks in this
context. This post explains how they are used and what the common
misunderstanding is.

@(the-jump)

@section{A Language without Functions}

Our programming languages are implemented by compilation to low-level
CPU languages that lack almost all forms of abstraction including
functions. Functions encapsulate many forms of abstraction, but the
main one is control-flow. The code @racket[(begin (f) (g))] abstracts
executing the code of @racket[f] followed by @racket[g], as opposed to
directly inserting that code into the @racket[begin] block.

In order to demonstrate this better, let's use a little interpreter
for a CPU-like language. In this language, we'll have a small number
of instructions and a bunch of registers.

This program performs two additions and returns the result:

@chunk[<basic>
       (check-equal?
        (cpu
         (LOAD 0 5)
         (LOAD 1 7)
         (ADD 2 0 1)
         (LOAD 3 8)
         (ADD 4 2 3)
         (EXIT 4))
        '(20))]

The @racket[LOAD] instruction has a destination register first and a
value second. The @racket[ADD] instruction has the destination
register and then the source registers. @racket[EXIT] has a status
source register as its argument.

This program has no control-flow other than "execute the next
instruction after this one". If we had @racket[GOTO] and labels, then
we could write a program like this:

@chunk[<basic/goto>
       (check-equal?
        (cpu
         (GOTO main)
         (LABEL second)
         (LOAD 3 8)
         (ADD 4 2 3)
         (EXIT 4)
         (LABEL first)
         (LOAD 0 5)
         (LOAD 1 7)
         (ADD 2 0 1)
         (GOTO second)
         (LABEL main)
         (GOTO first))
        '(20))]

@section{A Language with Functions}

However, @racket[GOTO]s like this aren't really like function calls,
because the @litchar{first} block directly calls the @litchar{second}
block. We really want code more like:

@chunk[<basic/call>
       (check-equal?
        (cpu
         (GOTO main)
         (LABEL second)
         (LOAD 3 8)
         (ADD 4 2 3)
         (EXIT 4)
         (LABEL first)
         (LOAD 0 5)
         (LOAD 1 7)
         (ADD 2 0 1)
         (RETURN)
         (LABEL main)
         (CALL first)
         (CALL second))
        '(20))]

What's the difference between @racket[CALL] and @racket[GOTO]? A
@racket[CALL] is a @racket[GOTO] that records its next instruction on
a stack. In the interpreter, it runs

@chunk[<call-code>
       (push! stack (add1 PC))]

before it jumps to the given label. In contrast, @racket[RETURN]
rather than jumping to @racket[(add1 PC)] jumps to

@chunk[<return-code>       
       (pop! stack)]

which was previously pushed by the @racket[CALL].

@section{The Misunderstanding}

In our example program, when the code for @litchar{first} is running,
the stack contains a program address from the @litchar{main}
block. @litchar{main} called @litchar{first} so it may seem that the
stack contains an abstraction of the @emph{past} of the computation.

This idea inspires many
@link["http://neopythonic.blogspot.com/2009/04/tail-recursion-elimination.html"]{novice
programmers} to believe that stack traces (read-outs of the stack) are
useful tools for determining how the currently running code started
executing, i.e. its @emph{history}.

However, this is incorrect.

The stack does not record the @emph{past} of the computation, it only
records its @emph{future}. The address that @racket[CALL] pushes on to
the stack has never been executed and will only execute when
@racket[RETURN] runs. If the @emph{past} of the computation contains
any branches that have already returned, then the stack will hide
these, such as in the code for @litchar{second}.

We can change our interpreter to remember the exact program history
and allow us to export it (along with the stack) to compare them.

@chunk[<basic/history>
       (check-equal?
        (cpu
         (GOTO main)
         (LABEL second)
         (EXPORT 2nd)
         (LOAD 3 8)
         (ADD 4 2 3)
         (EXIT 4)
         (LABEL first)
         (EXPORT 1st)
         (LOAD 0 5)
         (LOAD 1 7)
         (ADD 2 0 1)
         (RETURN)
         (LABEL main)
         (CALL first)
         (CALL second))
        '((1st (history 7 6 13 12 0)
               (stack 14)) 
          (2nd (history 2 1 14 11 10 9 8 7 6 13 12 0)
               (stack 15))
          20))]

As you can see, in the history of the second export, you can see that
@litchar{first} (PC 6--11) is recorded, but the stack only contains
@litchar{main} (PC 12--14).

@section{The Understanding}

Once we realize that @emph{stacks are future and not history},
tail-call optimization becomes much more clear: if a computation adds
nothing to the future, it doesn't need to add anything to the stack.

For example, in this program, @litchar{first} calls @litchar{second}
and does nothing with the result:

@chunk[<tco/before>
       (check-equal?
        (cpu (CALL first)
             (CALL second)
             (EXPORT end)
             (EXIT 2)
             (LABEL first)
             (LOAD 0 5)
             (LOAD 1 6)
             (ADD 2 0 1)
             (CALL second)
             (RETURN)
             (LABEL second)
             (ADD 2 0 2)
             (RETURN))
        '((end (history 2 12 11 10 1 9 12 11 10 8 7 6 5 4 0)
               (stack))          
          21))]

It is trivial to recognize this situation, because it corresponds to a
computation that contains the sequence @racket[(CALL
label) (RETURN)]. We can just change it to @racket[(GOTO label)].

@chunk[<tco/after>
       (check-equal?
        (cpu (CALL first)
             (CALL second)
             (EXPORT end)
             (EXIT 2)
             (LABEL first)
             (LOAD 0 5)
             (LOAD 1 6)
             (ADD 2 0 1)
             (GOTO second)
             (LABEL nop)
             (LABEL second)
             (ADD 2 0 2)
             (RETURN))
        '((end (history 2 12 11 10 1 12 11 10 8 7 6 5 4 0)
               (stack))
          21))]

We can see that in the history of the second version, it doesn't
execute PC 9 after 12, but more important it doesn't allocate any
stack space for the call inside @litchar{first}.

@section{Yo! It's almost time to go!}

But first let's remember what we learned today!

CPUs don't give you functions, but they do give you instructions like
@racket[CALL] and @racket[RETURN] which @emph{can} be used to
implement functions.

The stack (as used by @racket[CALL]/@racket[RETURN]) records the
@emph{future} of your computation. Stack traces are, therefore, a
reading of a potential future (only potential because of real-world
features like exceptions and continuations) and have @emph{nothing to
do} with the @emph{past} of the computation.

If you want to know the @emph{history} of your computation, you have
to get it some other way---the stack doesn't provide it.

If you'd like to run this exact code at home, you should put it in
this order:

@chunk[<*>
       (require racket/match
                racket/list
                data/gvector)

       (define-syntax-rule (cpu inst ...)
         (cpu-execute (vector 'inst ...)))

       (define-syntax-rule (push! stack v)
         (set! stack (cons v stack)))
       (define-syntax-rule (pop! stack)
         (begin0 (first stack)
                 (set! stack (rest stack))))

       (define (cpu-execute iv)
         (define label->PC (make-hasheq))
         (for ([i (in-vector iv)] [PC (in-naturals)])
           (match i
             [`(LABEL ,l)
              (hash-set! label->PC l PC)]
             [x
              (void)]))

         (define registers (make-gvector))
         (define (rref r) (gvector-ref registers r))
         (define (rset r v) (gvector-set! registers r v))

         (define stack empty)
         (define history empty)

         (let loop ([PC 0])
           (push! history PC)
           (match (vector-ref iv PC)
             [`(LOAD ,r ,v)
              (rset r v)
              (loop (add1 PC))]
             [`(ADD ,d ,l ,r)
              (rset d (+ (rref l) (rref r)))
              (loop (add1 PC))]
             [`(EXIT ,r)
              (list (rref r))]
             [`(LABEL ,l)
              (loop (add1 PC))]
             [`(GOTO ,l)
              (loop (hash-ref label->PC l))]
             [`(CALL ,l)
              <call-code>
              (loop (hash-ref label->PC l))]
             [`(RETURN)
              (loop <return-code>)]
             [`(EXPORT ,id)
              (cons (list id 
                          (cons 'history history)
                          (cons 'stack stack))
                    (loop (add1 PC)))])))

       (require rackunit)

       <basic>
       <basic/goto>
       <basic/call>
       <basic/history>
       <tco/before>
       <tco/after>]

@(the-end)
