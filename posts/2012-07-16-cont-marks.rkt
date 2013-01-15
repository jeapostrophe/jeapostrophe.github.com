#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/list)
          "../post.rkt")

@title{Continuation Marks, part I: Dynamic Wind}
@categories["Racket" "Continuations" "Continuation Marks"]

One of my favourite features of Racket is also one of its most unique
features: continuation marks.

Continuation marks allow you to annotate the dynamic context of a
running program and later observe it to respond differently to your
context.

In the next few posts, I'll talk about continuation marks. But first,
we need to cover a different feature of Racket called
@racket[dynamic-wind]. (Most people pronounce it like winding a winch,
but I like to pronounce it like a gust of wind and say "Dynaaamic
WIND!" as if it was a Street Fighter move.)

@(the-jump)

The classic example of continuation marks is a flow-sensitive
variable, like the current indentation level of debugging printfs.

For example, suppose you have this program:

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

You want it to print as:

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

One obvious way is to use a global variable:

@chunk[<mutation>
       (define indent-level 0)
       (define (with-indentation t)
         (define originally indent-level)
         (set! indent-level (add1 originally))
         (t)
         (set! indent-level originally))
       (define (displayln/indent x)
         (for ([i (in-range indent-level)])
           (display " "))
         (displayln x))]

The key is to reset the indentation level after the thunk returns.

However, this is not a robust technique in the presence of control
effects. For example, if the code throws an exception than the reset
will not occur.

@chunk[<example2>
       (with-handlers ([exn:misc:match? (λ (x) 'failed)])
         (show-structure
          (list 1 (list 2 (list 3 'error!)))))
       (show-structure
        (list 1 (list 2 (list 3))))]

So it prints as:

@verbatim{
 1
  2
   3
'failed
    1
     2
      3          
}

And every subsequent is off by three. The same problem occurs when
continuations are captured and invoked.

Racket provides a feature called @racket[dynamic-wind] that helps you
write this mutation code safely with respect to control effects:

@chunk[<mutation-control-safe>
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

@racket[dynamic-wind] takes three functions. The first gets called
whenever the body is "entered", the second is the body and is just
called once at the start, and the third is called whenever the body is
exited. In normal situations, it is just like a sequence of three
calls.

But, when control leaves the body somehow---like through an exception,
abort, or continuation invocation---then the third function is called
to "undo" whatever state changes were made when the body was entered.

Similarly, when control enters the body somehow---like when a
continuation is captured inside the body and is invoked after the body
has already returned---then the first function is called again
to "redo" whatever state changes the body relies on.

Try to predict the output of these programs:

Example 1: Just a sequence of printfs.

@chunk[<dw-1>
       (begin
         (printf "In\n")
         (printf "Body\n")
         (printf "Out\n"))]

Example 2: An equivalent sequence, but within @racket[dynamic-wind].

@chunk[<dw-2>
       (begin
         (dynamic-wind
             (λ () (printf "In\n"))
             (λ () (printf "Body\n"))
             (λ () (printf "Out\n"))))]

Example 3: An almost identical sequence, but now with an exception.

@chunk[<dw-3>
       (with-handlers
           ([(λ (x) #t) (λ (x) x)])
         (dynamic-wind
             (λ () (printf "In\n"))
             (λ () 
               (printf "Body Pre\n")
               (raise 'error)
               (printf "Body Post\n"))
             (λ () (printf "Out\n"))))]

Example 4: Now we replace the raise with continuation capture and
invoke the continuation.

@chunk[<dw-4>
       (begin
         (define the-k #f)
         (when
             (dynamic-wind
                 (λ () (printf "In\n"))
                 (λ () 
                   (printf "Body Pre\n")
                   (begin0
                     (let/cc k                  
                       (set! the-k k))
                     (printf "Body Post\n")))
                 (λ () (printf "Out\n")))
           (the-k #f)))]

Bonus question: Why doesn't this code infinite loop?

Example 5: Now rather than using mutation, we communicate the
continuation with an exception.

@chunk[<dw-5>
       (begin
         (with-handlers 
             ([continuation?
               (λ (k)
                 (k 0))])
           (dynamic-wind
               (λ () (printf "In\n"))
               (λ () 
                 (printf "Body Pre\n")
                 (let/cc k
                   (raise k))
                 (printf "Body Post\n"))
               (λ () (printf "Out\n")))))]

Example 6: Invoking the continuation repeatedly inside the exception
handler.

@chunk[<dw-6>
       (begin
         (with-handlers 
             ([pair?
               (λ (k*n)
                 (printf "Pong\n")
                 ((car k*n) (add1 (cdr k*n))))])
           (dynamic-wind
               (λ () (printf "In\n"))
               (λ () 
                 (printf "Body Pre\n")
                 (let loop ([i 0])
                   (unless (= i 3)
                     (printf "Ping ~a\n" i)
                     (loop
                      (let/cc k
                        (raise (cons k i))))))
                 (printf "Body Post\n"))
               (λ () (printf "Out\n")))))]

Next week we'll go on to how @racket[dynamic-wind] relates to
continuation marks.

And here is the expected output, by the way:

@verbatim{
Example 1
In
Body
Out

Example 2
In
Body
Out

Example 3
In
Body Pre
Out
'error

Example 4
In
Body Pre
Body Post
Out
In
Body Post
Out

Example 5
In
Body Pre
Out
In
Body Post
Out

Example 6
In
Body Pre
Ping 0
Out
Pong
In
Ping 1
Out
Pong
In
Ping 2
Out
Pong
In
Body Post
Out
}

By the way, if you use this code at home, make sure you put the code
in this order:

@chunk[<*>
       (require racket/match)

       (let ()
         (printf "Control un-Safe\n")
         <mutation>
         <show-structure>
         <example1>
         <example2>)

       (let ()
         (printf "Control Safe\n")
         <mutation-control-safe>
         <show-structure>
         <example1>
         <example2>)

       (printf "\nExample 1\n")
       <dw-1>
       (printf "\nExample 2\n")
       <dw-2>
       (printf "\nExample 3\n")
       <dw-3>
       (printf "\nExample 4\n")
       <dw-4>
       (printf "\nExample 5\n")
       <dw-5>
       (printf "\nExample 6\n")
       <dw-6>]
