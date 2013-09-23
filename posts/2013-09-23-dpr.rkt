#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/unit
                     racket/match
                     racket/list
                     data/heap)
          "../post.rkt")

@title{Defer, Panic, and Recover in Racket}
@categories["Racket" "Go" "Macros" "Semantics" "Specification"]

In this post, we show how to implement defer, panic, and recover from
the Go language in about fifty lines of Racket macros. If you aren't
familiar with these Go features, I'll be trying to adapt
@link["http://golang.org/doc/articles/defer_panic_recover.html"]{the
Go documentation} into the post, so don't worry.

@(the-jump)

These features attempt to provide something like exception handlers
and Racket's @racket[dynamic-wind]. Defer delays an expression's
evaluation until after the rest of the function returns (successfully
or not). Panic is like throwing an exception and Recover is a
procedural interface to inspecting if an exception is currently being
thrown. Recover in normal code is like a no-op, but inside of a
Deferred expression it is like a handler. One difference, however, is
that Recover @emph{always} recovers regardless of the kind of
exception thrown and you must explicitly re-throw if the "handler"
can't handle that kind of error.

Here's a small example of just Defer.

@chunk[<example-a>
       (define/dpr (a)
         (define i 0)
         (defer-funcall (printf "~a\n" i))
         (set! i (add1 i))
         i)]

This function returns 1 and prints out 0. In Go, Defer statements must
always be a single function call and not an arbitrary expression. This
is because Go will evaluate the arguments to the function before
saving the call, which is why this function prints out 0 and not
1. The macros I show implement a slightly more general version that
accepts any sequence of expressions. What my macro allows is done in
Go by writing an inline thunk... that is manually writing out the
expansion of this macro:

@chunk[<defer>
       (define-syntax-rule (defer . e)
         (defer-funcall ((λ () . e))))]

For an analogy to traditional Racket, this use of Defer is equivalent
to:

@chunk[<non-example-a>
       (define (non-a)
         (define i 0)
         (let ([iv i])
           (dynamic-wind void
               (λ ()
                 (set! i (add1 i))
                 i)
               (λ ()
                 (printf "~a\n" iv)))))]

As you can see, this style of programming is quite painful. We can
easily imagine a @racket[defer] macro that cooperated with
@racket[define] to transform the program into this output. It would be
easy if @racket[defer] had to occur at the function's top-level, but
would be much more complicated if @racket[defer] could occur
anywhere. In the future, I'll investigate that option, but for now
we'll make a simpler version that is more dynamic. The output will be
like:

@chunk[<non-example-a2>
       (define (non-a2)
         (define defered #f)
         (define i 0)
         (set! defered
               (let ([iv i])
                 (λ ()
                   (printf "~a\n" iv))))
         (set! i (add1 i))
         (begin0 i
                 (defered)))]

It is important, however, that there can be many deferred functions
and that they are executed in Last In First Out order. For instance,
this function should print "3210" and not "0123".

@chunk[<example-b>
       (define/dpr (b)
         (for ([i (in-range 4)])
           (defer (printf "~a" i))))]

This is very convenient to implement with a @racket[cons] list and a
queuing/queue-consuming function like:

@chunk[<deferred-interface>       
       (define ds empty)
       (define (add-deferred f)
         (set! ds (cons f ds)))
       (define (run-defers)
         (for ([d (in-list ds)])
           (d)))]

We'll use a syntax parameter called @racket[queue-defer] to
communicate the particular @racket[add-deferred] function to the
@racket[defer-funcall] macro.

@chunk[<defer-funcall>
       (define-syntax-parameter queue-defer #f)
       (define-syntax (defer-funcall stx)
         (define qdv (syntax-parameter-value #'queue-defer))
         (unless qdv
           (raise-syntax-error 'defer-funcall
                               "Illegal use outside of define/dpr"
                               stx))
         (syntax-case stx ()
           [(_ (f a ...))
            (with-syntax ([(v ...) (generate-temporaries #'(a ...))]
                          [qdv qdv])
              (syntax/loc stx
                (qdv (let ([v a] ...) (λ () (f v ...))))))]))]

The only interesting thing about this macro is that it is forces a
function application syntax and evaluates the arguments before
constructing the closure that will be called later.

One thing that Go supports that we will not is the ability for
deferred functions to modify the named return values after they have
been computed by the main body. This would be complicated to add
because Racket doesn't have named return values, but I think the
effort would be orthogonal. Once you had them, you would get
modification in deferred code for free by composing the macros.

The more interesting behavior comes from Panic and Recover. The Go
documentation contains the following example:

@chunk[<panic-example>
       (define/dpr (main recover?)
         (f recover?)
         (printf "Returned normally from f.\n"))

       (define/dpr (f recover?)
         (when recover?
           (defer
             (define r (recover))
             (when r
               (printf "Recovered in f ~a\n" r))))
         (printf "Calling g.\n")
         (g 0)
         (printf "Returned normally from g.\n"))

       (define/dpr (g i)
         (when (> i 3)
           (printf "Panicking!\n")
           (panic (format "~a" i)))
         (defer (printf "Defer in g ~a\n" i))
         (printf "Printing in g ~a\n" i)
         (g (add1 i)))]

This program has a call sequence of @racket[main], then @racket[f],
then three calls to @racket[g], before potentially having a Panic. The
Panic causes the rest of the final call to @racket[g] to not execute
and instead the deferred expressions in previous @racket[g] calls run
before the deferred Recover in @racket[f] takes control. If
@racket[recover?] is @racket[#t], then the output is:

@verbatim{
Calling g.
Printing in g 0
Printing in g 1
Printing in g 2
Printing in g 3
Panicking!
Defer in g 3
Defer in g 2
Defer in g 1
Defer in g 0
Recovered in f 4
Returned normally from f.          
}

But if @racket[recover?] is @racket[#f], then the final two lines are
not printed and the Panic ultimately kills the thread as a traditional
uncaught exception would.

In our Racket implementation, we will represent Panic as throwing a
particular kind of exception and encode Recover has inspecting
the "current" Panic.

@chunk[<panic-and-recover>
       (struct exn:panic exn (v [recovered? #:mutable]))
       (define (panic v)
         (raise 
          (exn:panic (format "panic ~e" v)
                     (current-continuation-marks)
                     v
                     #f)))

       (define current-panic (make-parameter #f))
       (define (recover)
         (define p (current-panic))
         (and p
              (set-exn:panic-recovered?! p #t)
              (exn:panic-v p)))]

In this code, we implement Go's behavior of a Recover leaving an
impact on the Panic through the use of a mutable field on the
@racket[exn:panic] structure.

This code cooperates with the @racket[define/dpr] macro, which
installs the following exception handler around all the code that it
executes:

@chunk[<panic-handler>
       (λ (p)
         (parameterize ([current-panic p])
           (run-defers))
         (unless (exn:panic-recovered? p)
           (raise p)))]

After a Panic is observed, we still run the deferred code and
afterwards, if one of them did not Recover, then we @racket[raise] the
Panic again, so the next layer can fail.

We can wrap these pieces together with a simple macro:

@chunk[<define/dpr>
       (define-syntax-rule
         (define/dpr (fun . fmls) . body)
         (define (fun . fmls)
           <deferred-interface>
           (with-handlers ([exn:panic? <panic-handler>])
             (begin0
               (syntax-parameterize ([queue-defer #'add-deferred])
                 . body)
               (run-defers)))))]

The only interesting thing here are that we use @racket[begin0] to
return the result of the body even though we run the deferred code
afterwards.

At this point, all the tests on the Go documentation page pass
correctly. I wrote a little macro that allows us to easily run
something while checking its return value and what its output is.

@chunk[<basic-tests>
       <example-a>
       (test (a) 1 "0\n")
       <example-b>
       (test (b) (void) "3210")

       <panic-example>
       (test (main #t) (void) "Calling g.\nPrinting in g 0\nPrinting in g 1\nPrinting in g 2\nPrinting in g 3\nPanicking!\nDefer in g 3\nDefer in g 2\nDefer in g 1\nDefer in g 0\nRecovered in f 4\nReturned normally from f.\n")
       (test (main #f) "4" "Calling g.\nPrinting in g 0\nPrinting in g 1\nPrinting in g 2\nPrinting in g 3\nPanicking!\nDefer in g 3\nDefer in g 2\nDefer in g 1\nDefer in g 0\n")]

At this point, we have a conforming implementation of Defer, Panic,
and Recover. Unfortunately, this implementation has some strange
behavior that the Go language documentation does not comment on what
should happen. It all has to do with what should happen when Panics
occur inside of Deferred code. For instance, consider this:

@chunk[<panic-in-defer>
       (define/dpr (panic-in-defer)
         (defer (displayln 0))
         (defer (displayln 1))
         (defer (displayln 2) (panic '!))
         (defer (displayln 3)))]

I would expect this to return @litchar{3210}, but with the above
implementation it returns @litchar{3232}. Unfortunately, Go does not
provide us with the behavior it should be. Since it feels right to me,
let's change it to be @litchar{3210}.

The problem is that the two calls to @racket[run-defers]---in the
success and the fail case---do not coordinate with each other to
ensure that a deferred computation only happens a single time. We can
fix that with a new interface to the defer queue. We'll replace
@racket[<deferred-interface>] with

@chunk[<deferred-interface-v2>       
       (define ds empty)
       (define (add-deferred f)
         (set! ds (cons (box f) ds)))
       (define (run-defers)
         (for ([db (in-list ds)])
           (define d (unbox db))
           (set-box! db #f)
           (when d
             (d))))]

The key here is that as @racket[run-defers] is executing, it removes
the deferred values from the list by replacing them with
@racket[#f]. It is important it removes them before they return,
because otherwise the failing deferred expression would be run twice.

However, this implementation still doesn't survive another weird
example, where there are multiple Panics during the Defer stage. For
instance,

@chunk[<panic-in-recover>
       (define/dpr (panic-in-recover)
         (defer (displayln 0))
         (defer (displayln 1) (panic '+))
         (defer (displayln (recover)))
         (defer (displayln 2) (panic '!))
         (defer (displayln 3)))]

It returns @litchar{32!1} and not @litchar{32!10}. 

The problem is that inside of @racket[<panic-handler>] when it calls
@racket[run-defers], the Panic handler is not re-installed to deal
with further Panic events inside the same call frame. We can solve
this by moving the handler inside of @racket[run-defers]:

@chunk[<deferred-interface-v3>       
       (define ds empty)
       (define (add-deferred f)
         (set! ds (cons (box f) ds)))
       (define (run-defers)
         (with-handlers ([exn:panic? <panic-handler>])                  
           (for ([db (in-list ds)])
             (define d (unbox db))
             (set-box! db #f)
             (when d
               (d)))))]

This allows us to refactor @racket[define/dpr] a little because the
first call to @racket[run-defers] does not need to be inside of two
@racket[with-handlers] forms:

@chunk[<define/dpr-v3>
       (define-syntax-rule
         (define/dpr (fun . fmls) . body)
         (define (fun . fmls)
           <deferred-interface-v3>
           (begin0
             (with-handlers ([exn:panic? <panic-handler>])                  
               (syntax-parameterize ([queue-defer #'add-deferred])
                 . body))
             (run-defers))))]

Now, we've successfully implemented a plausible version of Defer,
Panic, and Recover. Unfortunately we don't know if it is what the Go
designers intended, since they are vague in their specification.

As a final shot, something that is unfortunate about this
implementation is that it is not safe-for-space. The @racket[begin0]
accumulates space on the stack to check to see if any Defers had taken
place. You might think that you could check at the time of the call in
tail position if any had been accumulated, but the Racket
implementation is more powerful than the Go version, because you can
write code like:

@chunk[<not-safe-for-space>
       (define/dpr (not-safe l)
         (map (λ (x) (defer (displayln x))) l))]

In this example, the @racket[defer] in the higher-order argument
refers to the Defer of the @racket[not-safe] function and not the
anonymous function. In Go, you are restricted to only first-order uses
of Defer, so this is apparently not a relevant concern.

The Racket version also has some unintuitive behavior like the
following where a higher-order result causes Defers to an earlier
call:

@chunk[<unintuitive>
       (define/dpr (defer-later x)
         (λ (y)
           (defer x)
           y))]

This particular Defer is not observable, because the Defer goes into
the past. But using continuations, we can "go back in time" and see
it:

@chunk[<back-in-time>
       (define (back-in-time)
         (define deferred? #f)
         (define/dpr (deferrer x)
           (displayln "In deferrer")
           (let/cc k
               (λ (y)
                 (displayln "In closure")
                 (defer
                   (displayln "In deferred")
                   (set! deferred? #t))
                 k)))

         (define clo (deferrer 1))
         (displayln "After deferrer")
         (unless deferred?
           (displayln "Before closure")
           (define k (clo 2))
           (displayln "After closure")
           (k 2)
           (displayln "After k")))]

This program outputs

@verbatim{
In deferrer
After deferrer
Before closure
In closure
After closure
In deferred
After deferrer
}

This is actually unsurprising, because as we discussed in the
beginning, @racket[defer] is a special case of @racket[dynamic-wind],
which is specifically designed to deal with continuation-based time
travel.

@section{Yo! It's almost time to go!}

But first let's remember what we learned today!

Go's Defer, Panic, and Recover are special cases of
@racket[dynamic-wind] and exception handling, but with a convenient
syntax that does not induce right-ward drift.

Racket's macro system allows us to recreate Go's behavior and syntax.

Go does not specify what the behavior of Defer, Panic, and Recover is
in great enough detail to determine what they should do when there
are "recursive" Panics. It would not be sufficient to look at Go's
implementation, because that would only tell us what it @emph{happens
to} do, not what it @emph{should} do.

Defer necessarily consumes stack space and our implementation is not
pay-as-you-go and cannot be made to be due to its power over Go's
version, specifically because it allows higher-order, non-top-level
uses of Defer.

If you'd like to run this exact code at home, you should put it in
this order:

@chunk[<*>
       (require (for-syntax racket/base)
                racket/list
                racket/stxparam
                rackunit)       

       <defer-funcall>       
       <panic-and-recover>

       (define-syntax-rule (test c e o)
         (begin
           (define os (open-output-string))
           (check-equal? (parameterize ([current-output-port os])
                           (with-handlers ([exn:panic?
                                            (λ (p)
                                              (exn:panic-v p))])
                             c))
                         e)
           (check-equal? (get-output-string os) o)))

       <non-example-a>
       (test (non-a) 1 "0\n")
       <non-example-a2>
       (test (non-a2) 1 "0\n")       

       (let ()
         <define/dpr>
         <defer>
         <basic-tests>

         <panic-in-defer>
         (test (panic-in-defer) '! "3\n2\n3\n2\n"))

       (let ()
         (define-syntax-rule
           (define/dpr (fun . fmls) . body)
           (define (fun . fmls)
             <deferred-interface-v2>
             (with-handlers ([exn:panic? <panic-handler>])                  
               (begin0
                 (syntax-parameterize ([queue-defer #'add-deferred])
                   . body)
                 (run-defers)))))
         <defer>
         <basic-tests>
         
         <panic-in-defer>
         (test (panic-in-defer) '! "3\n2\n1\n0\n")

         <panic-in-recover>
         (test (panic-in-recover) '+ "3\n2\n!\n1\n"))

       (let ()
         <define/dpr-v3>
         <defer>
         <basic-tests>
         
         <panic-in-defer>
         (test (panic-in-defer) '! "3\n2\n1\n0\n")

         <panic-in-recover>
         (test (panic-in-recover) '+ "3\n2\n!\n1\n0\n")

         <not-safe-for-space>
         (test (not-safe (list 1 2)) 
               (list (void) (void))
               "2\n1\n")

         <unintuitive>
         (test ((defer-later 5) 4)
               4
               "")

         <back-in-time>
         (test (back-in-time)
               (void)
               "In deferrer\nAfter deferrer\nBefore closure\nIn closure\nAfter closure\nIn deferred\nAfter deferrer\n"))]

@(the-end)
