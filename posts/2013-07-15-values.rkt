#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/unit
                     racket/match
                     racket/list
                     data/heap)
          "../post.rkt")

@title{Symmetry in Function Calls and Returns}
@categories["Racket" "Macros"]

I find symmetry and uniformity beautiful. Something particularly
beautiful to me is the way that Racket provides some symmetry in
function calls, which can take any number of arguments, and function
returns, which can return any number of answers. However, things in
Racket are not as symmetric as they could be, as I discuss and correct
in this post.

@(the-jump)

@section{The Basics of @racket[values]}

It is trivial to observe that functions in Racket can take 0, 1, or
many arguments:

@chunk[<basic-funs>
       (define (zero-args) 
         0)
       (check-equal? (zero-args) 0)

       (define (one-arg first) 
         first)
       (check-equal? (one-arg 1) 1)

       (define (many-args first second third)
         (list first second third))
       (check-equal? (many-args 1 2 3)
                     (list 1 2 3))]

Indeed, many languages do this as well. However, Racket is fairly
unique in allowing functions to return 0, 1, or many results:

@chunk[<basic-vals>
       (define (zero-vals) 
         (values))
       (define (one-val) 
         (values 1))
       (define (many-vals)
         (values 1 2 3))]

This is sometimes challenging to beginners in Racket. They wonder if
@racket[values] creates some sort of primitive @racket[list] or
@racket[vector] or if multiple values are "spliced" into place. In
other words, they wonder if @racket[(= (+ 1 2) (+ (values 1 2)))] is
true or an error that @racket[+] cannot be applied to a "multiple
value thing".

The second idea is closer to the truth, but also wrong. The expression
@racket[(+ (values 1 2))] results in an error, but it has nothing to
do with @racket[+]. @racket[(+ (values 1 2))] is an abbreviation of
@racket[(#%plain-app + (values 1 2))] and @racket[#%plain-app] puts
@racket[(values 1 2)] into a context like @racket[(let ([x ....])
....)] which is expecting it to evaluate to exactly one value, but it
evaluates to two. Thus, the error says "result arity mismatch".

@chunk[<error-ex1>
       (check-exn
        (λ (x) (regexp-match #rx"result arity mismatch" (exn-message x)))
        (λ () (+ (values 1 2))))]

This situation is symmetric to when you try to apply a function to too
many arguments, such as with @racket[(one-arg 1 2)].

@chunk[<error-ex2>
       (check-exn
        (λ (x) (regexp-match #rx"arity mismatch" (exn-message x)))
        (λ () (one-arg 1 2)))]

This arity error gives a clue to what @racket[values] means: when a
function normally returns, you can think of it as "calling its
continuation" with the return value. All @racket[values] does it call
that continuation with more arguments:

@chunk[<basic-vals/k>
       (define (zero-vals/k) 
         (let/cc k
           (k)))
       (define (one-val/k) 
         (let/cc k
           (k 1)))
       (define (many-vals/k)
         (let/cc k
           (k 1 2 3)))]

The larger question, then, is "How do I create a continuation that
expects more than one value?" Racket supports two ways of doing this:
@racket[let-values] and @racket[call-with-values]. @racket[let-values]
creates a continuation that accepts a fixed number of values:

@chunk[<basic-conts>
       (check-equal?
        (let-values ([() (zero-vals/k)]
                     [(a) (one-val/k)]
                     [(b c d) (many-vals/k)])
          (list a b c d))
        (list 1 1 2 3))]

@racket[call-with-values] is more exciting and creates a continuation
that accepts any number of values and then delivers them all as
arguments to another function.

@chunk[<basic-conts/call>
       (check-equal? (call-with-values zero-vals/k list) (list))
       (check-equal? (call-with-values one-val/k list) (list 1))
       (check-equal? (call-with-values many-vals/k list) (list 1 2 3))]

You can think of @racket[let-values] as being pretty syntax for
constructing a call to @racket[call-with-values] with the right
function in the second position, as if @racket[call-with-values] is
@emph{the} "application" primitive in a multiple-values world. (This
is not actually true though, even though @racket[call-with-values]
is (and has to be) built in to the Racket VM.)

@section{The Asymmetry of @racket[values]}

Given the previous section, it appears that Racket is beautifully
symmetric in your ability to create functions and co-functions that
accept any number of arguments. However, this is not true even in
basic Racket. 

For example, in Racket you can create a function that accepts any
number of arguments and gives only a finite prefix names while
referring to the remaining elements as a single list:

@chunk[<rest-args>
       (define (rest-args a b . cs)
         (list a b cs))
       (check-equal? (rest-args 1 2 3 4 5)
                     (list 1 2 (list 3 4 5)))]

Unfortunately @racket[let-values] does not allow this idea when
specifying a co-function. Let's fix that (and other problems) with a
new form called @racket[let-values+].

@chunk[<rest-vals>
       (check-equal?
        (let-values+ ([(a b . cs) (values 1 2 3 4 5)])
          (list a b cs))
        (list 1 2 (list 3 4 5)))]

In addition to this built-in behavior of Racket functions, most Racket
functions aren't really what the Racket VM calls
@racket[#%plain-lambda]s but are instead @racket[lambda]s that
implement optional arguments and keyword arguments. (Similarly, most
applications are @racket[#%app] (and not @racket[#%plain-app]) which
implements the application side of keyword arguments.)

A optional argument function looks like:

@chunk[<opt-args>
       (define (opt-args a b [c 3])
         (list a b c))
       (check-equal? (opt-args 1 2) 
                     (list 1 2 3))
       (check-equal? (opt-args 1 2 4)
                     (list 1 2 4))]

And we would like to support optional co-arguments as well:

@chunk[<opt-vals>
       (check-equal? (let-values+ ([(a b [c 3]) (values 1 2)])
                       (list a b c))
                     (list 1 2 3))
       (check-equal? (let-values+ ([(a b [c 3]) (values 1 2 4)])
                       (list a b c))
                     (list 1 2 4))]

A keyword argument function looks like:

@chunk[<key-args>
       (define (key-args #:a a #:b b)
         (list a b))
       (check-equal? (key-args #:a 1 #:b 2)
                     (list 1 2))]

And we would like to support keyword co-arguments:

@chunk[<key-vals>
       (check-equal? (let-values+ ([(#:a a #:b b) (values+ #:a 1 #:b 2)])
                       (list a b))
                     (list 1 2))]

And, of course, everything should be usable at the same time in
co-functions:

@chunk[<all-vals>
       (check-equal? (let-values+ ([(a [b 2] #:c c #:d [d 3] . more)
                                    (values+ 1 2 #:c 3 #:d 4 5 6)])
                       (list a b c d more))
                     (list 1 2 3 4 (list 5 6)))
       (check-equal? (let-values+ ([(a [b 2] #:c c #:d [d 3] . more)
                                    (values+ 1 #:c 3)])
                       (list a b c d more))
                     (list 1 2 3 3 (list)))]

Just like in normal functions:

@chunk[<all-args>
       (define (all-args a [b 2] #:c c #:d [d 3] . more)
         (list a b c d more))
       (check-equal? (all-args 1 2 #:c 3 #:d 4 5 6)
                     (list 1 2 3 4 (list 5 6)))
       (check-equal? (all-args 1 #:c 3)
                     (list 1 2 3 3 (list)))]

@section{Implementing @racket[values+]}

Given this understand of what @racket[values+] and
@racket[let-values+] should do, let's implement them.

The first thing is to realize that it all comes down to
@racket[call-with-values+] and a technique for @racket[values+] to
communicate with it. The only thing that is complicated is getting the
keyword arguments that @racket[values+] may be called with. If you
want to write a function that accepts a strange configuration of
keyword arguments, you need to use @racket[make-keyword-procedure]. 

@racket[make-keyword-procedure] takes two arguments: one to be called
when there are keywords (the slow path) and one when there are no
keywords (the fast path). The slow path is given a list of keywords, a
list of keyword arguments, and then the normal arguments. Our trick
will be to capture these three lists into an arity three multiple
return values that @racket[call-with-values+] just delivers (via
@racket[keyword-apply]) to the consumer.

Unfortunately, @racket[call-with-values+] needs to distinguish the
slow path and the fast path. We'll do that with a hidden value that
only the two of them share.

@chunk[<core-hidden>
       (define value+-code (gensym))

       (define values+
         (make-keyword-procedure
          (lambda (kws kw-args . rest)
            (values value+-code kws kw-args rest))
          values))

       (define (call-with-values+ producer consumer)
         (call-with-values producer
           (case-lambda
             [(maybe-key kws kw-args rest)
              (if (eq? value+-code maybe-key)
                (keyword-apply consumer kws kw-args rest)
                (consumer maybe-key kws kw-args rest))]
             [args
              (apply consumer args)])))]

The way the hiding will work is with a @racket[racket/package] that
just exports the functions:

@chunk[<core>
       (define-package values+-pkg (call-with-values+ values+)
         <core-hidden>)
       (open-package values+-pkg)]

Once this is in place, the first macro is trivial:

@chunk[<let-values+/one>
       (define-syntax-rule (let-values+/one ([formals expr]) body)
         (call-with-values+ 
          (lambda () expr) 
          (lambda formals body)))]

But it is complicated to extend it to many binding clauses, because
each binding would be nested inside the @racket[_body] of the
@racket[call-with-values+] consumer. That behavior is more like
@racket[let*-values]. So, we'll implement it first:

@chunk[<let*-values+>
       (define-syntax let*-values+
         (syntax-rules ()
           [(_ () body)
            body]
           [(_ ([formals0 expr0] [formals1 expr1] ...) body)
            (let-values+/one ([formals0 expr0])
             (let*-values+ ([formals1 expr1] ...) body))]))]

In order to term @racket[let*-values+] into @racket[let-values+], we
need to ensure that the bindings introduced in one clause aren't
available in the next. In other words,

@chunk[<let-values+-isnt-let*-values+>
       (check-equal?
        (let ([x 1] [y 2])
          (let-values+ ([(x) (+ x y)]
                        [(y) (+ x y)])
           (list x y)))
        (list 3 3))

       (check-equal?
        (let ([x 1] [y 2])
          (let*-values+ ([(x) (+ x y)]
                         [(y) (+ x y)])
           (list x y)))
        (list 3 5))]

One way to do that is to introduce temporary identifiers that will be
rewritten in the body:

@chunk[<let-values+>
       (define-syntax (let-values+ stx)
         (syntax-case stx ()
           [(_ () body)
            (syntax/loc stx
              body)]
           [(_ ([formals0 expr0] [formals1 expr1] ...) body)
            (with-syntax
                ([(new-formals0 (formals0i ...) (new-formals0i ...))
                  (generate-temporaries/formals #'formals0)])
              (syntax/loc stx
                (let-values+/one ([new-formals0 expr0])
                 (let-values+ ([formals1 expr1] ...)
                  (let-syntax ([formals0i (make-rename-transformer #'new-formals0i)]
                               ...)
                    body)))))]))]

But, generating such identifiers is complicated and requires deeply
understanding the syntax of @racket[lambda]:

@chunk[<ugly>
       (begin-for-syntax 
         (define (generate-temporaries/formals formals)
           (let loop ([formals formals])
             (syntax-case formals ()
               [()
                (list empty empty empty)]
               [(id . m)
                (identifier? #'id)
                (let ()
                  (match-define (list nm mis nmis) (loop #'m))
                  (match-define (list new-id) (generate-temporaries #'(id)))
                  (list (cons new-id nm)
                        (cons #'id mis)
                        (cons new-id nmis)))]
               [((id def) . m)
                (identifier? #'id)
                (let ()
                  (match-define (list nm mis nmis) (loop #'m))
                  (match-define (list new-id) (generate-temporaries #'(id)))
                  (list (cons (list new-id #'def) nm)
                        (cons #'id mis)
                        (cons new-id nmis)))]
               [(key id . m)
                (and (keyword? (syntax-e #'key)) (identifier? #'id))
                (let ()
                  (match-define (list nm mis nmis) (loop #'m))
                  (match-define (list new-id) (generate-temporaries #'(id)))
                  (list (list* #'key new-id nm)
                        (cons #'id mis)
                        (cons new-id nmis)))]
               [(key (id def) . m)
                (and (keyword? (syntax-e #'key)) (identifier? #'id))
                (let ()
                  (match-define (list nm mis nmis) (loop #'m))
                  (match-define (list new-id) (generate-temporaries #'(id)))
                  (list (list* #'key (list new-id #'def) nm)
                        (cons #'id mis)
                        (cons new-id nmis)))]
               [id
                (identifier? #'id)
                (let ()
                  (match-define (list new-id) (generate-temporaries #'(id)))
                  (list new-id
                        (list #'id)
                        (list new-id)))]))))]

I find this code very ugly and think it has way too much boiler-plate,
but it seems necessary to me. Aside from this piece, I think this is a
really beautiful set of macros that does a nice thing.

@section{Yo! It's almost time to go!}

But first let's remember what we learned today!

Symmetry is beautiful.

Racket provides a lot of symmetry between function calls and returns.

Multiple return values are a natural consequence of first-class
continuations.

Racket macros give us an elegant way to add even more symmetry between
function calls and returns.

If you'd like to run this exact code at home, you should put it in
this order:

@chunk[<*>
       (require (for-syntax racket/base
                            racket/list
                            racket/match)
                racket/package
                rackunit)

       <basic-funs>
       <basic-vals>
       <error-ex1>
       <error-ex2>
       <basic-vals/k>
       <basic-conts>
       <basic-conts/call>
       
       <core>
       <let-values+/one>
       <let*-values+>

       <ugly>

       <let-values+>

       <let-values+-isnt-let*-values+>

       <rest-args>
       <rest-vals>
       <opt-args>
       <opt-vals>
       <key-args>
       <key-vals>
       <all-args>
       <all-vals>]

This post was inspired by
@link["http://www.mail-archive.com/users@racket-lang.org/msg18552.html"]{a
question on the Racket mailing list}.

@(the-end)
