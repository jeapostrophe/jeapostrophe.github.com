#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/unit
                     racket/match
                     racket/list
                     data/heap)
          "../post.rkt")

@title{Syntax Parameters and a Threading Macro}
@categories["Racket" "Clojure" "Macros"]

Greg Hendershott recently
@link["http://www.greghendershott.com/2013/05/the-threading-macro.html"]{wrote
a blog post} about a "threading" macro for Racket. This post presents
a possible "improvement" to that macro and uses it as a way to explain
the use of @tech{syntax parameters} in Racket.

@(the-jump)

@section{Syntax Parameters}

A classic macro is the so-called "anaphoric if", where you can refer
to the value of the condition in the true branch. It is typically used
as:

@chunk[<aif-ex>
       (define ht (hasheq 'key 7))
       (define (f k)
         (aif (hash-ref ht k #f)
              (+ it 5)
              0))
       (check-equal? (f 'key) 12)
       (check-equal? (f 'other-key) 0)]

It is "classic" because it non-hygienically introduces a binding for
@racket[it]. The normal definition is:

@chunk[<classic-aif>
       (define-syntax (aif stx)
         (syntax-parse stx
           [(_ c:expr t:expr f:expr)
            (with-syntax
                ([it (datum->syntax #'t 'it)])
              (syntax/loc stx
                (let ([it c])
                  (if it t f))))]))]

This definition is problematic. Since it breaks hygiene, there is no
way for the user to have access to the @racket[it] binding from the
user program inside the @racket[aif] macro. For example, this use
returns @racket[#f]:

@chunk[<aif-bad-ex>
       (check-false
        (let ([it 5])
          (aif 6
               (= it 5)
               #f)))]

In normal libraries, it is possible to rename bindings to ensure that
they won't conflict with your bindings or other libraries. For
example, you could use @racket[prefix-in] when you require the
library. The @racket[it] introduced by @racket[aif] is not actually a
binding, so these techniques fail. Similarly, if we try to use
@racket[aif]'s @racket[it] outside an @racket[aif], then we just get
a "Unbounded identifier" error and not one that references
@racket[aif].

@tech{Syntax parameters} solve all these problems. We can rewrite the
macro as:

@chunk[<modern-aif>
       (define-syntax-parameter it
         (λ (stx) 
           (raise-syntax-error 
            'it 
            "Only allowed inside true branch of aif" 
            stx)))
       (define-syntax (aif stx)
         (syntax-parse stx
           [(_ c:expr t:expr f:expr)
            (syntax/loc stx
                (let ([this-it c])
                  (if this-it 
                    (syntax-parameterize 
                        ([it (make-rename-transformer #'this-it)])
                      t)
                    f)))]))]

Syntax parameters are like @tech{parameters} (which we've discussed
before in @postref{2012-07-25-cont-mar}), but rather than affecting
the dynamic (run-time) behavior of their body, they affect the
static (expansion-time) behavior.

In this case, we change the @racket[it] make to be a @tech{rename
transformer} (a macro that just rewrites all references from it to its
argument, in this case the newly bound @racket[this-it]). In addition,
we specify a default value of the parameter which gives a nice error
message about where you allowed to use @racket[it].

Furthermore, we specifically only parameterize the true branch,
because it's the only branch where the value of @racket[it] is
useful. There would be no easy way to do this without syntax
parameters.

Since it is now a binding, whenever our user uses it, they
automatically shadow the macro's binding and preserve what they think
the value should be. But, since it is also a binding, the user can
explicitly rename it to something else, either in their program (as
below) or with @racket[prefix-in]. This example demonstrates:

@chunk[<aif-good-ex>
       (check-true
        (let ([it 5])
          (aif 6
               (= it 5)
               #f)))

       (check-false   
        (let-syntax ([old-it (make-rename-transformer #'it)])
          (let ([it 5])
            (aif 6
                 (= old-it 5)
                 #f))))]

Now, this is the "traditional" use syntax parameters. But, they don't
have to be bound to syntax transformers. They can be bound to
arbitrary data. That's how we'll make a new threading macro.

@section{Threading Macro}

The goal of a threading macro is to remove nesting and replace it with
sequencing. For example,

@chunk[<tm-ex1>
       (check-equal?
        (number->string (- (add1 5)))
        (~> 5
            add1
            -
            number->string))]

This is similar to @postlink["2013-05-20-forth"]{Forth style},
actually, because it emphasizes the implicit flow of values, rather
than the structure of the computation.

A threading macro that only works on unary functions seems pretty
trivial:

@chunk[<unary-tm>
       (define-syntax ~>
         (syntax-rules ()
           [(_ val)
            val]
           [(_ val fun more ...)
            (~> (fun val) more ...)]))]

But a more interesting version would allow you call multi-arity
functions where you've specified just a prefix of the arguments. For
example:

@chunk[<tm-ex2>
       (check-equal?
        (number->string (- 6 (+ 1 5)))
        (~> 5
            (+ 1)
            (- 6)
            number->string))]

The unary threading macro fails, because it produces
@racket[(number->string ((- 6) ((+ 1) 5)))], which is an error,
because @racket[-6] isn't a function that you can call.

@section{Improved Threading Macro}

We can improve this macro by noticing that some arguments have been
given and expanding to the right kind of application:

@chunk[<better-tm>
       (define-syntax appish
         (syntax-rules ()
           [(_ (fun arg0 ...) argN)
            (fun arg0 ... argN)]
           [(_ fun arg0)
            (fun arg0)]))
       (define-syntax ~>
         (syntax-rules ()
           [(_ val)
            val]
           [(_ val fun more ...)
            (~> (appish fun val) more ...)]))]

But, what if the nesting isn't in the last argument position, such as
with @racket[(number->string (- (+ 1 5) 6))]? Obviously there is a
dual to @racket[appish] that puts the new argument in the first
position rather than the last, but clearly that is brittle.

It would be better if we could specify where the nesting occurs. I
also prefer not having a default place where nesting occurs, so that
we can use expressions like @racket[(curry map add1)] and
@racket[(λ (x) x)] in threading. For example:

@chunk[<tm-ex3>
       (check-equal?
        (number->string (+ 5 (- (+ 1 5) 6)))
        (~> 5
            (+ 1 <>)
            (- <> 6)
            (curry + 5)
            number->string))]

@section{Improved^2 Threading Macro}

The key will be to make @racket[<>] a syntax parameter that records
the last thing in the threading macro and then insert @racket[<>] if
one isn't there already.

@chunk[<betterer-tm>
       (define-syntax (ensure-<> stx)
         (syntax-case stx ()
           [(_ e)
            (if (contains-<>? #'e)
              (syntax/loc stx e)
              (syntax/loc stx (e <>)))]))
       (define-syntax ~>
         (syntax-rules ()
           [(_ val)
            val]
           [(_ val fun more ...)
            (let ([new-val val])
              (~> (syntax-parameterize 
                      ([<> (make-rename-transformer #'new-val)])
                    (ensure-<> fun))
                  more ...))]))]

However, it is quite complicated to implement
@racket[contains-<>?]. One approach would be to search through the
syntax for a use of @racket[<>], but that would hide uses behind
macros and find uses of nested @racket[~>] forms. Instead, we'll
locally expand the syntax after rebinding @racket[<>] back to the
original binding. Then, we ensure that when @racket[<>] is used
outside @racket[~>] it throws a unique error message that we can catch
to tell that there was one there.

@chunk[<betterer-support>       
       (begin-for-syntax
         (struct exn:fail:syntax:<> exn:fail:syntax ())
         (define default-<>
           (λ (stx) 
             (raise (exn:fail:syntax:<> "<>: Only allowed inside ~>:"
                                        (current-continuation-marks)
                                        (list stx))))))
       (define-syntax-parameter <> default-<>)
       (begin-for-syntax
         (define (contains-<>? stx)
           (with-handlers ([exn:fail:syntax:<>? (λ (x) #t)]
                           [(λ (x) #t) (λ (x) #f)])
             (local-expand (with-syntax ([body stx])
                             (syntax
                               (syntax-parameterize
                                   ([<> default-<>])
                                 body)))
                           'expression empty)
             #f)))]

This is a neat macro, but what if we want to be able to refer to more
than just one of the threaded positions? We could change @racket[<>],
so that it takes an optional argument which names the value (backwards
in the stack) that it needs. For example:

@chunk[<tm-ex4>
       (check-equal?
        (~> 1
            (+ <>)
            (+ <> (<> 1))
            (+ <> (<> 1) (<> 2)))
        (let* ([s0 1]
               [s1 (+ s0)]
               [s2 (+ s1 s0)]
               [s3 (+ s2 s1 s0)])
          s3))]

@section{Improved^3 Threading Macro}

One way to do this would be to compile to something like Forth where
we have an explicit data stack recording what values have been
computed. There's a few things distasteful about that. First, it would
be expensive to store this stack and second, it would mean that we
might accidentally refer to values too far back in the stack and get
dynamic errors. It would be nice to catch those statically.

We'll do this by having @racket[<>] always be the same macro, but use
a different syntax parameter to name the @emph{identifiers} that refer
to earlier values. If these identifiers are not referenced, then
Racket's safe-for-space guarantees will garbage collect them, so they
won't be expensive to store. Furthermore, when they are referenced,
they will be used directly without any intermediate data structure
access.

The core is almost exactly the same as before, except that we don't
use a @racket[make-rename-transformer]. Instead, we extend
@racket[<>s] to have one more identifier (using
@racket[syntax-parameter-value] to read the old value). A more subtle
change is that the @racket[syntax-parameterize] has to be around the
entire recursive @racket[~>] so that the @racket[more ...] piece can
view this identifier.

@chunk[<best-tm-core>
       (define-syntax-parameter <>s empty)
       (define-syntax (~> stx)
         (syntax-parse stx
           [(_ last:expr)
            (syntax/loc stx
              last)]
           [(_ rand:expr rator:expr more:expr ...)
            (syntax/loc stx
              (let ([rand-v rand])
                (syntax-parameterize
                    ([<>s (list* #'rand-v (syntax-parameter-value #'<>s))])
                  (~> (ensure-<> rator) more ...))))]))]

The macro to ensure that @racket[<>] is present is @emph{exactly} the
same:

@chunk[<best-tm-ensure>
       (define-syntax (ensure-<> stx)
         (syntax-parse stx
           [(_ e:expr)
            (if (contains-<>? #'e)
              (syntax/loc stx e)
              (syntax/loc stx (e <>)))]))]

@racket[contains-<>?] is very close to what it was before, but now it
resets @racket[<>s] back to the empty list:

@chunk[<best-tm-contains>
       (begin-for-syntax
         (struct exn:fail:syntax:<> exn:fail:syntax ())
         (define (contains-<>? stx)
           (with-handlers ([exn:fail:syntax:<>? (λ (x) #t)]
                           [(λ (x) #t) (λ (x) #f)])
             (local-expand (with-syntax ([body stx])
                             (syntax
                               (syntax-parameterize ([<>s empty]) body)))
                           'expression empty)
             #f)))]

The most interesting piece is the @racket[<>] macro. If the macro is
used by itself, then expand to a use of @racket[(<> 0)]. Otherwise,
check if @racket[<>s] is as long as the static number. If it is, then
expand to a use of that identifier. Otherwise error, with our special
error structure:

@chunk[<best-tm-<>>
       (define-syntax (<> stx)
         (syntax-parse stx
           [_:id
            (syntax/loc stx
              (<> 0))]
           [(_ idx:nat)
            (define l (syntax-parameter-value #'<>s))
            (define idx-v (syntax->datum #'idx))
            (if (> (length l) idx-v)
              (list-ref l idx-v)
              (if (empty? l)
                (raise (exn:fail:syntax:<> "<>: Only allowed inside ~>:"
                                           (current-continuation-marks)
                                           (list stx)))
                (raise-syntax-error '<>
                                    (format "~e is too large" idx-v)
                                    stx)))]))]

This version allows some exciting code like this:

@chunk[<tm-ex5>
       (check-equal?
        (~> (~> (list 1 2 3)
                (map add1 <>))
            (~> <>
                (map add1 <>)
                (map + (<> 2) <>))
            list->vector)

        (list->vector
         (let ([s2 (map add1 (list 1 2 3))])
           (let ([s1 (map add1 s2)])
             (map + s2 s1)))))]

But I don't recommend using many nested @racket[~>]s or a large number
of @racket[<>] uses with many indexes.

@section{Yo! It's almost time to go!}

But first let's remember what we did today!

@tech{Syntax parameters} are great for having hygiene-breaking-like
effects in a safe way and creating context-sensitive macros that
communicate from outside (@racket[~>]) to inside (@racket[<>]).

You might want to read
@link["http://www.schemeworkshop.org/2011/papers/Barzilay2011.pdf"]{Keeping
it Clean with Syntax Parameters} by Eli Barzilay, Ryan Culpepper, and
Matthew Flatt.

The
@link["https://github.com/jeapostrophe/exp/blob/master/threading-arrow.rkt"]{combined
version} is just 55 lines, with about a hundred lines of interesting
tests. But if you'd like to run this exact code at home, you should
put it in this order:

@chunk[<*>
       (require rackunit
                racket/stxparam
                racket/function
                (for-syntax syntax/parse
                            racket/base
                            racket/list))

       (let ()
         <classic-aif>
         <aif-ex>
         <aif-bad-ex>)

       (let ()
         <modern-aif>
         <aif-ex>
         <aif-good-ex>)

       (let ()
         <unary-tm>
         <tm-ex1>)

       (let ()
         <better-tm>
         <tm-ex1>
         <tm-ex2>)

       #;#;
       <betterer-support>
       (let ()         
         <betterer-tm>
         <tm-ex3>)

       <best-tm-contains>
       <best-tm-<>>       
       <best-tm-ensure>
       <best-tm-core>
       <tm-ex3>
       <tm-ex4>
       <tm-ex5>]

@(the-end)
