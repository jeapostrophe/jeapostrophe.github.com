#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/unit
                     racket/match
                     racket/list
                     data/heap)
          "../post.rkt")

@title{The Underbelly of Racket Macro Basics}
@categories["Racket" "Macros"]

Racket macros are a beautiful and simple way to extend the programming
language, but even the simplest macros have many deep things going on
to make them work. This post attempts to take a trivial macro down to
the "bare metal" of the Racket VM to show what's going on. Hopefully,
by tracing such a simple macro, it will help you better understand how
to use low-level tools, if you ever need them.

@(the-jump)

Our simple macro has a stupid purpose: it creates a list that
references three different identifiers, each at different places in
the context of the macro invocation, but that are all written
identically. Here's a test:

@chunk[<test>
       (define x 1)
       (let ([x 2])
         (check-equal? (123-list x) (list 1 2 3)))]

The idea is that @racket[123-list] will create a new binding for
@racket[x] and expand to @racket[(list x x x)], where the first
@racket[x] is the @racket[define]d @racket[x], the second is to the
@racket[let]-bound @racket[x], and the final @racket[x] is introduced
by the macro, with the value of @racket[3].

A non-working version of this macro would be:

@chunk[<broken>
       (define-syntax-rule (123-list x)
         (let ([x 3]) 
           (list x x x)))]

This version is broken because the @racket[let] binding captures all
three references. It returns @racket[(list 3 3 3)].

A better version is:

@chunk[<better>
       (define-syntax-rule (123-list middle-x)
         (let ([inner-x 3])
           (list x middle-x inner-x)))]

But, this version is a little bit disingenuous because the macro is
pre-programmed to know about the outer binding of @racket[x]. This
means that it fails on:

@chunk[<test-y>
       (define y 1)
       (let ([y 2])
         (check-equal? (123-list y) (list 1 2 3)))]

But, we won't fix this problem until the very end. First, let's
unravel the sugar that @racket[define-syntax-rule] gives us. All
@racket[define-syntax-rule] does is abbreviate the definition of a
value that uses @racket[syntax-rules] with one pattern.

@chunk[<better2>
       (define-syntax 123-list
         (syntax-rules ()
           [(_ middle-x)
            (let ([inner-x 3])
              (list x middle-x inner-x))]))]

The expansion from @racket[syntax-rules] is similarly simple. It
expands into a single argument function, uses @racket[syntax-case] to
match the pattern, and then returns a piece of syntax with the same
source location as the argument.

@chunk[<better3>
       (define-syntax (123-list stx)
         (syntax-case stx ()
           [(_ middle-x)
            (syntax/loc stx
              (let ([inner-x 3])
                (list x middle-x inner-x)))]))]

While @racket[syntax-case] is a powerful macro that does full-fledged
pattern matching, we do a trivial thing with it: extract the second
element of an input list. You might think we could replace
@racket[syntax-case] with:

@chunk[<better4-wrong>
       (define-syntax (123-list stx)
         (define middle-x (second (syntax->list stx)))
         (syntax/loc stx
           (let ([inner-x 3])
             (list x middle-x inner-x))))]

But this is incorrect, because the @racket[middle-x] in the macro
template is a pattern variable and not a normal binding. Luckily, we
can fix this by using @racket[quasisyntax/loc] rather than
@racket[syntax/loc]:

@CHUNK[<better4>
       (define-syntax (123-list stx)
         (define middle-x-stx (second (syntax->list stx)))
         (quasisyntax/loc stx
           (let ([inner-x 3])
             (list x #,middle-x-stx inner-x))))]

The next thing to attack is @racket[quasisyntax/loc], which is like
@racket[quasiquote] but constructs syntax objects rather than
lists. Thus, we can just use @racket[datum->syntax] to turn the
@racket[quasiquote] constructed list into a syntax object with the
same context as some other syntax object:

@chunk[<better5>
       (define-syntax (123-list stx)
         (define middle-x-stx (second (syntax->list stx)))
         (datum->syntax
          #'123-list
          `(let ([inner-x 3])
             (list x ,middle-x-stx inner-x))))]

However, it is not really correct to use the lexical context of
@racket[#'123-list] for the entire new syntax object. Instead, we just
need that context for stuff like @racket[let], @racket[#%app],
@racket[list], and @racket[x]. Normally, each macro invocation has its
own fresh context, which is made with @racket[#f]:

@chunk[<better6>
       (define-syntax (123-list stx)
         (define middle-x-stx (second (syntax->list stx)))
         (define new-ctxt-stx (datum->syntax #f 'new-ctxt))
         (datum->syntax
          new-ctxt-stx
          (list (datum->syntax #'123-list 'let)
                (list (list 'inner-x (datum->syntax #'123-list 3)))
                (list (datum->syntax #'123-list '#%app)
                      (datum->syntax #'123-list 'list)
                      (datum->syntax #'123-list 'x)
                      middle-x-stx
                      'inner-x))))]

At this point, we can go back and fix @racket[<test-y>] by using
@racket[middle-x-stx] to discover the name the macro user wants to
use.

@chunk[<better7>
       (define-syntax (123-list stx)
         (define middle-x-stx (second (syntax->list stx)))
         (define x-id (syntax-e middle-x-stx))
         (define new-ctxt-stx (datum->syntax #f 'new-ctxt))
         (datum->syntax
          new-ctxt-stx
          (list (datum->syntax #'123-list 'let)
                (list (list x-id (datum->syntax #'123-list 3)))
                (list (datum->syntax #'123-list '#%app)
                      (datum->syntax #'123-list 'list)
                      (datum->syntax #'123-list x-id)
                      middle-x-stx
                      x-id))))]

There's basically no lower than we can really go with this macro.

@section{Yo! It's almost time to go!}

But first let's remember what we learned today!

At their base, Racket macros really are functions from a syntax object
to a new syntax object.

A syntax object is an S-expression of data paired with a lexical
context.

Different identifiers that are typed the same way are differentiated
by their lexical context objects.

When @racket[datum->syntax] attaches lexical context to raw data, it
does not replace lexical context that is already there.

If you'd like to run this exact code at home, you should put it in
this order:

@CHUNK[<*>
       (require (for-syntax racket/base
                            racket/list)
                rackunit)
       
       (let ()
         <broken>
         <test>)

       (let ()
         <better>
         <test>)

       (let ()
         <better>
         (define x 0)
         <test-y>)

       (let ()
         <better2>
         <test>)

       (let ()
         <better3>
         <test>)

       (let ()
         <better4>
         <test>)

       (let ()
         <better5>
         <test>)

       (let ()
         <better6>
         <test>)

       (let ()
         <better7>
         <test>
         <test-y>)]

@(the-end)
