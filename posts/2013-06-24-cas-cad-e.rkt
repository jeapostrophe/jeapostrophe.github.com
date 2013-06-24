#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/unit
                     racket/match
                     racket/list
                     data/heap)
          "../post.rkt")

@title{Cascading Switches in Racket}
@categories["Racket" "Macros"]

A few years ago, Shriram Krishnamurthi sent a
@link["http://www.mail-archive.com/users@racket-lang.org/msg02170.html"]{message}
to the Racket user mailing list about a little macro problem. The goal
was to create a switch with "fall-through", like in C and some other
languages. I enjoyed my solution, so I will present it here.

@(the-jump)

Here's the specification of the problem:

Define a @racket[case] construct syntactically just like that of
Racket. In terms of semantics:
@itemlist[

@item{each branch automatically falls through to the next,}

@item{the last one returns its answer since it has no next clause, and}

@item{any branch can contain @racket[(break <expr>)], which evaluates
@racket[<expr>] and returns its value as that of the entire case.}

]

For instance, if we have three cases where the second breaks:

@chunk[<example>
       (define printed "")
       (define (cas v)
         (set! printed "")
         (cas-cad-e 
          v
          [(1)
           (set! printed (string-append printed "1"))]
          [(2) 
           (set! printed (string-append printed "2"))
           (break 2)]
          [(3)
           3]))]

Then, we can observe the effect of the first in the second, but when
we start on the third case, then the previous cases are invisible:

@chunk[<test>      
       (check-equal? (cas 1) 2)
       (check-equal? printed "12")

       (check-equal? (cas 2) 2)
       (check-equal? printed "2")

       (check-equal? (cas 3) 3)
       (check-equal? printed "")

       (check-equal? (cas 4) (void))
       (check-equal? printed "")]

We're going to generate code like:

@chunk[<expanded>
       (define printed "")
       (define (cas v)
         (set! printed "")
         (let/ec escape
           (let* ([third-case
                   (λ ()
                     3)]
                  [second-case
                   (λ ()
                     (set! printed (string-append printed "2"))
                     (escape 2)
                     (third-case))]
                  [first-case 
                   (λ ()
                     (set! printed (string-append printed "1"))
                     (second-case))])
             (case v
               [(1) (first-case)]
               [(2) (second-case)]
               [(3) (third-case)]))))]

The idea behind this code generation is that:
@itemlist[

@item{Breaking is handled with an escape continuation that
@racket[break] is rewritten to.}

@item{The case logic is handled exactly by the existing @racket[case]
macro.}

@item{Each case is turned into one function, because we have delayed
code evaluation.}

@item{Each case explicitly calls the next case.}

@item{The case functions are defined in the reverse order from the way
they are defined, facilitating the use of @racket[let*] rather than
@racket[letrec].}

]

The basic structure of this macro is simple, given the example
expansion and modulo @postlink["2013-05-27-stxparam"]{the classic use
of syntax parameters}:

@chunk[<cas-cad-e>
       (define-syntax-parameter break
         (λ (stx) (raise-syntax-error 'break "Used outside cas-cad-e" stx)))
       
       (define-syntax cas-cad-e
         (syntax-parser
          [(_ e:expr [opt body:expr ...+] ...)
           (with-syntax*
            ([(forward-id ...)         <forward-id>]
             [(reverse-id ...)         <reverse-id>]
             [((reverse-body ...) ...) <reverse-body>]
             [((next-id ...) ...)      <next-id>])
            #'(let/ec escape
                (syntax-parameterize ([break (make-rename-transformer #'escape)])
                  (let* ([reverse-id (lambda () reverse-body ... (next-id) ...)] ...)
                    (case e [opt (forward-id)] ...)))))]))]

There's a convenient function to generate a list of fresh identifiers
for each of a list of syntaxes called @racket[generate-temporaries]:

@chunk[<forward-id>
       (generate-temporaries #'(opt ...))]

But, since we need to put them into the output in the opposite order,
we reverse that list:

@chunk[<reverse-id>
       (reverse (syntax->list #'(forward-id ...)))]

Similarly, we must reverse the list of function bodies (not reverse
their bodies, but just reverse the order the bodies are seen in):

@chunk[<reverse-body>
       (reverse (syntax->list #'((body ...) ...)))]

Finally, the most complicated part is finding out the next function to
call. We take the identifiers in the forward order, but drop the first
one, because we don't want to create a lot of infinite loops. This
creates the problem of the last case not having a next, so we add one
at the end. Rather than adding @racket[void] as the thing to call, we
instead have each of the next ids be a list, so the last one is just
empty.

@chunk[<next-id>
       (reverse (cdr (syntax->list #'((forward-id) ... ()))))]

And that's it!

I like this macro because the expansion is so beautiful: it doesn't
use complicated features like mutation or recursion and is thus easy
to parse and understand.

@section{Yo! It's almost time to go!}

But first let's remember what we learned today!

Everything you miss about C can be recovered in Racket with a few
short macros, including the bizarre behavior of @code{switch}.

If you'd like to run this exact code at home, you should put it in
this order:

@chunk[<*>
       (require (for-syntax racket/base
                            syntax/parse 
                            racket/syntax)
                rackunit
                racket/stxparam)
       
       (let ()
         <expanded>
         <test>)

       <cas-cad-e>

       (let ()
         <example>
         <test>)]

@(the-end)
