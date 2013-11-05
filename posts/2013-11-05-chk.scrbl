#lang scribble/manual
@(require (for-label racket/base
                     rackunit
                     rackunit/chk
                     syntax/parse
                     syntax/parse/define)
          "../post.rkt")

@title{Implementing a Short Hand for Rackunit Tests}
@categories["Racket" "Macros"]

I recently released the
@link["http://pkgs.racket-lang.org/#[rackunit-chk]"]{@code{rackunit-chk}}
package to simplifying writing @racketmodname[rackunit] tests. In this
post, I write about how it works.

@(the-jump)

I have been thoroughly brainwashed into believing that writing test
for software is extremely important. @racketmodname[rackunit] is a
pretty typical unit-testing framework. For instance, suppose that we
have a function that does a basic math calculation:

@racketblock[
 (define (f x y)
   (+ x (/ 1 y)))
]

You can write tests for this by writing things like:

@racketblock[
 (check-equal? (f 1 1) 2)
 (check-equal? (f 1 1/10) 11)
]

I find that the vast majority of the time all my tests are calls to
@racket[check-equal?]. Of course, there are other kinds of tests you
can write too. For instance, suppose that you want to test if a
boolean expression evaluates to false:

@racketblock[
 (check-false (and #t (or #f #f) #t))
]

This is slightly more convenient than using @racket[check-equal?] with
@racket[#f] as one side. An even more convenient thing is when you
want to see if a boolean expression isn't false:

@racketblock[
 (check-not-false (and 1 (or #f 2)))             
]

It's nice to not have to specify what the actual answer is by just
using @racket[check-not-false].

Despite these few variants of @racket[check-equal?], Rackunit starts
to feel really verbose for me when I have many many
@racket[check-equal?] tests in sequence.

Another place where it feels verbose is with functions that return
multiple values. For instance, @racket[quotient/remainder] returns two
values. If you wanted to test it, here are some different ways:

@racketblock[
 (let-values ([(x y) (quotient/remainder 10 3)])
   (check-equal? x 3)
   (check-equal? y 1))

 (call-with-values (λ () (quotient/remainder 10 3))
   (λ vs
     (check-equal? vs (list 3 1))))
]

I find both of these to be extremely awkward. Of course, because it's
Racket we can take either approach and make it a macro.

Finally, dealing with errors can be pretty painful in Rackunit. For
instance, if you write the test a test where either the actual or
expected results throws an error, then the entire test suite dies. You
can't even protect against this by using
@racket[check-not-equal?]. Each one of these expression will crash the
whole test suite:

@racketblock[
 (check-equal? (f 1 0) 11)
 (check-equal? (f 1 2) (/ 1 0))
 (check-not-equal? (f 1 0) 11)
]

It is very awkward to work around this problem. 

Similarly, if you want to make sure there is an error, then it is
fairly verbose, because you are required to thunk the expression that
errors. You have to write a predicate that determines if the thrown
exception is the "right" one, although there is a built-in short-cut
for matching the message against a regular expression.

@racketblock[
 (check-exn exn:fail?
            (λ () (f 1 0)))
 (check-exn (λ (x) (regexp-match #rx"di.ision" (exn-message x)))
            (λ () (f 1 0)))
 (check-exn #rx"division"
            (λ () (f 1 0)))
]

I've written @racketmodname[rackunit/chk] to deal with all these
things and simplify writing lots of tests. It is inspired by
@racketmodname[tests/eli-tester], but it has a different syntax and
compiles to Rackunit, rather than its own testing system.

The macro is called @racket[chk] and supports all the uses case from
the prior examples as follows:

@racketblock[
 (chk (f 1 1) 2
      (f 1 1/10) 11

      #:f 1 2
      #:f #:f 1 1

      #:f #:t (and #t (or #f #f) #t)

      #:t (and 1 (or #f 2))
      
      (quotient/remainder 10 3) (values 3 1)
      
      #:f (f 1 0) 11
      #:f (f 1 2) (/ 1 0)
      #:exn (f 1 0) exn:fail?
      #:exn (f 1 0) #rx"di.ision"
      #:exn (f 1 0) "division"
      (f 1 0) (error '/ "division by zero"))
]

The @racket[chk] macro accepts any number of tests, without additional
parenthesis grouping them. If there are two elements, then it is
implicitly a @racket[check-equal?]-like test, but with protection from
errors, support for multiple values, and (as the last example shows)
it can ensure that two erroneous expression have the same error. You
can negate the meaning of any test with @racket[#:f]. You can test
that a single expression is true-like with @racket[#:t]. Finally, you
can explicitly check for a certain kind of exception with
@racket[#:exn].

Most of the
@link["https://github.com/jeapostrophe/rackunit-chk/blob/master/rackunit/chk.rkt"]{implementation}
of this is very simple, but the coolest part is the way I use a
@racket[define-splicing-syntax-class] to specify the syntax of
tests. Each different variant of a test provides two attributes:
@racket[unit] and @racket[fail-unit]. This allows @racket[#:f] to have
a simple implementation that just switches the two of them:

@racketblock[
[pattern (~seq #:f t:test)
         #:attr unit #'t.fail-unit
         #:attr fail-unit #'t.unit]
]

The other cool part of the implementation is the way @racket[(chk a)]
is an abbreviation for @racket[(chk #:t a)]. Normally if you wanted to
do that, you would expand the first into the second, but this has to
work during the parsing of a syntax class, so you can't produce some
output to re-expand. Instead, I use a @racket[with-syntax] to
immediately parse the expanded version and then grab its attributes:

@racketblock[
[pattern (~seq a:expr)
         #:with (c:test) (syntax/loc #'a (#:t a))
         #:attr unit #'c.unit
         #:attr fail-unit #'c.fail-unit]
]

This was a fun macro to write and I've already found it very useful.

@section{Yo! It's almost time to go!}

But first let's remember what we did today!

It's wonderful to write test cases.

@racketmodname[rackunit] is great for writing test cases and wherever
it is lacking macros can make up the difference.

@link["http://pkgs.racket-lang.org/#[rackunit-chk]"]{@code{rackunit-chk}}
tries to unify a lot of the possible problems into a single macro,
@racket[chk].

@(the-end)
