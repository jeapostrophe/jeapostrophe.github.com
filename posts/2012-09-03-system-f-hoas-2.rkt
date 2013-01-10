#lang scribble/lp
@(require (for-label (except-in racket/base
                                eval)
                     rackunit
                     racket/list))
@literal{
---
layout: post
title: "System F: Interpreter and Type Checker, HOAS style"
comments: true
categories:
- Racket
- Lambda Calculus
---
}

Last time we implemented the obvious version of a System F interpreter
and type-checker. But, it was annoying to have to implement
substitution and a type environment, because they are tedious and
error-prone (especially w.r.t renaming free variables.) This week,
we'll re-present them using the binding technique, higher-order
abstract syntax, or HOAS (which has a bit of an unsightly
pronunciation.)

@(the-jump)

In our last version, we represented terms (types and programs) as
first-order, recursive data. This included representing program
identifiers as symbols. With HOAS, we use identifiers in the
meta-language (Racket) to represent identifiers in the
object-language (System F) and functions in the meta-language (Racket)
to represent binding in the object-language (System F).

Here are the new terms:

@chunk[<program-terms>
       (struct APP (rator rand))
       (struct TYAPP (rator rand))
       (struct ABS (typ val->body))
       (struct TYABS (type->body))
       (struct NUM (val) #:transparent)
       (struct SUCC ())
       ]

The thing to notice is that we've removed the @racket[ID] constructed
and removed the binding names from @racket[ABS] and @racket[TYABS],
replacing the body with a function that accepts the substitution that
would have been inserted where the name was used.

We do the same thing @racket[TYID] and @racket[TYARR] ("for all") in the types:

@chunk[<type-terms>
       (struct ARR (dom rng) #:transparent)
       (struct TYARR (type->body) #:transparent)
       (struct TYNUM () #:transparent)]

Here's an new version of the example, @racket[DOUBLE] function.

@chunk[<double>
       (define DOUBLE
         (TYABS
          (λ (X)
            (ABS (ARR X X)
                 (λ (f)
                   (ABS X
                        (λ (a)
                          (APP f
                               (APP f
                                    a)))))))))]

And example use of the function gives it the successor function and
calls it with the number 3.

@chunk[<example>
       (APP
        (APP (TYAPP DOUBLE (TYNUM))
             (SUCC))
        (NUM 3))]

When we run this program, we would expect it to return 5.

The interpreter for this language is still mostly the same, except
wherever we called substitution before, we now have a function that
will do the substitution for us.

@chunk[<eval>
       (define eval
         (match-lambda
          [(APP rator rand)
           (match (eval rator)
             [(ABS _ val->body)
              (eval (val->body (eval rand)))]
             [(SUCC)
              (match (eval rand)
                [(NUM n)
                 (NUM (add1 n))])])]
          [(TYAPP rator rand)
           (match (eval rator)
             [(TYABS type->body)
              (eval (type->body rand))])]
          [val
           val]))]

The beautiful thing about this is that we can totally ignore
substitution, because Racket implemented it for us.

HOAS is an amazing technique because of this convenience. If you want
to learn more about HOAS, I suggest starting from
@link["http://barzilay.org/research.html"]{Eli Barzilay's various
publications on it}.

But, HOAS is not without its problems. The first is that the binding
structure of the meta-language and object-language should be similar,
or at least binding in the object-language should not have any exotic
analysis tied to it that would not be reflected in the
meta-language. This makes using HOAS to implement Scheme and Racket
macros challenging. Another problem is that HOAS terms are not easy to
do induction on, so it is difficult to just "open them up for a look",
because the only way to get at the inside is to perform the
substitution. For example, let's look at how we need to change the
type-checker.

Recall our type checker examples, the doubling example and this
program, which contains a type error:

@chunk[<type-error>
       (APP
        (APP (TYAPP DOUBLE (TYNUM))
             (ABS (TYNUM) 
                  (λ (N)
                    (ABS (TYNUM) (λ (U) N)))))
        (NUM 3))]

The first thing about the type-checker is that it doesn't use an
environment or substitution, as it did before. Instead, for type
applications, where it used type substitution it simply uses the HOAS
substitution. But, the situation is more complicated for the
replacement of the environment. The environment was used to map value
identifiers to their types during the analysis. But during
type-checking, we have no values, so we don't have anything to
substitute in place of the identifier so we can inspect the body of
the function.

We could create a new kind of value term that "is" a type that we
could substitute in with the expected type. I don't like this approach
because these terms don't have any meaningful run-time
behavior. Instead, I like to write a function that takes a type and
creates a value that has that type: @racket[type->val].

@chunk[<type->val>
       (define type->val
         (match-lambda
          [(TYNUM) 
           (NUM 0)]
          [(ARR dom rng)
           (ABS dom (λ (val) (type->val rng)))]))]

Once this function is in place, it is simple to write the
@racket[type-of] function (I've put the most interesting cases on
top):

@chunk[<type-of>        
       (define type-of
         (match-lambda
          [(ABS ty val->body)
           (ARR ty (type-of (val->body (type->val ty))))]
          [(TYAPP rator rand)
           (match (type-of rator)
             [(TYARR type->body)
              (type-of (type->body rand))]
             [_
              #f])]
          [(APP rator rand)
           (match (type-of rator)
             [(ARR dom rng)
              (and (equal? dom (type-of rand))
                   rng)]
             [_
              #f])]           
          [(TYABS type->body)
           (TYARR type->body)]
          [(SUCC)
           (ARR (TYNUM) (TYNUM))]
          [(NUM _)
           (TYNUM)]
          [_
           #f]))]

And now we have another implementation of System F.

One other great thing about HOAS though, that could inspire another
implementation, is that it becomes easier to use GADTs to enforce
@emph{in the meta-language} the type-correctness of the
object-language terms. My student, Dan Burton, has written about that
in
@link["https://github.com/DanBurton/Blog/blob/master/Literate%20Haskell/SystemF.lhs"]{this
literate Haskell file}.

By the way, if you use this code at home, make sure you put the code
in this order:

@chunk[<*>
        (require racket/match)

        <program-terms>
        <type-terms>
        <double>
        <eval>
        <type->val>
        <type-of>

        (require rackunit)
        (check-equal? (eval <example>)
                      (NUM 5))
        (check-equal? (type-of <example>)
                      (TYNUM))
        (check-equal? (type-of <type-error>)
                      #f)]
