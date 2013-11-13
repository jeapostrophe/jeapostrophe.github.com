#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/list
                     racket/generator
                     racket/file)
          "../post.rkt")

@title{The Design Space of Conditionals with Embedded Defines}
@categories["Racket" "Macros"]

I have a great disgust for long lines and right-ward shift in Racket
programs. One of the hardest places to avoid this is when you have a
large conditional where multiple cases need to refer to the same
values that may not actually exist unless some other cases have
already passed. In this post, I discuss a few macros that attempt to
avoid this problem.

@(the-jump)

First, let's look at a few examples of this problem.

In the first case, we need to use the same value, @racket[(first
env)], in the condition and the body, but we can't define the value
outside the @racket[cond], because the @racket[env] may be
@racket[empty?].

@chunk[<ex1>
       (define (lookup id env)
         (cond
           [(empty? env)
            #f]
           [(eq? id (car (first env)))
            (cdr (first env))]
           [else
            (lookup id (rest env))]))]

In the second case, we share the same values across multiple clauses.

@chunk[<ex2>
       (define (f l)
         (cond
           [(empty? l)
            empty]
           [(zero? (modulo (first l) 3))
            (cons 3 (f (rest l)))]
           [(zero? (modulo (first l) 4))
            (cons (+ (first l) 4) (rest l))]
           [else
            (list (first l))]))]

The second case represents the worst case, so let's look at the
traditional way to avoid the problem: hoist the common values and
create a new @racket[cond].

@chunk[<ex2-hoist>
       (define (f-hoist l)
         (cond
           [(empty? l)
            empty]
           [else
            (match-define (cons fst rst) l)
            (cond
              [(zero? (modulo fst 3))
               (cons 3 (f rst))]
              [(zero? (modulo fst 4))
               (cons (+ fst 4) rst)]
              [else
               (list fst)])]))]

This code has the desirable property that we don't duplicate the same
expression, but the undesirable additional layer of indentation.

One solution to this is to write a macro that enables such inline
@racket[define]s in a very simple way.

@chunk[<condd1>
       (define-syntax (condd1 stx)
         (syntax-parse stx
           #:literals (define else)
           [(_)
            #'(error 'condd1 "Fell through without else clause")]
           [(_ [else . d])
            #'(let () . d)]
           [(_ (define . d) ...+ . more)
            #'(let ()
                (define . d) ...
                (condd1 . more))]
           [(_ [test . b] . more)
            #'(if test
                (let () . b)
                (condd1 . more))]))]

This macro is very simple to write, understand, and use:

@chunk[<ex2-condd1>
       (define (f-condd1 l)
         (condd1
          [(empty? l)
           empty]
          (define fst (first l))
          (define rst (rest l))
          [(zero? (modulo fst 3))
           (cons 3 (f rst))]
          [(zero? (modulo fst 4))
           (cons (+ fst 4) rst)]
          [else
           (list fst)]))]

Unfortunately, it is brittle and ugly, because we had to hard-code the
@racket[define] in to the macro and we can't use @racket[match-define]
like our original version did. Obviously we could hard-code more
forms, but then we couldn't use these inter-clause @racket[define]s to
define new macros, for instance.

This problem is kind of unavoidable. We don't have any syntactic cue
to know where the @racket[define]s are and where the clauses are, so
we have to use @racket[define] for it. Since we have more defining
forms than clauses forms, we could make the opposite choice and add
something to the clause syntax:

@chunk[<ex2-condd2>
       (define (f-condd2 l)
         (condd2
          #:cond [(empty? l)
                  empty]
          (match-define (cons fst rst) l)
          #:cond [(zero? (modulo fst 3))
                  (cons 3 (f rst))]
          #:cond [(zero? (modulo fst 4))
                  (cons (+ fst 4) rst)]
          #:else (list fst)))]

This style has the added benefit of simplifying the @racket[else]
syntax by removing the final set of parentheses. This is a little bit
more complicated to implement, mainly because we have to identify
@racket[define]s by their lack of a keyword.

@chunk[<condd2>
       (define-syntax (condd2 stx)
         (syntax-parse stx
           [(_)
            #'(error 'condd2 "Fell through without else clause")]
           [(_ #:else . e)
            #'(let () . e)]
           [(_ (~and d (~not y:keyword)) ...
               #:cond [t:expr . e] . tail)
            #'(let ()
                d ...
                (if t
                  (let () . e)
                  (condd2 . tail)))]))]

While this is a lot more elegant in some ways, it feels a little
painful to have to meticulously annotate each clause. A final way to
do it is to add a set of parentheses everywhere to will go unused when
there is no set of definitions for that layer:

@chunk[<ex2-condd3>
       (define (f-condd3 l)
         (condd3
          ([(empty? l)
            empty])
          ((match-define (cons fst rst) l)
           [(zero? (modulo fst 3))
            (cons 3 (f rst))])
          ([(zero? (modulo fst 4))
            (cons (+ fst 4) rst)])
          ([else (list fst)])))]

The macro for this is basically the same as @racket[condd2], but with
parenthesis rather than keywords in the input syntax.

@chunk[<condd3>
       (define-syntax (condd3 stx)
         (syntax-parse stx
           [(_)
            #'(error 'condd3 "Fell through without else clause")]
           [(_ (d ... [else . e]))
            #'(let () d ... . e)]
           [(_ (d ... [t:expr . e]) . tail)
            #'(let ()
                d ...
                (if t
                  (let () . e)
                  (condd3 . tail)))]))]

I like the simplicity of this version, but I don't find it is easy to
read and slightly prefer @racket[condd2], despite its flaws.

@bold{Update:} Laurent Orseau had a great suggestion where you use a
keyword for the definitions, because they are less common. I feel
stupid for not thinking of this at first. Here's the program:

@chunk[<ex2-condd4>
       (define (f-condd4 l)
         (condd4
          [(empty? l)
           empty]
          #:do (match-define (cons fst rst) l)
          [(zero? (modulo fst 3))
           (cons 3 (f rst))]
          [(zero? (modulo fst 4))
           (cons (+ fst 4) rst)]
          [else (list fst)]))]

And the trivial definition:

@chunk[<condd4>
       (define-syntax (condd4 stx)
         (syntax-parse stx
           [(_)
            #'(error 'condd4 "Fell through without else clause")]
           [(_ [else . e])
            #'(let () . e)]
           [(_ #:do d . tail)
            #'(let () d (condd4 . tail))]
           [(_ [t:expr . e] . tail)
            #'(if t
                (let () . e)
                (condd4 . tail))]))]

I think this is clearly superior.

@section{Yo! It's almost time to go!}

But first let's remember what we learned today!

Even with internal definitions, many binding forms create indentation
in Racket.

Simple macros can remove the need for structures you don't like in
your programs.

The design space of macros that solve the same task can be relatively
large.

A good macro is one that composes with other macros and doesn't
hard-code decisions like which definition forms are allowed.

If you'd like to run this exact code at home, you should put it in
this order:

@CHUNK[<*>
       (require (for-syntax racket/base
                            syntax/parse)
                racket/list
                racket/match)

       <ex1>
       <ex2>
       <ex2-hoist>

       <condd1>
       <ex2-condd1>

       <condd2>
       <ex2-condd2>

       <condd3>
       <ex2-condd3>

       <condd4>
       <ex2-condd4>]

Or just download the
@link["https://github.com/jeapostrophe/exp/blob/master/condd.rkt"]{raw
version}.

@(the-end)
