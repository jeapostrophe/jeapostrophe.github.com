#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/list
                     racket/generator
                     racket/file)
          "../post.rkt")

@title{Swapping Folds on the Left and Right}
@categories["Racket" "Algebra"]

Folds, or catamorphisms, are beautiful functions that have lots of
neat properties and are often a programmer's first foray into
algebra. In this post, we talk about the difference between a left and
right fold and how to swap between them.

@(the-jump)

Although folds exist for all data-structures, it is easiest to
understand folds on lists, I think. One way to understand them is that
the change the constructors of a data-structure into a different
function. For instance, in this example, we change the @racket[cons]s
to @racket[+] and the @racket[empty] to @racket[0]:

@chunk[<ex1>
       (check-equal?
        (fold + 0
              (cons 1 (cons 2 (cons 3 empty))))
        (   + 1 (   + 2 (   + 3     0))))]

The same operation is conceivable for anything else, like a tree,
where we could turn branches into @racket[+] and the leafs into the
identity function:

@chunk[<ex2>
       (struct lf (v))
       (struct br (l r))
       (check-equal?
        (fold + id
              (br (br (lf 1) (br (lf 1) (lf 2))) (br (lf 1) (lf 2))))
        ( + ( + (id 1) ( + (id 1) (id 2))) ( + (id 1) (id 2))))]

This way of thinking about folds leads directly to their
implementation. Here is the implementation for lists:

@chunk[<list-fold>
       (define (fold replace-cons replace-empty a-list)
         (match a-list
           [(? empty?)
            replace-empty]
           [(cons first-v rest-list)
            (replace-cons first-v
                          (fold replace-cons replace-empty rest-list))]))]

And for trees:

@chunk[<tree-fold>
       (define (fold replace-br replace-lf a-tree)
         (match a-tree
           [(br l-tree r-tree)
            (replace-br (fold replace-br replace-lf l-tree)
                        (fold replace-br replace-lf r-tree))]
           [(lf leaf-val)
            (replace-lf leaf-val)]))]

Something to notice about a fold is that the structure of the
computation mirrors the structure of the data-structure. This means
that folding a list with @racket[_N] elements will create a stack that
is @racket[_N] elements deep. There is another kind of a fold, a
so-called "linear fold" that does not have this property. However, it
necessarily has a different semantics, because things happen in a
different order. Let's see the function definition first:

@chunk[<list-linear-fold>
       (define (linear-fold do-at-cons replace-empty-or-start-at a-list)
         (match a-list
           [(? empty?)
            replace-empty-or-start-at]
           [(cons first-v rest-list)
            (linear-fold do-at-cons
                         (do-at-cons first-v
                                     replace-empty-or-start-at)
                         rest-list)]))]

Since the structure of the computation is no longer related to the
structure of the data, it maintains constant stack space by being
tail-recursive. The other consequence of this is that it no longer
makes sense to say that the function "replaces" @racket[cons] and
@racket[empty], but rather are "done at" @racket[cons]. If you look at
an example like before you can see that the final answer is the same,
but the order is different:

@chunk[<ex3>
       (check-equal?
        (linear-fold + 0
                     (cons 1 (cons 2 (cons 3 empty))))
        (+ (+ (+ 1 0) 2) 3))]

This means that @racket[fold] and @racket[linear-fold] are equivalent
on associative operations, which means @racket[linear-fold] is
preferable in some sense. However, it is more awkward to describe and
gives different answers for other operations. For instance, compare
the following versions:

@chunk[<ex4>
       (check-equal? (fold string-append "[]" '("1" "2" "3"))
                     "123[]")
       (check-equal? (linear-fold string-append "[]" '("1" "2" "3"))
                     "321[]")]

Furthermore, a linear version of a tree fold doesn't really make as
much sense, because there is no linear sequence to start at the "left"
of. (Although it does make sense to traverse the tree in one order and
              then process that sequence "from the left".)

Is it possible to "swap" the folds by implementing a linear fold in
terms of a normal fold? In other words, can we write
@racket[fold-as-linear-fold] as in the following code?

@chunk[<ex5>
       (check-equal? (linear-fold-as-fold string-append "[]" '("1" "2" "3"))
                     "123[]")
       (check-equal? (fold-as-linear-fold string-append "[]" '("1" "2" "3"))
                     "321[]")]

The amazing thing is that not only can we write it, but the same
function swaps both!

@chunk[<swaps>
       (define fold-as-linear-fold (swap-folds fold))
       (define linear-fold-as-fold (swap-folds linear-fold))]

The easiest way to think about it is from the
@racket[linear-fold-as-fold] perspective. We manually create the
right-to-left stack by folding the creation of the stack, and then
call it at the end:

@chunk[<swap-folds>
       (define ((swap-folds base-fold) do-at-cons final-answer a-list)
         ((base-fold (λ (first-element stack)
                       (λ (next-answer)
                         (stack (do-at-cons first-element next-answer))))
                     (λ (last-answer)
                       last-answer)
                     a-list)
          final-answer))]

The computation is structured as in this trace:

@chunk[<ex5-trace>
       (check-trace?
        (linear-fold-as-fold string-append "[]" '("1" "2" "3"))
        ((linear-fold (λ (first-element stack)
                        (λ (next-answer)
                          (stack (string-append first-element next-answer))))
                      (λ (last-answer)
                        last-answer)
                      '("1" "2" "3"))
         "[]")
        ((linear-fold (λ (first-element stack)
                        (λ (next-answer)
                          (stack (string-append first-element next-answer))))
                      (λ (next-answer)
                        ((λ (last-answer)
                           last-answer)
                         (string-append "1" next-answer)))
                      '("2" "3"))
         "[]")
        ((linear-fold (λ (first-element stack)
                        (λ (next-answer)
                          (stack (string-append first-element next-answer))))
                      (λ (next-answer)
                        ((λ (next-answer)
                           ((λ (last-answer)
                              last-answer)
                            (string-append "1" next-answer)))
                         (string-append "2" next-answer)))
                      '("3"))
         "[]")
        ((linear-fold (λ (first-element stack)
                        (λ (next-answer)
                          (stack (string-append first-element next-answer))))
                      (λ (next-answer)
                        ((λ (next-answer)
                           ((λ (next-answer)
                              ((λ (last-answer)
                                 last-answer)
                               (string-append "1" next-answer)))
                            (string-append "2" next-answer)))
                         (string-append "3" next-answer)))
                      '())
         "[]")
        ((λ (next-answer)
           ((λ (next-answer)
              ((λ (next-answer)
                 ((λ (last-answer)
                    last-answer)
                  (string-append "1" next-answer)))
               (string-append "2" next-answer)))
            (string-append "3" next-answer)))
         "[]")
        ((λ (next-answer)
           ((λ (next-answer)
              ((λ (last-answer)
                 last-answer)
               (string-append "1" next-answer)))
            (string-append "2" next-answer)))
         (string-append "3" "[]"))
        ((λ (next-answer)
           ((λ (next-answer)
              ((λ (last-answer)
                 last-answer)
               (string-append "1" next-answer)))
            (string-append "2" next-answer)))
         "3[]")
        ((λ (next-answer)
           ((λ (last-answer)
              last-answer)
            (string-append "1" next-answer)))
         (string-append "2" "3[]"))
        ((λ (next-answer)
           ((λ (last-answer)
              last-answer)
            (string-append "1" next-answer)))
         "23[]")
        ((λ (last-answer)
           last-answer)
         (string-append "1" "23[]"))
        ((λ (last-answer)
           last-answer)
         "123[]")
        "123[]")]

It may be surprising that using @racket[linear-fold] to implement
@racket[fold] does not grant the space guarantees of
@racket[linear-fold] to @racket[fold]. While it does ensure that
@emph{during the creation of the computation} the stack is constant,
the computation that is built is linear in the size of the list.

From the other perspective, it works because @racket[fold] builds a
computation that is just structured in the other direction. This
technique does not, however, give us a way to make linear folds of
trees because the continuation would be duplicated at each leaf.

@section{Yo! It's almost time to go!}

But first let's remember what we learned today!

Folds are neat and exist for every data-structure.

Linear folds are weird and aren't really like the other folds at all.

But you can swap them by using continuation-passing style! Weird!

If you'd like to run this exact code at home, you should put it in
this order:

@chunk[<*>
       (require racket/list
                racket/function
                racket/match
                rackunit)

       (define-syntax-rule (check-3qual? a b c)
         (begin (check-equal? a b)
                (check-equal? b c)))

       (define (check-trace? . l)
         (for/fold ([last (first l)])
             ([next (in-list (rest l))])
           (check-equal? last next)
           next))

       (define (id x) x)

       (let ()
         <list-fold>
         <ex1>
         <list-linear-fold>
         <ex3>
         <ex4>
         <swap-folds>
         <swaps>
         <ex5>)

       (let ()
         <tree-fold>
         <ex2>)]

@(the-end)
