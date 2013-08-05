#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/unit
                     racket/match
                     racket/list
                     data/heap)
          "../post.rkt")

@title{Binding Forms in Racket and How to Implement Them}
@categories["Racket" "Macros"]

This post discusses a classic sequence of macros that show how to
construct a "tower of languages" by building macros atop one
another. In particular, the sequence focuses on the binding forms.

@(the-jump)

"Binding", in this context, refers to attaching values to
identifiers. This feature is built-in to most languages through the
features of @emph{function definition} and @emph{function call}. For
instance, in the following sample the function @racket[f] binds the
identifier @racket[x] and the function call attaches the value
@racket[1] to @racket[x]:

@chunk[<lambda>
       (define f (lambda (x) (list x)))
       (check-equal? (f 1) (list 1))]

It is possible to use this pattern to introduce purely local
bindings. For instance, we can remove the duplication in the code:

@chunk[<let-before>
       (check-equal?
        (+ (+ 1 2)
           (+ 1 2))
        6)]

By using a local function definition and immediately calling it:

@chunk[<let-after-expanded>
       (check-equal?
        ((lambda (x)
           (+ x
              x))
         (+ 1 2))
        6)]

This pattern works just fine for binding many things at the same time:

@chunk[<let-after-expanded-many>
       (check-equal?
        ((lambda (x y)
           (+ x
              x
              y
              y))
         (+ 1 2)
         (+ 3 4))
        20)]

This kind of pattern is so widely used, it is the first basic binding
form: @racket[let].

@chunk[<let-after-many>
       (check-equal?
        (let ([x (+ 1 2)]
              [y (+ 3 4)])
          (+ x
             x
             y
             y))
        20)]

The definition is very simple:

@chunk[<let-defn>
       (define-syntax-rule (jlet ([what for] ...) where ...)
         ((lambda (what ...) 
            where ...)
          for ...))]

However, the pattern fails when you want one binding to refer to
another as in:

@chunk[<let*-failure>
       (check-not-equal?
        (let ([x (+ 1 2)]
              [y (+ x 3)])
          (+ x y))
        9)]

The reason why this does not work is clear by looking at the expansion
where the use of @racket[x] is clearly not inside the @racket[lambda].

@chunk[<let*-failure-expanded>
       (check-not-equal?
        ((lambda (x y)
           (+ x y))
         (+ 1 2)
         (+ x 3))
        9)]

We can allow this by using multiple @racket[let]s in sequence:

@chunk[<let*-expanded>
       (check-equal?
        (let ([x (+ 1 2)])
          (let ([y (+ x 3)])
            (+ x y)))
        9)]

This introduces us to the next binding form: @racket[let*].

@chunk[<let*>
       (check-equal?
        (let* ([x (+ 1 2)]
               [y (+ x 3)])
          (+ x y))
        9)]

The definition of @racket[let*] is similarly simple, although rather
than requiring only one pattern, it uses two:

@chunk[<let*-defn>
       (define-syntax jlet*
         (syntax-rules ()
           [(_ () where ...)
            (begin where ...)]
           [(_ ([what for]
                more ...)
               where ...)
            (let ([what for])
              (let* (more ...)
                where ...))]))]

Unfortunately, even this pattern fails when the references cannot be
sequenced because they both refer to each other:

@chunk[<letrec-failure>
       (check-not-equal?
        (let* ([even? 
                (λ (x) 
                  (if (zero? x)
                    true
                    (odd? (sub1 x))))]
               [odd?
                (λ (x) 
                  (if (zero? x)
                    false
                    (even? (sub1 x))))])
          (even? 10))
        #t)]

The problem is, of course, that @racket[odd?] can refer to
@racket[even?] but not the reverse. In this case, it doesn't matter
what order they are written, because they won't be called until both
are defined. We can solve this with the pattern of doing the binding
in one place and then filling in the values later with @racket[set!]:

@chunk[<letrec-expanded>
       (check-equal?
        (let ([even? #f]
              [odd? #f])
          (set! even? 
                (λ (x) 
                  (if (zero? x)
                    true
                    (odd? (sub1 x)))))
          (set! odd?
                (λ (x) 
                  (if (zero? x)
                    false
                    (even? (sub1 x)))))
          (even? 10))
        #t)]

This pattern is captured in the @racket[letrec] macro:

@chunk[<letrec>
       (check-equal?
        (letrec ([even? 
                  (λ (x) 
                    (if (zero? x)
                      true
                      (odd? (sub1 x))))]
                 [odd?
                  (λ (x) 
                    (if (zero? x)
                      false
                      (even? (sub1 x))))])
          (even? 10))
        #t)]

The implementation is even simpler than @racket[let*]:

@chunk[<letrec-defn>
       (define-syntax-rule (jletrec ([what for] ...) where ...)
         (jlet ([what #f] ...)
               (set! what for)
               ...
               where
               ...))]

(In a real implementation, however, the initial binding is not
@racket[#f], but a special undefined value that cannot be used in a
useful way, like @racket[#f] can. Unfortunately, there are some
problems with such a value, but that's a topic for another day.)

The last binding form we'll discuss isn't a standard one and doesn't
really have much of a use, but it's very cute. It's called
@racket[letwreck] and it combines all the best features of
@racket[let], @racket[let*], and @racket[letrec] but in a way where
you can pick what behavior you want for each binding! 

The key to @racket[letwreck] is that each binding explicitly specifies
which other bindings it can see:

@chunk[<letwreck>
       (define-syntax-rule (t e) (λ () e))
       (define-syntax-rule (tlist e ...) (t (list (e) ...)))
       (check-equal?
        (let ([x (t 'x)] [y (t 'y)] [z (t 'z)] [h (t 'h)] [i (t 'i)])
          (letwreck
           ([x (i) (tlist x y z h i)]
            [y (x) (tlist x y z h i)]
            [z (y) (tlist x y z h i)]
            [h (x z) (tlist x y z h i)]
            [i () (tlist x y z h i)])
           ((tlist x y z h i))))
        '((x y z h (x y z h i))
          ((x y z h (x y z h i)) y z h i)
          (x ((x y z h (x y z h i)) y z h i) z h i)
          ((x y z h (x y z h i)) y (x ((x y z h (x y z h i)) y z h i) z h i) h i)
          (x y z h i)))]

The implementation is very exciting. The key idea is to use a
@racket[letrec] on the outside but with a new name for each
binding. Then, inside of the right-hand sides, create a local macro
that renames uses of the @emph{original} name to the @emph{new}
name (but only for the ones explicitly mentioned.) Finally, around the
body of the @racket[letwreck], rename all uses of the @emph{original}
names to the @emph{new} ones. This renaming is accomplished through
@racket[make-rename-transformer], which is a special macro-producing
function that renames uses to its argument, where in function or
identifier position, even when used with @racket[set!].

@chunk[<letwreck-defn>
       (define-syntax (letwreck stx)
         (syntax-parse stx
           [(_ ([binding:id (other-binding:id ...) bound-body:expr]
                ...)
               body:expr)
            (with-syntax*
             ([(new-binding ...)
               (generate-temporaries #'(binding ...))]
              [((new-other-binding ...) ...)
               (for/list ([obs (in-list (syntax->list #'((other-binding ...) ...)))])
                 (for/list ([ob (in-list (syntax->list obs))])
                   (for/or ([old (in-list (syntax->list #'(binding ...)))]
                            [new (in-list (syntax->list #'(new-binding ...)))])
                     (and (bound-identifier=? old ob)
                          new))))])
             (syntax/loc stx
               (letrec ([new-binding
                         (let-syntax ([other-binding (make-rename-transformer #'new-other-binding)]
                                      ...)
                           bound-body)] ...)
                 (let-syntax ([binding (make-rename-transformer #'new-binding)]
                              ...)
                   body))))]))]

Isn't that great? The idea for this macro comes as a response to a
@link["https://groups.google.com/forum/#!topicsearchin/comp.lang.scheme/letwreck/comp.lang.scheme/GBF7oJZTl_w"]{November
10th, 1995 @code{comp.lang.scheme} post} from
@link["http://cs.brown.edu/~sk/"]{Shriram Krishnamurthi} (my PhD
advisor) when he was a fresh PhD student at Rice. It came from Alan
Bawden, but no implementation was given at the time. (The earliest I
can find is Petrofsky's in 2001.) I think my implementation is
particularly tight and beautiful.

@section{Yo! It's almost time to go!}

But first let's remember what we learned today!

All you need is @racket[lambda] (all together, now!)

Actually, if you refer to @postref["2013-07-15-values"] you'll see
that you can't build everything on @racket[lambda] once you have
multiple value co-arguments.

The tower of macros can get tall quickly, particularly if you make
very useful and generic macros.

Macros can do crazy things.

If you'd like to run this exact code at home, you should put it in
this order:

@chunk[<*>
       (require rackunit
                racket/bool
                racket/list
                (for-syntax racket/base
                            syntax/parse
                            racket/syntax))

       <lambda>
       <let-before>
       <let-after-expanded>
       <let-after-expanded-many>
       <let-after-many>
       
       <let-defn>
       (check-equal?
        (jlet ([x (+ 1 2)]
               [y (+ 3 4)])
              (+ x
                 x
                 y
                 y))
        20)

       (let ([x 0])
         <let*-failure>)
       (let ([x 0])
         <let*-failure-expanded>)

       <let*-expanded>

       <let*>
       <let*-defn>
       (check-equal?
        (jlet* ([x (+ 1 2)]
                [y (+ x 3)])
          (+ x y))
        9)

       (let ([odd? (λ (x) false)])
         <letrec-failure>)
       <letrec-expanded>
       <letrec>

       <letrec-defn>
       (check-equal?
        (jletrec ([even? 
                   (λ (x) 
                     (if (zero? x)
                       true
                       (odd? (sub1 x))))]
                  [odd?
                   (λ (x) 
                     (if (zero? x)
                       false
                       (even? (sub1 x))))])
                 (even? 10))
        #t)

       <letwreck-defn>
       <letwreck>]

@(the-end)
