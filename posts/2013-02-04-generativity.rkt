#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/list
                     racket/sandbox
                     web-server/servlet-env)
          "../post.rkt")

@title{Structures with Macros and Generativity}
@categories["Macros" "Racket"]

Racket comes with built-in records called structures, naturally. In
this post, I discuss how you might have implemented them yourself and
how that implementation might be different than how Racket's work.

@(the-jump)

Let's start with a simple example of a structure:

@chunk[<struct-example>
       (jstruct posn posn? (posn-x posn-y))
       (define p (posn 1 2))
       (check-true (posn? p))
       (check-equal? 1 (posn-x p))
       (check-equal? 2 (posn-y p))]

We define a @racket[_posn] with two fields, @racket[_x] and
@racket[_y], then construct one, test its @racket[_posn]-ness and the
extract its fields.

One way to implement this is with lists of a fixed length:

@chunk[<list-struct1>
       (define-syntax-rule (jstruct struct struct? (struct-f ...))
         (begin
           (define (struct struct-f ...)
             (list struct-f ...))
           (define (struct? v)
             (and (list? v)
                  (= (length v) (length '(struct-f ...)))))
           (define (struct-f l)
             (for/or ([v (in-list l)]
                      [f (in-list '(struct-f ...))])
               (and (eq? 'struct-f f)
                    v)))
           ...))]

This is pretty clearly a bad implementation for a few reasons. One
problem is that the struct accessors are O(n) in the number of fields,
but that's just to simplify the macro. We can fix that with a
hash-table instead. (Although the best thing would be to use a vector
where we compute the offsets in the macro, but I don't want to get
into the macro machinery to do that.)

@chunk[<hash-struct1>
       (define-syntax-rule (jstruct struct struct? (struct-f ...))
         (begin
           (define (struct struct-f ...)
             (make-immutable-hasheq (list (cons 'struct-f struct-f) ...)))
           (define (struct? v)
             (and (hash-eq? v) (immutable? v)
                  (hash-has-key? v 'struct-f)
                  ...))
           (define (struct-f v)
             (hash-ref v 'struct-f))
           ...))]

The other problem, that the hash implementation shares is that any
hash table (list) that happens to have the right structure counts as a
@racket[_posn]. 

@chunk[<hash-struct1-hack>
       (define not-a-posn
         (hasheq 'posn-x #f 'posn-y #t))
       (check-true (posn? not-a-posn))]

We can fix that by having a non-forgeable value:

@chunk[<hash-struct2>
       (define jstruct-key (gensym 'dont-tase-me-bro))
       (define-syntax-rule (jstruct struct struct? (struct-f ...))
         (begin
           (define (struct struct-f ...)
             (make-immutable-hasheq (list (cons jstruct-key jstruct-key)
                                          (cons 'struct-f struct-f) ...)))
           (define (struct? v)
             (and (hash-eq? v) (immutable? v)
                  (eq? (hash-ref v jstruct-key #f) jstruct-key)
                  (hash-has-key? v 'struct-f)
                  ...))
           (define (struct-f v)
             (hash-ref v 'struct-f))
           ...))]

(We will assume that the @racket[_jstruct-key] identifier is not
exported from the code implementing the structure macro.)

@chunk[<hash-struct2-nothack>
       (define not-a-posn
         (hasheq 'posn-x #f 'posn-y #t))
       (check-false (posn? not-a-posn))]

Although, this value isn't really non-forgeable because we could
extract the value from the hashes that get exposed by the instances:

@chunk[<hash-struct2-hack>
       (define a-posn (posn 1 2))
       (define the-key 
         (for/or ([v (in-hash-values a-posn)])
           (and (not (number? v))
                v)))
       (define not-a-posn-2
         (hasheq the-key the-key 'posn-x #f 'posn-y #t))
       (check-true (posn? not-a-posn-2))]

The problem is that we don't have a way to restrict access to the
data-structure we use to implement the structures. Many people assume
that the feature structures provide is the ability to efficiently
represent records and extract their values, etc. This is not the
case. Structures provide a way to implement @deftech{sealing}, or
tying core data-structures to their creators and only their creators.

Supposing that we had sealing, then we could implement structures as
follows:

@chunk[<hash-struct3>
       (define-syntax-rule (jstruct struct struct? (struct-f ...))
         (begin
           (define-values (seal seal? unseal) (make-seal))
           (define (struct struct-f ...)
             (seal (make-immutable-hasheq (list (cons 'struct-f struct-f) ...))))
           (define (struct? v)
             (seal? v))
           (define (struct-f v)
             (hash-ref (unseal v) 'struct-f))
           ...))]

But how could we implement seals? Is it possible to do it inside of
the language or do we have rely on extending the Racket VM?

It turns out that if your language provides first-class functions,
then you already have one data-structure that is automatically
sealed. (Closures are data-structures because they hold their
environment.) We can use this by implementing sealed values as
functions waiting for a dynamically-generated key to unlock them:

@chunk[<seals>
       (define (make-seal)
         (define our-key (gensym 'dont-tase-me-bro))
         (define (seal v)
           (λ (key)
             (when (eq? key our-key)
               v)))
         (define (seal? sv)
           (and (procedure? sv)
                (not (void? (unseal sv)))))
         (define (unseal sv)
           (sv our-key))
         (values seal seal? unseal))]

With seals in place, our previous hacks do not work:

@chunk[<hash-struct3-nothack>
       (define not-a-posn
         (hasheq 'posn-x #f 'posn-y #t))
       (check-false (posn? not-a-posn))
       (define a-posn (posn 1 2))
       (check-false (hash? a-posn))]

This is roughly how Racket's sealing (called
@racket[make-struct-type]) works except (1) it is much more
efficient, (2) it is baked into structures, and (3) it cooperates with
the inspector system to allow some "master" code to view the private
values of its "slaves".

This implementation is so similar that it has a behavior that many
Racket users find a little confusing. Consider this program:

@chunk[<confusing>
       (module server racket/base
         (require rackunit)
         <seals>
         <hash-struct3>
         <struct-example>
         (provide posn posn? posn-x posn-y))
       (module a racket/base
         (require 'server)
         (define p (posn 1 2))
         (provide p))
       (module b racket/base
         (require 'server)
         (define f posn-x)
         (provide f))
       (module connector racket/base
         (require racket/sandbox)
         (define a-eval (make-evaluator 'racket/base '(require a)))
         (define b-eval (make-evaluator 'racket/base '(require b)))
         (check-equal?
          ((b-eval 'f)
           (a-eval 'p))
          1))]

It may seem like this test should pass because both @racket[_a] and
@racket[_b] use the same @racket[_server] module. Unfortunately,
because they are evaluated separately, the @racket[_server] module
runs twice, generating a different seal each time. This is called
@deftech{generativity}.

The same thing happens in Racket and often shows up when people use
sandboxes or code that uses sandboxes. For example, DrRacket keeps
user programs separate from itself, so most structures do not
inter-operate. The Web server runs each servlet in a separate place, so
they are all separate from each other and the server. In the case of
the Web server, @racket[#:servlet-namespace] argument to functions
like @racket[serve/servlet] exists to explicitly allow servlets to
share some (but not all) modules.

By the way, if you use this code at home, make sure you put the code
in this order:

@chunk[<*>
       (require rackunit)

       (let ()
         <list-struct1>
         <struct-example>)

       (let ()
         <hash-struct1>
         <struct-example>
         <hash-struct1-hack>)

       (let ()
         <hash-struct2>
         <struct-example>
         <hash-struct2-nothack>
         <hash-struct2-hack>)

       (let ()
         <seals>
         <hash-struct3>
         <struct-example>
         <hash-struct3-nothack>)]

@(the-end)
