#lang scribble/lp
@(require (for-label (except-in racket/base
                                eval)
                     rackunit
                     racket/list)
          "post.rkt")

@title{System F: Interpreter and Type Checker}
@categories["Racket" "Lambda Calculus"]

System F is a very powerful programming and type system. It is fun to
make an interpreter for, but I think it is even more fun to write such
an interpreter using higher-order abstract syntax. In this post, I
describe how a normal System F interpreter works and next week we'll
see the HOAS version.

@(the-jump)

System F is basically the same as the normal Lambda Calculus, except
that it has first-class, explicit polymorphism.

Its program terms are: identifiers, applications, type
applications (for instantiating polymorphism), abstractions (value
functions), and type abstractions (type functions, or points of
polymorphism). For the purposes of making the code more meaningful,
I'll also add primitive numbers and the successor function.

@chunk[<program-terms>
       (struct ID (sym))
       (struct APP (rator rand))
       (struct TYAPP (rator rand))
       (struct ABS (typ sym body))
       (struct TYABS (sym body))
       (struct NUM (val) #:transparent)
       (struct SUCC ())
       ]

Its type terms are: type identifiers, arrows and type arrows (for the
two kinds of abstraction). I'll also add a primitive number type.

@chunk[<type-terms>
       (struct TYID (sym) #:transparent)
       (struct ARR (dom rng) #:transparent)
       (struct TYARR (sym body) #:transparent)
       (struct TYNUM () #:transparent)]

Here's an example function: the polymorphic doubling function that
applies its argument twice.

@chunk[<double>
       (define DOUBLE
         (TYABS 'X
                (ABS (ARR (TYID 'X) (TYID 'X))
                     'f
                     (ABS (TYID 'X)
                          'a
                          (APP (ID 'f)
                               (APP (ID 'f)
                                    (ID 'a)))))))]

And example use of the function gives it the successor function and
calls it with the number 3.

@chunk[<example>
       (APP
        (APP (TYAPP DOUBLE (TYNUM))
             (SUCC))
        (NUM 3))]

When we run this program, we would expect it to return 5.

An interpreter for this language is fairly obvious:

@chunk[<eval>
       (define eval
         (match-lambda
          [(APP rator rand)
           (match (eval rator)
             [(ABS _ id body)
              (eval (subst id (eval rand) body))]
             [(SUCC)
              (match (eval rand)
                [(NUM n)
                 (NUM (add1 n))])])]
          [(TYAPP rator rand)
           (match (eval rator)
             [(TYABS id body)
              (eval (type-subst id rand body))])]
          [val
           val]))]

But it relies on two substitution functions: one for value
abstractions and another for type abstractions. These functions, while
not especially complicated, are tedious to write and only get more
complicated as binding rules become more complex.

@chunk[<subst>
       (define (subst x v t)
         (match t
           [(ID sym)
            (if (eq? x sym)
              v
              t)]
           [(SUCC)
            t]
           [(APP rator rand)
            (APP (subst x v rator)
                 (subst x v rand))]
           [(ABS ty id body)
            (if (eq? id x)
              t
              (ABS ty id (subst x v body)))]))
       (define (type-subst x v t)
         (match t
           [(SUCC)
            t]
           [(TYID sym)
            (if (eq? sym x)
              v
              t)]
           [(APP rator rand)
            (APP (type-subst x v rator)
                 (type-subst x v rand))]
           [(ABS ty id body)
            (ABS (type-subst x v ty)
                 id
                 (type-subst x v body))]
           [(ARR dom rng)
            (ARR (type-subst x v dom)
                 (type-subst x v rng))]
           [(ID sym)
            t]))]

But, this language does not just have runtime behavior, we also need a
type checker. As test cases, we'll type the example and this program,
which contains a type error:

@chunk[<type-error>
       (APP
        (APP (TYAPP DOUBLE (TYNUM))
             (ABS (TYNUM) 'N
                  (ABS (TYNUM) 'U
                       (ID 'N))))
        (NUM 3))]

The type checker is fairly straight-forward relative to the
evaluator. We won't do substitution for value abstraction, instead
we'll keep a type environment. We will need to do substitution for
type applications, though.

@chunk[<type-of>
       (define (type-of t)
         (type-of/env (hasheq) t))
       (define (type-of/env env t)
         (match t
           [(ID sym)
            (hash-ref env sym)]
           [(ABS ty id body)
            (ARR ty (type-of/env (hash-set env id ty) body))]
           [(APP rator rand)
            (match (type-of/env env rator)
              [(ARR dom rng)
               (and (equal? dom (type-of/env env rand))
                    rng)]
              [_
               #f])]
           [(TYAPP rator rand)
            (match (type-of/env env rator)
              [(TYARR id body)
               (type-of/env env (type-subst id rand body))]
              [_
               #f])]
           [(TYABS id body)
            (TYARR id body)]
           [(SUCC)
            (ARR (TYNUM) (TYNUM))]
           [(NUM _)
            (TYNUM)]
           [_
            #f]))]

At this point, we have a basic and obvious implementation of System
F. There are a few things annoying about it though, mainly to do with
how we have to deal with identifiers through the tedious substitution
function and the ugly use of a type environment.

In the next post, we'll see how to remedy this through the use of
higher-order abstract syntax.

By the way, if you use this code at home, make sure you put the code
in this order:

@chunk[<*>
       (require racket/match)

       <program-terms>
       <type-terms>
       <double>
       <eval>
       <subst>
       <type-of>

       (require rackunit)
       (check-equal? (eval <example>)
                     (NUM 5))
       (check-equal? (type-of <example>)
                     (TYNUM))
       (check-equal? (type-of <type-error>)
                     #f)]

