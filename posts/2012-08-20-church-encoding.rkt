#lang scribble/lp
@(require (planet ryanc/scriblogify/scribble-util)
          (for-label racket/base
                     rackunit
                     racket/list))
@literal{
---
layout: post
title: "Church Encoding"
comments: true
categories:
- Racket
- Lambda Calculus
- Coq
---
}

One of my favourite ideas in the Lambda Calculus in Church
Encoding. This is the basis of how the Lambda Calculus, with only
functions, can represent other kinds of data.

@(the-jump)

The Lambda Calculus is a very simple language. It just has variable
references, unary functions, and unary applications. Yet, it can
express all computations. A natural question is how this simple
language can represent things like numbers, lists, and other complex
data structures?

@blogsection{Booleans}

I think it is actually easier to start with the most basic of
data-structures: the boolean.

Everyone knows booleans. There are just two of them: true and
false. And what do we do with them? We decide whether to launch the
missiles or to bake a cake:

@chunk[<boolean-example>
       (if <some-boolean>
         (launch-the-missiles)
         (bake-a-cake))]

The essence of being a boolean is being able to decide between two
choices. We can represent this idea in Lambda simply:

@chunk[<booleans>
       (define TRUE
         (λ (first-choice)
           (λ (second-choice) 
             first-choice)))
       (define FALSE
         (λ (first-choice)
           (λ (second-choice) 
             second-choice)))]

The booleans are just functions that when given two choices, return
one or the other. These values can easily be used with if:

@chunk[<if>
       (define IF
         (λ (condition)
           (λ (true-side-thunk)
             (λ (false-side-thunk)
               (((condition true-side-thunk) false-side-thunk) UNIT)))))]

IF simply takes a boolean, deliver the thunks, and calls the one that
the condition selects. This example makes the right choice:

@chunk[<real-boolean-example>
       (((IF FALSE)
         (λ (x) 'launch-the-missiles))
        (λ (x) 'bake-a-cake))]

Booleans are a microcosm of the idea of Church Encoding: represent
data by what it @emph{does} rather than "is". Other to put it another way,
data "isn't" anything, it is just what it does. Booleans make choices,
so encode them as functions that make choices.

All boolean functions can be build with these definitions. There's
normally an obvious way and clever way. Here are two implementations
of and:

@chunk[<and>
       (define AND-OBVIOUS
         (λ (left-side)
           (λ (right-side)
             (((IF left-side)
               (λ (x) right-side))
              FALSE))))
       (define AND-CLEVER
         (λ (left-side)
           (λ (right-side)
             ((left-side right-side) FALSE))))]

The "obvious" way is based on the traditional understanding of data
where we must use functions like IF, whereas the "clever" way is based
on the understanding that booleans *are* IF, so we don't need to call
it.

@blogsection{Natural numbers}

Let's do something a little bit more complicated: numbers.

The traditional Church Encoding of numbers all starts with two things:
zero and the "plus one"---or successor---function.

@chunk[<numbers>
       (define ZERO
         (λ (f) (λ (z) z)))
       (define SUCC
         (λ (n) (λ (f) (λ (z) ((n f) (f z))))))]

Using these definitions you can implement stuff like addition and
derive other numbers, like one and two:

@chunk[<addition>
       (define ONE
         (λ (f) (λ (z) (f z))))
       (define TWO
         (λ (f) (λ (z) (f (f z)))))
       (define PLUS
         (λ (n) (λ (m) (λ (f) (λ (z) ((n f) ((m f) z)))))))]

But what do these functions even mean? If Church Encoding is about
representing data with what it *does*... what do numbers *do*?

Let's take a small detour in the land of my second favourite
programming, Coq. This is the type of natural number
induction:

@verbatim{
 forall P : nat -> Prop,
     P 0 
  -> (forall m : nat, P m -> P (S m))
  -> forall n : nat,
      P n          
}

This say that if you have a property about numbers, and give a proof
of that property for zero, then give a way of taking a proof about m
and returning a proof about m + 1, then for any number n, you can have
a proof about n. How could this function be implemented?

@chunk[<nat_ind>
       (define (nat_ind Pz Pm2PSm)
         (define (loop n)
           (if (zero? n)
             Pz
             (Pm2PSm (loop (sub1 n)))))
         loop)]

Natural number induction's type suggests that it would do interesting
work, but really it was you that did all the work. You gave the proof
about zero, which it returns to you if you call it with zero. You gave
the proof from m to m + 1, which it just uses in case the number isn't
zero. All natural number induction does is do a trivial loop calling
functions you gave it. 

Consider calling this with two. Here's the trace:

@chunk[<nat_ind_trace2>
       (loop 2)
       (Pm2PSm (loop 1))
       (Pm2PSm (Pm2PSm (loop 0)))
       (Pm2PSm (Pm2PSm Pz))]

Hmm... that looks familiar.

Now what does all this natural number induction have to do with Church
Encoding?

Well, Church Encoding of natural numbers represents them as their own
induction functions. The Church Encoding of 'two' IS @tt{(Pm2PSm (Pm2PSm
Pz))} where the proof for zero and the proof for the successor are
passed as arguments.

In the same way the essence of booleans is choice, the essence of
natural numbers is induction.

Isn't that awesome?

As an aside, Church Encoding is Ur-Object-Oriented Programming. For
more on this, read William Cook's great paper:
@link["http://www.cs.utexas.edu/~wcook/Drafts/2009/essay.pdf"]{On
understanding data abstraction, revisited}.

By the way, if you use this code at home, make sure you put the code
in this order:

@chunk[<*>
       (define UNIT (λ (x) x))

       <booleans>
       <if>
       <real-boolean-example>

       <and>
       
       <numbers>
       <addition>

       <nat_ind>]
