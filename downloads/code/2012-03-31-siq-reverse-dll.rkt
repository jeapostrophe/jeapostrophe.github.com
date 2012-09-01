#lang scribble/lp
@(require "../../post.rkt"
          (for-label racket/base
                     rackunit
                     racket/list))
@yaml{
---
layout: post
title: "Stupid Interview Questions - Introduction + Reversing a Doubly Linked List"
comments: true
categories: 
- Stupid Interview Questions
- Racket
---
}

Stupid interviews ask applicants to write programs on whiteboards or
pieces of paper without the resources that are normally available when
programming. These questions purport to be able understanding the
applicant's algorithmic design process, but the problems in question
are almost always so trivial that no interesting thinking is
necessary. Worse, many of these questions are really more about
knowing inane tricks than having good design abilities.

Some students in my lab are always talking and worrying about such
problems. I like to provide unnecessarily silly solutions to some of
the questions. In this introduction to the series:

How do you reverse a doubly linked list?

@more

# The Obvious

The naive implementation looks like this:

@chunky[<naive-impl>
 (define (dll-reverse! l)
   (define head (dll-head l))
   (let loop ([last #f] [current head])
     (when last
       (set-node-last! last current))
     (when current
       (define next (node-next current))
       (set-node-next! current last)
       (loop current next)))
   (set-dll-head! l (dll-tail l))
   (set-dll-tail! l head))]

The thing to notice about this implementation is that it takes O(n)
time and is a tiny bit hairy in the order that the effects have to
happen in, plus the swapping at the end.

I assume that getting this right is what interviewers are looking for
when they ask a question like this. It would be even better if they
asked about how you would validate that your code worked. Here's what
I did:

@chunky[<naive-tests>
 (define (dll-test make-dll dll-cons! dll-snoc!
                   dll-fold dll-rfold dll-reverse!)
   (define c123 (make-dll))
   (dll-cons! 2 c123)
   (dll-cons! 1 c123)
   (dll-snoc! 3 c123)
   (check-equal? (dll-fold cons empty c123)
                 '(1 2 3))
   (check-equal? (dll-rfold cons empty c123)
                 '(3 2 1))
   (dll-reverse! c123)
   (check-equal? (dll-fold cons empty c123)
                 '(3 2 1))
   (check-equal? (dll-rfold cons empty c123)
                 '(1 2 3))
   (dll-cons! 4 c123)
   (dll-reverse! c123)
   (dll-cons! 0 c123)
   (check-equal? (dll-fold cons empty c123)
                 '(0 1 2 3 4))
   (check-equal? (dll-rfold cons empty c123)
                 '(4 3 2 1 0)))
 (dll-test make-dll dll-cons! dll-snoc!
           dll-fold dll-rfold dll-reverse!)]

(Remember how we parameterize these tests over the implementation of
the functions. We'll pass in different implementations later.) And, by
the way, this assumes we have this definition of doubly-linked-lists:

@chunky[<dll>
 (struct node (last element next) #:transparent #:mutable)
 (struct dll (head tail) #:transparent #:mutable)
 (define (make-dll) 
   (dll #f #f))
 (define ((make-dll-cons!
           dll-head node set-node-last!
           dll-tail set-dll-tail! set-dll-head!)
          e l)
   (define head (dll-head l))
   (define new (node #f e head))
   (when head
     (set-node-last! head new))
   (unless (dll-tail l)
     (set-dll-tail! l new))
   (set-dll-head! l new))
 (define dll-cons! 
   (make-dll-cons!
    dll-head node set-node-last!
    dll-tail set-dll-tail! set-dll-head!))
 (define dll-snoc! 
   (make-dll-cons! 
    dll-tail
    (λ (last element next) (node next element last))
    set-node-next! dll-head
    set-dll-head! set-dll-tail!))
 (define (make-dll-fold dll-head node-next)
   (define (dll-fold cons empty node)
     (if node
       (cons (node-element node)
             (dll-fold cons empty (node-next node)))
       empty))
   (λ (cons empty list)
     (dll-fold cons empty (dll-head list))))
 (define dll-fold (make-dll-fold dll-head node-next))
 (define dll-rfold (make-dll-fold dll-tail node-last))]

But, remember, my whole goal is to show silly ways to "solve" these
interview problems... so let's think of a trick.

# The Trick

An important trick that functional programmers should always be ready
to employ is delaying. Rather than actually doing work, just record
that you should do in the future, so future operations will act as-if
the operation has been done, or perhaps do (some) of it for you.

In this case, we'll make @racket[dll-reverse!] O(1) by simply
recording that we should consider the list reversed for all future
uses. 

@chunky[<rdll-impl>
 (define (rdll-reverse! l)
   (set-rdll-reversed?!
    l (not (rdll-reversed? l))))]

This, of course, assumes that the rest of the doubly-linked-list code
is ready to pay attention to this flag. Luckily, it is pretty easy to
do that, without really writing anything again:

@chunky[<rdll>
 (struct rdll (reversed? dll) #:transparent #:mutable)
 (define (make-rdll)
   (rdll #f (make-dll)))
 (define-syntax-rule
   (define-rdll (id arg ... rl) reversed-dll normal-dll)
   (define (id arg ... rl)
     (define l (rdll-dll rl))
     (if (rdll-reversed? rl)
       (reversed-dll arg ... l)
       (normal-dll arg ... l))))
 (define-rdll (rdll-cons! e rl) dll-snoc! dll-cons!)
 (define-rdll (rdll-snoc! e rl) dll-cons! dll-snoc!)
 (define-rdll (rdll-fold cons empty rl) dll-rfold dll-fold)
 (define-rdll (rdll-rfold cons empty rl) dll-fold dll-rfold)]

Now that that's all setup, we can re-run the earlier tests with these
new functions:

@chunky[<rdll-tests>
 (dll-test 
  make-rdll rdll-cons! rdll-snoc!
  rdll-fold rdll-rfold rdll-reverse!)]

# Conclusion

This idea is the basis of a lot of efficient functional data
structures. For example, if you want to make @racket[append] fast,
then just store "append nodes" in your "list". ([These are
called "conc" lists.](http://news.ycombinator.com/item?id=814632)) If
you want to make @racket[snoc]/@racket[last] fast, then store two
lists---one starting from the head and one starting from the
tail---and deal with one going empty when you get to it.

# Exercises

1. In this code, I've used a functional/structure oriented
approach. In this case, an object-oriented approach could be more
convenient for the user, because then they would be inherently
parameterized over the set of doubly linked-list functions. For your
homework, translate this idea to an OO setting, where "fast
reversible" lists and normal lists just implement a common interface.

2. In this code, nodes and lists are distinguished from each
other. Rewrite it so there is no such distinction, while maintaining
the O(1) reversibility. (Hint: The hard part is telling each node that
the list is reversed simultaneously.) (Spoiler: Have them store a
pointer to a flag rather than a flag itself.)

# Whole Program

By the way, if you use this code at home, make sure you put the code in this
order:

@chunky[<*>
 (require rackunit
          racket/list)
 <dll> 
 <naive-impl>
 <naive-tests>
 <rdll>
 <rdll-impl>
 <rdll-tests>]

@download-link
