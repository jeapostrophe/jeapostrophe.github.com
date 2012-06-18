#lang scribble/lp
@(require "../../post.rkt"
          (for-label racket/base
                     rackunit
                     racket/list))
@yaml{
---
layout: post
title: "Delimited Pipes"
categories:
- Racket
- Continuations
---
}

I love continuations. We couldn't do much of anything on a computer
without them. But, I love first-class access to
continuations (i.e. call/cc) even more.

The standard reason is that call/cc allows you to express things that
are not possible at the user-level without it. For example,
generators, implicit back-tracking search, threads, etc.

However, many of these use-cases for call/cc use mutation in an
essential way and have let some to criticize useful uses of call/cc to
be necessarily tied to mutation.

In this post, I'll show how call/cc saves use from mutation and
produces something pretty elegant.

@more

Suppose you are parsing a parenthetical language (for some crazy
reason) and you need to associate parens and then deal with the list
structure later.

The obvious way to do that is:

@chunky[<obvious>
        (define (parse ip)
          (let loop ([inside? #f])
            (match (read-char ip)
              [#\(
               (list* (loop #t) (loop inside?))]
              [#\)
               (if inside?
                 empty
                 (error 'parse "Mismatching right paren"))]
              [(? eof-object?)
               (if inside?
                 (error 'parse "Mismatching left paren")
                 empty)]
              [this
               (list* this (loop inside?))])))]

Here's a little test suite:

@chunky[<tests>
        (test 
         (parse/string "")
         => empty
         (parse/string "a")
         => (list #\a)
         (parse/string "(")
         =error> #rx"left paren"
         (parse/string ")")
         =error> #rx"right paren"
         (parse/string "b(a)c")
         => (list #\b (list #\a) #\c)
         (parse/string "(a((a)a))b(a)c")
         => (list (list #\a (list (list #\a) #\a)) #\b (list #\a) #\c))]

The problem with this solution is that it uses side-effects! Each call
to @racket[read-char] is mutation of the input-port data-structure's
next-position-to-read field. It would be more elegant to use a stream
to represent the input.

Unfortunately, the obvious stream-based solution is broken:

@chunky[<obvious-list>
        (define (parse i)
          (let loop ([i i] [inside? #f])
            (match i
              [(list* #\( more)
               (list* (loop more #t)
                      (loop more inside?))]
              [(list* #\) more)
               (if inside?
                 empty
                 (error 'parse "Mismatching right paren"))]
              [(list)
               (if inside?
                 (error 'parse "Mismatching left paren")
                 empty)]
              [(list* this more)
               (list* this (loop more inside?))])))]

The problem is that after the matching right-paren is found for a
given left-paren, you must "skip" the interleaving characters on the
recursion. In the code, the problem is the first case of the match,
where @racket[more] is used in two recursive calls.

The correct version is written monadically:

@chunky[<monad-list>
        (define (parse i)
          (let loop ([i i] [inside? #f])
            (match i
              [(list* #\( more)
               (define-values (this more-p) (loop more #t))
               (define-values (that more-pp) (loop more-p inside?))
               (values (list* this that)
                       more-pp)]
              [(list* #\) more)
               (if inside?
                 (values empty more)
                 (error 'parse "Mismatching right paren"))]
              [(list)
               (if inside?
                 (error 'parse "Mismatching left paren")
                 (values empty empty))]
              [(list* this more)
               (define-values (that more-p) (loop more inside?))
               (values (list* this that) more-p)])))]

Unfortunately, monadic programming is effectful programming, just with
more pain, because you have to do the plumbing yourself or contaminate
the rest of your program with the effectful type sewage.

If we look at the monadic program, though, we can see that the only
useful threading is between the first and second cases of the
match. The stuff after the right-paren gets passed out to the
left-paren context. Why not just implement that "piping" to the
calling context directly as a feature?

The final code will look like this:

@chunky[<pipe-list>
        (define (parse i)
          (let loop ([i i] [inside? #f])
            (match i
              [(list* #\( more)
               (define-values (more-p pipe-in)
                 (pipe (loop more #t)))
               (list* (pipe-in empty)
                      (loop more-p inside?))]
              [(list* #\) more)
               (if inside?
                 (pipe-out more)
                 (error 'parse "Mismatching right paren"))]
              [(list)
               (if inside?
                 (error 'parse "Mismatching left paren")
                 empty)]
              [(list* this more)
               (list* this (loop more inside?))])))]

The crucial point is that when we recur, looking for the right-paren,
we use the @racket[pipe] form, which allows the body to communicate
with the context. The body then calls @racket[pipe-out], which returns
a value to the context. The context receives the
value (@racket[more-p]) as well as a function to call when it should
communicate back (@racket[pipe-in]). The context then sends back the
empty list, which the body will return at the end of the list it
constructed, the call to @racket[pipe-in] returns with the final
answer from the body... the inner list.

It is fairly simple to imagine implementing such a piping-system with
concurrency: every call to @racket[pipe] creates a new thread with a
line of communication back to the calling context, which waits for
communication. This is easy to realize in code, but there are some
gross details, especially with getting exceptions to throw in the
parent:

@chunky[<pipes-as-threads>
        (define-syntax-rule (pipe e ...)
          (pipe* (λ () e ...)))
        (define pipe-channel
          (make-parameter #f))        
        (define (pipe* f)
          (define c (make-channel))
          (thread 
           (λ () 
             (parameterize ([pipe-channel c])
               (channel-put c 
                            (with-handlers ([exn? (λ (x) x)])
                              (f))))))
          (define intermediate (channel-get* c))
          (values intermediate
                  (λ (response)
                    (channel-put c response)
                    (channel-get* c))))
        (define (channel-get* c)
          (define v (channel-get c))
          (if (exn? v)
            (raise v)
            v))
        (define (pipe-out v)
          (define c (pipe-channel))
          (channel-put c v)
          (channel-get c))]

Of course, this has many hidden effects, much more than the original
port-based code! So it's not exactly an advisable way of solving the
problem.

Luckily we can get the same feature in a tiny amount of
continuation-based code:

@chunky[<pipes-as-conts>
        (define pipe-tag (make-continuation-prompt-tag 'pipe))
        (define (pipe* f)
          (let/ec esc
            (call-with-continuation-prompt f pipe-tag esc)
            (error 'pipe "did not pipe-out")))
        (define-syntax-rule (pipe e ...)
          (pipe* (λ () e ...)))
        (define (pipe-out v)
          (call-with-composable-continuation
           (λ (come-back)
             (abort-current-continuation pipe-tag v come-back))
           pipe-tag))]

The basic idea is to turn the call to @racket[pipe] into a new
continuation prompt, then @racket[pipe-out] captures the continuation
back to that point, and then aborts back to the prompt, delivering an
intermediate value and then the continuation which resumes the
computation from outside the calling context. This system is
particularly beautiful because it allows the inside to be resumed
multiple times.

In my opinion this is the perfect example of the power of first-class
continuations: we are able to seamlessly implement a powerful new
feature that no other language supports in 12 simple lines. And,
there's no mutation anywhere!

The only objection to continuations I feel reason to accept is that it
can be difficult to reason about contexts. If you're not sure that you
agree with that statement, try to figure what this returns, without
evaluating it:

@chunky[<puzzle>
        (let loop ([i 5])
          (cond
            [(<= i 0)
             empty]
            [else
             (define-values (j pipe-in) 
               (pipe (list* i (loop (pipe-out (sub1 i))))))
             (list* j 
                    (append (pipe-in (- j 1))
                            (pipe-in (- j 2))))]))]

By the way, if you use this code at home, make sure you put the code
in this order:

@chunky[<*>
        (require tests/eli-tester
                 racket/list
                 racket/match)

        (test
         (let ()
           <obvious>
           (define (parse/string s)
             (parse (open-input-string s)))
           <tests>)

         (let ()
           <obvious-list>
           (define (parse/string s)
             (parse (string->list s)))
           <tests>)

         (let ()
           <monad-list>
           (define (parse/string s)
             (define-values (this more) (parse (string->list s)))
             this)
           <tests>)

         (let ()
           <pipes-as-threads>
           <pipe-list>
           (define (parse/string s)
             (parse (string->list s)))
           <tests>)

         (let ()
           <pipes-as-conts>
           <pipe-list>
           (define (parse/string s)
             (parse (string->list s)))
           <tests>

           (printf "The answer to the puzzle is... ~a\n"
                   <puzzle>)))
        ]

@download-link
