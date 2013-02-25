#lang scribble/manual
@(require (for-label racket/base
                     rackunit
                     racket/list)
          "../post.rkt")

@title{Bugs or Errors?}
@categories["Software" "Culture" "Morality" "Errors" "Testing"]

One of my greatest annoyances (@tt{#firstworldproblems}) is when
people refer to problems with software as "bugs". In this post, I
discuss what is wrong with this word and what we should say instead.

@(the-jump)

The popular tale of where the term "bug" came from is that in the
mid-1940s, during the creation of the Mark II computer, a moth was
found trapped in relay 70 of panel F, and it was causing problems with
the proper execution of the Mark II. Although apparently the term was
used as early as 1878, by Thomas Edison no less, to refer to "little
faults and difficulties" with his products, perhaps due to a reference
to the Bugaboo (or bogeyman.)

In either case, the term has the same problem: it absolves the creator
of the software of responsibility for the problem. In the case of
actual insects, it is the agency of some external force that takes a
working product and destroys it. The case of the mythical monster is
similar, with the added supernatural source.

Of course, in reality, the only one responsible for problems with
software is the programmer of the software who made a mistake. It is
their fault that the problem exists and no one else's, certainly not a
mythical beast or an uncontrollable physical force.

As programmers, we should not hide behind these myths, but should take
responsibility for our actions and the mistakes we make while
programming. If you need to refer to a problem with software you
should call it a "problem", "fault", or "error". If you are
programmer, particularly when talking about your own software, you
should own up and call it a "mistake".

I think this perspective is useful in a practical way as well: many
research projects are about trying to "reduce faults" or "catch bugs",
yet they ignore the people who are actually responsible for creating
these errors and introducing the faults. The goal of creating
error-free software will be realized when we make it easier for
programmers to: not make mistakes, notice the mistakes they make, and
recover from mistakes after they've made them. A fundamental part of
that is educating programmers to where errors come from (themselves),
how to find them, and how to not create them in the first place.

The most problematic consequence of this linguistic change is that the
term "debugging" or "using a debugger" becomes immoral. The
alternatives, however, are more descriptive and useful.

For example, a "debugger" is really just a tool for "inspecting a
program's execution" and if you need to do this, then you should
say "I don't understand what my program does, so I'm inspecting its
execution".  This, by the way, is the great flaw of "debuggers": they
are for people who don't understand their program and they don't
really increase understanding. First, they fail to create a record of
what has been learned in a repeatable way since they are so
emphemeral. Second, they do not force the programmer to think
critically about what the program @emph{should} do, just what it
@emph{does} do.

A far more effective tool is to use test cases to encode the correct
behavior of a program and check it: start with a large test case that
fails, then break it down (through substitution) into the definition
of the function that is called and turn that large test case into a
few small test cases about the pieces of the function. As you repeat
this process you will discover that some sub-test cases pass (meaning
that code is correct in this path) and some that fail (meaning the
error is in that code).

A common critique of automating testing is that there are certain
programming styles or systems that a hard to test. However, I consider
this to be a criticism of those programming styles and not testing. If
you can't isolate the pieces of your software and what assumptions
each piece relies on others to provide, then you have a bad design.

This process of modifying a program and discovering where a mistake
was made is typically called "debugging", which is an immoral term. A
far better term is "fault isolation" or "test case minimization",
because you are trying to turn a mega-failure into a micro-failure to
isolate the error to the component that needs to change. The best part
of this process of test case generation is that you create a
repository of specifications of what your software should do paired
with the assurance that it actually does these things. Thus, it
succeeds at creating understanding where "debuggers" fail.

In summary, please stop covering your sins with the scapegoat of
insects and take responsibility for your actions. When your software
has faults, isolate them and increase your evidence that the software
works by creating repeatable test cases.

@(the-end)
