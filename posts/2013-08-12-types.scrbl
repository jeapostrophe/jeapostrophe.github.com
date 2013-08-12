#lang scribble/manual
@(require (for-label racket/base
                     rackunit
                     racket/bool
                     racket/list)
          "../post.rkt")

@title{The Anti-Human Consequences of Static Typing}
@categories["Humanism" "Type Systems" "Verification"]

An ancient debate among programmers and programming language
researchers, like myself, is whether it is @emph{better} to have a
static type system or not. In the age old tradition of Protagoras and
Cratylus, I attempt to thwart this technical argument by an appeal to
morality: technically superior or not, static typing is inhumane!

@(the-jump)

Although many of you, no doubt, have a clear vision of the enemy, that
common whore of mankind, I shall first describe my target: static
typing.

A static type system is a mechanism whereby an algorithm determines if
a program exhibits a property, @racket[_P], and if the property is not
found to hold, then the program is @emph{rejected}. A language will
often employ a static type system in such a way that such rejected
programs are not (or cannot be) compiled or run.

For example, my second favourite language,
@link["http://coq.inria.fr/"]{Coq}, has a type system that includes as
one of its properties strong normalization, or guaranteed
termination. (Some of the plebs out there may think it is impossible
to "disprove" the Halting Program Argument and guarantee termination,
but recall that this argument relies on a sufficiently powerful source
language, which Coq is not.)

Other more quaint languages like Java may have type systems that check
properties such as "The program does not add numbers and strings"
or "The program does not jump to non-program code location." (Of
course, most type systems really check exactly one property that
entails these, and other, properties.)

These many statically typed languages have in common the
@emph{prohibition} against programs that do not "pass" the type
checker. Indeed, this rejection is the entire purpose of the type
systems. The logic goes that if you reject "bad" programs then
only "good" programs remain, and who would want to run "bad" programs
anyways?

Unfortunately the great flaw in this argument is the presumption that
the programs rejected by the type system are only the "bad" programs
when, in fact, nearly every type system rejects infinitely many "good"
programs as well. For example, it is a rare type system that would
accept this program:

@racketblock[
(+ 1 
   (if (negative? (fahrenheit->kelvin (abs some-number)))
     "2"
     2))
]

Why would a type system reject this? Most type systems do not attempt
to reason about the different run-time paths a program can take, so
they "merge" their conclusions on the two sides of an @racket[if]
expression. The consequence of this is that all sides must return the
same thing: a number or a string, but not both. This program appears
erroneous to a type system for this reason: the outer context of the
@racket[if] mandates a number, but the @racket[if] "could" return a
string, so an error is "possible", so the program is rejected.

Of course, this example is so delicious because the type system is
wrong. If we assume that @racket[fahrenheit->kelvin] lives up to its
name, and if you don't want to assume that, just replace it with
@racket[(λ (f) (* (+ f 459.67) 5/9))], then it is impossible for the
@racket[if]'s test to be anything except @racket[false]. Thus, the
@racket[if] will always return @racket[2] and never exhibit the error.

There are some type systems that would accept this program,
however. For instance, the @link["http://sage.soe.ucsc.edu/"]{Sage}
programming language by a co-author of mine,
@link["http://cs.ucsc.edu/%7Ecormac"]{Cormac Flanagan}, dispatches
formula such as "Is that expression ever negative?" to a totally
automated theorem prover that might easily check this particular
expression and therefore allow it through. (By the way, I don't know
if Sage actually does accept this, but something like it certainly
could.)

However, we can imagine more and more absurd @racket[if] statements
that we would have no doubt that a type system could not analyze. In
the final analysis, we may appeal to the Gödel Incompleteness Theorems
and conclude that in every logic there are statements which are
@emph{true}, but not @emph{provable}, or @emph{provable}, but not
@emph{true}.

And this is @bold{the ultimate problem with type systems}: in their
quest to reject "bad" programs, they must reject "good" programs as
well because they cannot prove their "goodness".

A corollary, which I won't go into detail on, is that the "goodness" a
type system purports to verify is only related to its specific
properties and not the human ends of the software. For instance, even
Haskell will not check if your physics simulation is actually the
correct one, it will only check if you correctly applied your monad
transformer correctly on the right multi-parameter type class
instance. (This, by the way, is why I prefer Coq, because it allows
actual correctness verification is a static way.)

Until now, I have merely been preaching the technical consequence of
how type systems and logic work. But now, I shall go for the jugular.

Advocates for static typing are anti-human, because they argue that
the @emph{only} programs we should allow to run are the ones that have
been verified by machines! In contrast, the freedom fighters contra to
these typing terrorists argue that humans can perform analysis and
decision making as well!

The villains of static typing hegemony point to the line where their
algorithms fail and say, "This is the line and we shall go no
further!", but the heroes of the dynamic few look upon the wide
horizon and say, "I shall go and seek a new land flowing with milk and
honey."

My own belief is that verification is good and we should use it
whenever we can, but we should not let human accomplishment be held
back by a fear of the unverifiable. Once we know that the way is
clear, we can try to encode our human wisdom and verify more. We
should blaze the frontier @emph{and} return and build roads so that
feebler minds can trust in the highways we create. I am reminded by
the classic Mormon proverb: "Trust in God, but brush your teeth."

I believe that this humanistic ideal is exemplified in Racket, the
world's only full spectrum programming language, where the human mind
and static analysis are wed together to complement each where the
other fails. We have not yet achieved the dream of a Verified Racket
where you can conduct Coq-style program verification, but we may yet
be on
@link["http://en.wikipedia.org/wiki/Mount_Pisgah_(Bible)"]{Mt. Pisgah}
and can see it afar off.

@section{Yo! It's almost time to go!}

But first let's remember what we did today!

The sinister bands of mongrels known as static typing advocates trust
in the weak arm of silicon that leads them astray with false promises
of freeing them from "bad" programs and delivering them into "good"
programs.

This is a necessary situation due to the incompleteness of logic in
the face of powerful programming languages.

Racket attempts to skirt this situation by respecting humans and
providing analysis tools, for those humans to use.

@(the-end)
