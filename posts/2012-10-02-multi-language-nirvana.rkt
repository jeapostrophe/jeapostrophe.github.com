#lang scribble/lp
@(require (planet ryanc/scriblogify/scribble-util)
          (for-label racket/base
                     rackunit
                     racket/list))

@literal{
---
layout: post
title: "The Multi-language Nirvana"
comments: true
categories:
- Coq
- Ocaml
- Racket
- Make
- CPP
---
}

A student and I are working on a
@link["https://github.com/ChaseWhite3/linear-logic"]{verified
linear-logic theorem prover}, in Coq. We came up with a very cute way
to integrate many languages in the production of this tool and this
post is about that multi-language nirvana.

@(the-jump)

The first language we use in this project is
@link["http://coq.inria.fr/"]{Coq}, the wonderful proof assistant that
I used for my dissertation and use whenever possible. The main linear
logic theorem prover is
@link["https://github.com/ChaseWhite3/linear-logic/blob/master/basic.v"]{written
in Coq}. It's divided into three pieces: (1)
@link["https://github.com/ChaseWhite3/linear-logic/blob/master/basic.v#L35"]{an
embedding of linear logic} as an inductive theory; (2)
@link["https://github.com/ChaseWhite3/linear-logic/blob/master/basic.v#L289"]{a
set of decision procedures}, one for each kind of linear logic proof,
that independently reach true conclusions, assuming an oracle that can
handle all other kinds of proofs; and (3)
@link["https://github.com/ChaseWhite3/linear-logic/blob/master/basic.v#L799"]{the
glue that ties them all together} that we prove sound overall.

The set up gives us a function that when given a linear logic problem,
expressed as a list of assumptions, it will return a list of all
formulas that can be proved from it. However, we'd like to run this
program efficiently with @link["http://caml.inria.fr/"]{Ocaml}, so we
@link["https://github.com/ChaseWhite3/linear-logic/blob/master/basic.v#L941"]{extract
it}, giving the normal Ocaml types as replacements for the normal Coq
types, such as booleans and lists.

However, this program isn't really useful by itself, because it needs
to (a) have a specified problem and (b) be called with input and then
the output printed in some way. Rather than go through the pain of
creating a module, we use the wonderful
@link["https://en.wikipedia.org/wiki/C_preprocessor"]{C preprocessor}
to stitch together
@link["https://github.com/ChaseWhite3/linear-logic/blob/master/linearLogic.ml.in"]{these
various pieces}: the prover, the problem specification, and the code
that calls them.

However, it is inconvenient to specify the problem directly in the
Ocaml data type syntax, because it is very verbose. We'd prefer to
specify it using
@link["https://github.com/ChaseWhite3/linear-logic/blob/master/small.rktd"]{simple
S-expressions}. So, we combine that with a simple
@link["http://www.racket-lang.org/"]{Racket} program
@link["https://github.com/ChaseWhite3/linear-logic/blob/master/roomer-ll.rkt"]{that
re-formats the S-expression as Ocaml}.

The beautiful
@link["https://www.gnu.org/software/make/manual/make.html"]{make
language} is used to
@link["https://github.com/ChaseWhite3/linear-logic/blob/master/Makefile"]{manage
this process} and keep everything in sync whenever we change one of
the pieces.

I like this little program because it shows how convenient it can be
to use many different languages, each for its own little purpose. The
only way that this could be better, I think, would be if all the
languages were actually just Racket underneath. Maybe some day...

