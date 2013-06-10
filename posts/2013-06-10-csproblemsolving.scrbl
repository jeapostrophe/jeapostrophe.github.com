#lang scribble/manual
@(require (for-label racket/base
                     rackunit
                     racket/list)
          "../post.rkt")

@title{Computer Science & Problem Solving}
@categories["Education" "Computer Science"]

One of my projects is trying to improve computer science education in
Utah public schools. As part of that effort, I think about why this is
a worth goal a lot. In order to help me solidify these thoughts, I
wrote the following essay. I think it captures a major reason why
computer science education is valuable for all. I think it also
captures a big slice of what computer science is. Although I don't
think it is an exhaustive explanation of either thing.

@(the-jump)

@section{Introduction}

Every secondary education student, no matter what field or vocation
they pursue, must be able to systematically solve problems with the
confidence that their solutions are correct. Furthermore, problem
solvers are more successful and fulfilled when they can bring their
creative interpretations of problems to bear on their solutions. Thus,
we take it as a necessity of a secondary education to provide
creative, problem-solving opportunities and give students an
understanding of a systematic process of solution design and checking.

Traditionally, educators have thought mathematics and science
curricula provide for these goals, although they recognize that arts
curricula have a good influence.

@section{Mathematics}

In traditional mathematics curricula, the approach to @emph{creative
problem-solving} is best exemplified by "word problems". In a "word
problem", prose describes a human scenario and the student must
discover how to model the scenario as a mathematical equation whose
solution solves the human challenge. Mathematics itself is the system
of design. Students check their solutions with tools like graphs,
tables, and by looking at properties such as "Does the slope make
sense given our understanding of the human scenario?" Students are
creative in as much as they discover ways to understand prose problems
differently.

There are a four ways where this approach is lacking and where it is
not applied thoroughly.

First, "word problems" are scarce in math courses. Instead, they are
an "extra" atop the general mechanical skills of solving mathematical
equations. This means that, in general, math course neglect the
@emph{problem formulation} step of problem solving in favor of the
@emph{solution modeling} and @emph{solution checking} steps. This
leads, in part, to the common refrain of students, "When will I ever
use this?"  In this complaint, they are saying "What are the human
problems this solves?"

Second, "word problems" are generally tightly coupled with particular
mathematical equation forms. For example, in the chapter on the
quadratic equation, all "word problems" are quadratic equations in
disguise. This limits the potential creativity of students, because
there is really only one way to model a given problem. Furthermore,
this means that a @emph{systematic design process} is not taught,
because each human scenario is effectively already paired with a
solution. In addition to not being able to creatively express their
solution ideas, students cannot learn how to discover solution
paradigms; i.e., they do not learn how to recognize when to use the
quadratic equation.

Third, checking that a particular mathematical solution is correct is
error-prone and relies on the student's ability to perform the needed
operations manually. For example, a student may recognize correctly
that a scenario corresponds to the quadratic equation, but then fail
to solve the equation correctly. From a problem-solving perspective,
they were a success, because they modeled the problem in a way that
the solution is obvious (i.e. in the real world, computers can solve
the quadratic equation correctly every time); but from a typical
mathematics curriculum perspective, they were a failure, because they
carried out the arithmetic incorrectly. In other words, mathematical
solutions are typically @emph{mechanical}, but not @emph{mechanized}.

Finally, mathematics courses in secondary school are rarely about
things other than numbers. Instead of solving problems directly on
human values, they enforce an abstraction to numbers (and sometimes
functions, represented as graphs or charts). This limits the scope of
problems they can discuss. It disconnects the student and the math
from the human problem. And thus, it contributes to the "When will I
ever use this?" phenomenon.

In summary, although mathematics can, and often does, engage
@emph{creative, problem-solving} and @emph{systematic design and
checking}, the typical curriculum is not designed for this
end. Instead, math curricula's goal is to produce mastery of
mathematical methods.

@section{Science}

In traditional science curricula, the approach to @emph{creative
problem-solving} is best exemplified by laboratory experiments. In a
lab experiment, a student performs a rigorous and structured process
of performing experiments, gathering and recording data, and verifying
hypothesis about the experiment afterwards.

In general, in secondary school, these experiments are studies of the
"great works" of science and not problems where students are
discovering the solution themselves. Similarly, it is typical for a
course to present the solution in a superficial level; for example,
the exact mathematics of physics are rarely used in secondary school
because teachers assume students do not have calculus knowledge.

In these ways, science classes are like mathematics in not stressing
creativity or solution design. Unlike mathematics, they do not stress
a solution "model" (equations for math) because modern models for most
natural phenomena are too complicated. Instead, where science classes
excel is at teaching a systematic process of checking a solution's
correctness.

@section{Arts}

The arts and vocational courses are rarely understood to be about
@emph{creative problem-solving}. Instead, they are either seen as
about creativity-qua-creativity or work skills. Yet, most of them
present ideal problem-solving scenarios: a student faces a human
challenge
(normally their own) and a large toolbox of possible solution
approaches with which they must construct a solution and satisfy the
teacher of its correctness. This is a maximally creative environment
where a student has total control over the @emph{solution design}
step.

These environments are typically lacking relative to the ideal in
their use of systematic processes of either design or checking and in
their ability to provide solution models that generalize beyond a
small number of examples.

@section{Computer Science}

Computer Science (CS) is the study of problem solving and generalized
solutions. In CS, we understand problem solving to have five steps:

@subsection{Problem Formulation}

When confronted with a human challenge, our first task is to
understand the causal relationship between a description of the
problem instance and a description of a particular solution. For
example, if we want a solution to planning the price of movie tickets,
a problem instance would be a set of information about a particular
theatre and a solution instance would be a ticket price schedule. We
write this as

@verbatim{Solution : Problem Instance -> Solution Instance}

This means the "Solution" transforms "Problem Instance"s to "Solution
Instance"s. For instance,

@verbatim{MoviePricePlanner : TheatreData -> TicketPrice}

Computer Science provides a vocabulary for turning human information
(such as information about a movie theatre) into computer data (such
as particular numbers). Computer Science abstracts over different
domain values, such as numbers, colors, images, etc, which are
generally referred to as "atomic data". Computer Science then provides
a framework of combining data, namely as: fixed-sized data
(structures, e.g. database records), mixed data (interfaces,
e.g. circles and square are shapes), arbitrarily large data (lists,
e.g. databases, and binary trees, e.g. ancestry chart), and
intertwined data (n-ary trees, e.g. descendantry chart). This
framework is incredibly compact (just these four kinds in nearly all
programs[*]) but incredibly powerful.

[*] The only other kinds of data that computer scientists have
discovered are co-inductive (potentially infinite) or transfinite
(necessarily infinite), but these are esoteric.

Compared to other fields, CS is unique in making this an explicit step
in problem solving. Math in secondary school does not go beyond
numbers, so it is not an issue to decided how to represent data. While
science and arts typically do not expose a model wherein problem
formulation is relevant.

Furthermore, there are typically a large number of ways to represent
the human information as computer data for a given problem. Most
revolve around bringing students further up the hierarchy of kinds of
data (atomic to compound to mixed to ...) instead of through a series
of particular data configurations. Furthermore, each point in the CS
data hierarchy contains all prior kinds of data, so at every point in
the curriculum a student needs to decide how to use each of the kinds
of data they know about.

@subsection{Planning Solution Checking}

After understanding the kind of data that a solution deals with, CS
mandates that we determine how to check a solution. Similar to the
scientific method, a computer scientist designs a set of experiments
to check if a solution is correct. These are typically called "test
cases" and are themselves programs that a computer can run
automatically. For instance,

@verbatim{MoviePricePlanner( 4 screens, 100 seats per screen, $500 rent, $70
electricity bill ) = $8 per ticket}

A CS student would create multiple such "test cases" for a solution,
each corresponding to a different human problem. A solution is a good
solution when it can automatically solve all similar problems and test
cases capture this idea.

Unlike traditional sciences, where the emphasis is on carrying out and
planning a single experiment, CS emphasizes how to build sets of
experiments that establish more general correctness properties. For
instance, if a problem has three different variants (like negative,
zero, and positive numbers), then there should be at least six
different test cases: two for each variant. (It is important that
there are at least two to show that the solution is general and not
specific to the details of the first test case.)

@subsection{Solution Structuring}

CS provides a mapping between data structures to solution
structures. For example, if during step 1 a student decided that there
were two kinds of data inputs, then CS mandates that the program will
have to distinguish between these two different kinds of data as one
of its steps.

This framework of turning problem understanding into solution
structure is a kind of systematic design that computer science
teaches. Each kind of data structure (fixed, mixed, large, and
intertwined) is pair with exactly one control structure (matching,
switching, recursion, and mutual recursion); this means that the
framework is compact and natural to teach.

@subsection{Applying Domain Knowledge}

Since CS is just about understanding solutions and their structure, it
rarely has anything to say about particular problems. Instead CS must
rely on external domain knowledge to actually do work. For instance,
while CS would maintain a certain structure for a problem with
negative, zero, and positive numbers, it has nothing to say about how
the operation on the numbers should occur. Instead, expertise in the
domain, say physics or geometry, would mandate one operation or
another.

In other words, while there are a large number of CS solutions
involving numbers and images, CS isn't about the numbers or images. CS
is about structuring a problem and its solution, then checking its
correctness. The best CS tries to look beyond particular domains and
discover structures that are not obvious "on the ground". For example,
computer scientists have discovered that spell checking and gene
sequencing are actually the same problem through this process.

In this way, CS is maximally applicable because a course can combine
it with any domain of interest to the student, teacher, or society.

@subsection{Performing Solution Checking}

Finally, CS mandates that we have an unambiguous way to evaluate
whether a particular solution (constructed in steps 3 and 4) satisfies
the experiments (constructed in step 2). The most traditional way to
do this is with a computer program that automatically executes a
program written in a computer language. (Although technically this is
not necessary; any unambiguous process is a computer to a computer
scientist. This is how we can "run programs" on organizations,
biological systems, groups of people following rules, etc.)

When this mechanical process is entirely mechanized, like on a
physical computer, there is no possibility for a potential solution to
be incorrectly judged because of a human's mistake. This is unlike the
situation in math or sciences where a human can make a mistake
carrying out the arithmetic or experiment and "discover" that the
quadratic equation doesn't hold or Earth's gravity isn't 9.78 m/s^2.

@section{Summary}

Since CS is nothing more than the study of problem solving itself, it
should be unsurprising that it teaches it more explicitly and
directly. In some sense, CS combines all the problem solving aspects
of other fields: every CS problem is like a big word problem where you
must combine different math, science, and arts principles at different
stages, then follow the scientific method to check its correctness.

One great advantage of CS is that it integrates multiple areas in this
way, thus creating more realistic problem solving scenarios and
requiring more understanding to apply the right principle at the right
layer. This would also be a great disadvantage, as well, because it
means CS problems are more complicated; however, because of this, CS
provides a systematic framework of design and checking to deal with
the complexity.

This does not mean, however, that math, sciences, or arts are less
than CS.

First, CS requires a domain such as those provided by math, sciences,
or arts.

Second, mathematical models define the framework underneath CS, so as
CS students reach more advanced material, it is necessary to
understand more advanced math. In some sense, CS is math generalized
to all kinds of data and restricted to mechanical computable
equations.

Third, the rigorous environment of CS and computer programs is
unforgiving of experimentation and mistakes. This strictness is dual
to how problem solving in the arts is incredibly free-flowing and
unlimited.

Instead of pitting CS against other fields, it is best to understand
its ability to complement them. For example: students can automate
solutions to mathematical word problems as computer programs; students
can model scientific experiments as test cases of the
scientific-theory-as-solution; CS teachers could adopt the traditional
science approach of studying and recreating "great works" by exploring
famous CS solutions; the low cost of virtual production relative to
physical production can augment and accelerate the creativity of arts;
and so on.

@section{Yo! It's almost time to go!}

But first let's remember what we did today!

We learned that Computer Science is the science of problem solving and
teaches it directly, as opposed to other fields which include and rely
on problem solving but are ultimately about something else, so only
teach problem solving implicitly.

@bold{Acknowledgments.} Everything good in this post comes from
@link["http://www.ccs.neu.edu/home/matthias/"]{Matthias Felleisen},
@link["http://www.eecs.northwestern.edu/~robby/"]{Robby Findler},
@link["http://www.cs.utah.edu/~mflatt/"]{Matthew Flatt}, and
@link["http://cs.brown.edu/~sk/"]{Shriram Krishnamurthi}. Everything
bad comes from my inability to communicate what the beauty of
@link["http://www.htdp.org/"]{How to Design Programs} is.

@(the-end)
