#lang scribble/manual
@(require (for-label racket/base
                     rackunit
                     racket/list)
          "../post.rkt")

@title{How to Write a Good Thesis Proposal}
@categories["Academia" "Computer Science"]

I often give the same criticism and advice to graduate (Masters and
Ph.D) students who are writing their first thesis proposal. This blog
post attempts to refine that advice into a single concise document.

@(the-jump)

@section{The basics}

The point of a @emph{thesis proposal} is to @emph{propose} a
@emph{thesis}. There is no other purpose to it. It is not to argue
that you are smart. It is not to argue that you have a plan. It is not
to argue that your work will save the world. It is simply to
@emph{propose} a @emph{thesis}. You may find it useful to do some of
those things, in a way, during the proposing, but never forget what
the point of @emph{thesis proposal} is.

The other thing that you need to understand before writing a thesis
proposal is that it provides the foundation of how your dissertation
will be evaluated. In that sense it is contract-like. The things you
propose you must actually do. Therefore, you should be careful and
clear about what you propose.

Given that you understand the purpose of a thesis proposal, you need
to understand what it takes to credibly propose your thesis.

@section{Step A. What is your thesis?}

A thesis is an intellectual proposition or claim about truth. A thesis
is not a question---it is an answer. A thesis is not a project---it is
the reason a project is done. A thesis is not a problem---it is
proposed solution.

Here are some theses:
@itemize[

@item{@link["http://en.wikipedia.org/wiki/P_versus_NP_problem"]{P is equal to NP.}}

@item{@link["http://en.wikipedia.org/wiki/G%C3%B6del%27s_incompleteness_theorems"]{Any effectively generated theory capable of expressing elementary arithmetic cannot be both consistent and complete.}}

@item{@link["http://www.ccs.neu.edu/~shivers/papers/diss.pdf"]{Control-flow analysis is feasible and useful for higher-order languages.}}

@item{@link["http://www.cs.yale.edu/publications/techreports/tr364.pdf"]{Ordinary scientific programs can be compiled for a new parallel architecture called VLIW (Very Long Instruction Word), yielding order of magnitude speedups over scalar architectures.}}

@item{@link["http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.29.4871"]{Operating systems can provide fundamental services an order of magnitude more efficiently than traditional implementations.}}         

@item{If it is raining, then the grass is wet.}

]

Here are some non-theses:
@itemize[
@item{I will study whether frobnozzles are composable with nobfrozzles.}
@item{I will implement a frobnozitcator for the Java programming language.}
@item{Understanding frobnozzles is important.}         
]

The most important part of writing a thesis proposal is understanding
what your thesis is. There are many people who just start working and
figure what they've discovered later. This is bad science. It may
create knowledge, but it is bad science. A thesis is also called
a "hypothesis". You should know what it is before you start.

@section{Step B. State your thesis}

The first sentence of your proposal should be your thesis. You should
write it down clearly and succinctly. It tends to be the case that a
single simple sentence is the best form for a thesis because it forces
you to be precise and tightly scoped in your claim.

Naturally, your thesis will be highly technical and complicated to
understand. Therefore, after clearly stating the thesis, your job is
to explain what the thesis means. 

For example, if your thesis were "P is equal to NP", then you need to
explain (a) what "P" is, (b) what "NP" is, and (c) what it means for
these things to be equal.

For example, if your thesis is "frobnozzle analysis is feasible and
useful for nobfrozzled languages", then you need to explain (a)
what "frobnozzle analysis" is, (b) what it means to be "feasible", (c)
what it means to be "useful", and (d) what "nobfrozzled languages"
are.

In the process of explaining what your thesis means, you will
naturally do two things:
@itemize[

@item{You will give the background and context of the area of research
you are working in.}

@item{You will establish your understanding of that area and partially
establish your expertise and capability to answer the truth of the
thesis.}

]

These things are valuable and are often high on the list of goals that
advisers, committees, and graduate programs have for students who are
proposing theses. However, it is important to realize that this is not
the @emph{point} of a thesis proposal. Instead, in the course of
writing a good thesis proposal, it is natural to express evidence of
these things.

@section{Step C. Explain how to know the truth of your thesis}

Since your thesis is a proposition, and since the purpose of your
graduate work is to verify the truth of that proposition, the next
most important thing your thesis proposal should contain is a
description of how to know if this particular proposition is true.

For example, if your thesis were "P is equal to NP", then you would
explain what constitutes a proof of this statement. In this case, you
might say that a polynomial-time solution to a NP-complete problem
would constitute proof.

In this part of your proposal, it is often useful to discuss how to
verify the negation of your thesis, because it provides a useful
rhetorical contrast.

In this part of your proposal, you continue to establish your
understanding of the area and your expertise. For example, it is often
useful to discuss how similar theses have been verified.

This part of the proposal is the most contractual part, because it
lays out an evaluation framework for the work that you ultimately turn
in.

It is often the best and most clear place to define your
non-scientific standards. For example, if your thesis is "frobnozzle
analysis is feasible and useful for nobfrozzled languages", then the
terms "feasible" and "useful" are probably @emph{squishy} and could be
interpreted differently by different people. In this part, you would
clarify that "feasible" for you means that the average zombified
freshman can do it and that "useful" means that it saves on average
ten zorkmids given a set of benchmarks you layout in the
proposal. (This is not to say that you should not give some
explanation of these ideas before, just that that you should solidify
expectations in this piece.)

@section{Step D. Establish the unknown truth status of your thesis}

There are many coherent and clear theses that have verification
procedures that are not worthy of graduate work. One large class is
the set of theses where the truth of them is already known or easily
knowable. Therefore, after explaining @emph{how to know} if your
thesis is true, you should explain that the answer is @emph{not
known.} From a proof-theoretic perspective, you first explain what
proof system will verify the claim and then you argue that no proof is
extant.

It is common to use sections like this to discuss why existing
solutions to similar problems do not apply to the study of your
thesis.

This part is the final nail in the argument that you are a burgeoning
expert in the research area of the thesis, because you demonstrate
your breadth of understanding that the thesis is currently unknown and
what similar problems have been solved.

Something that many proposal review committees claim to care about
is "does anyone care about your work?" In my opinion, the thing of
value that this question is getting at is "is this really an unknown
thesis?" In my opinion, the answers to scientific questions and the
increase in humanity's knowledge of truth is enough to care about
anything. I don't like to directly answer the "who cares" question,
because I think it can blind us to fundamental research and it
encourages us to follow fads and human tastes.

@section{Step E. Sketch a plan (optional)}

Although it is not strictly necessary for a thesis proposal to contain
any argument that you are capable of answering the truth of the thesis
in a reasonable time, it can be useful for you and some proposal
review committees require it. It can be useful to you, so you don't
bite off more than you can chew. But, because it is not strictly
necessary, in my opinion, you should limit your plan to a sketch and
you should try to integrate it as much as possible into your
discussion of how to verify the thesis---essentially, you describe a
sketch of what method @emph{you} will use to verify it.

I think the practice of requiring elaborate or detailed plans is
destructive to science, because it encourages late proposals where a
lot of the work is complete, the thesis is a post-hoc construction,
and the verification is designed after what is expedient rather than
what is in principle good.

@section{Conclusion}

Do not be blinded by inane suggested thesis proposal outlines and
requirements. Understand what the actual purpose of a thesis proposal
is and do that.

Finally, this discussion merely explains what a thesis @emph{is} and
@emph{how to propose} one for graduate study. Whether the thesis you
have in mind is---(a) good science, (b) significant science, or (c)
computer science research---are totally different questions with their
own answers. Different committees are reviewers are likely to have
very different standards on these questions and could approve or fail
your proposal for failing to satisfy those points, despite your
proposal being well-formed.

@bold{Acknowledgments:} I see this whole post as a restatement and
backwards-reasoning from
@link["http://www.ccs.neu.edu/home/shivers/diss-advice.html"]{Olin
Shivers' excellent advice about dissertations}.

@(the-end)
