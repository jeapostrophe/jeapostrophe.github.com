#lang scribble/manual
@(require (for-label racket/base
                     rackunit
                     racket/list)
          "../post.rkt")

@title{Switching from OmniFocus to Org-Mode}
@categories["Linux" "Apple" "OmniGroup" "Emacs"]

One of my joys in life is keeping a TODO list and checking off
stuff. When I was young, I used plain text files mainly, but when I
got to graduate school, I used more automatic approaches.

In this post, I discuss my journey through various TODO list managers,
culminating in my switch from OmniFocus to Org-Mode.

@(the-jump)

My first foray into automated systems was a hand-written system called
@link["http://planet.racket-lang.org/display.ss?package=grid.plt&owner=jaymccarthy"]{The
Grid}. This was a system for keeping track of your obedience to a
daily regime. Check out this
@link["http://planet.racket-lang.org/package-source/jaymccarthy/grid.plt/1/2/tour/index.html"]{tour}
to see how it worked. You would add a list of things to do and then
there'd be a box to check of every day. From my Grid in the tour, you
can see that mine were: Kiss Claire (my girlfriend at the time), Eat a
new meal, Wake up before 7am, Goto sleep before 11pm (I wasn't good at
that), Cardio-exercise, Weight training, SET (the card game), Shuffle
cards (I wanted to learn how to, so I practiced every day), Write
left-handed, Speak a foreign language, Write a foreign language, Read
a scientific paper, Program, Read literature, Dance!, Smell the
flowers, Talk to my family, and Pray.

This was awesome and I really enjoyed it.

Eventually, I wanted to keep track of more things, especially longer
term things, so I started using
@link["https://www.omnigroup.com/products/omnifocus/"]{OmniFocus} for
the Mac. It was pretty awesome and I enjoyed it. Eventually when the
iPhone came out and I got one, I got it for the iPhone. But it was
pretty terrible on there, because the synchronization time was
unbearable for my huge list.

I switched to using OmniFocus instead of the Grid when I realized that
I could make tasks repeat on a daily basis, and then have
their "scheduled" time also repeat and be in the morning. This would
make it so that when I completed a task for Monday, it would repeat
again on Tuesday, but be hidden until then. Unfortunately, this made
the list bigger and bigger every day, because each completed task was
an entry in the OmniFocus database.

Nevertheless, I totally converted over to OmniFocus.

I enjoyed it for a while, but some things started to be very
annoying. The main one was that it was incredibly slow: Synchronizing
between my computers was slow, going from an item in Agenda mode to
its place in the TODO list was slow, searching was INCREDIBLY slow,
and there weren't enough keyboard commands.

I researched some other options and eventually decided to experiment
with switching to Org-Mode inside of Emacs.

The main thing to do was to make the interface look as much link
OmniFocus as possible. Most of the options built-in to Org-Mode
weren't good enough though, but there were two hooks that I could use:
the @tt{org-agenda-before-sorting-filter-function} and
@tt{org-agenda-cmp-user-defined}.

@tt{org-agenda-before-sorting-filter-function} is run on all the items
that will be displayed on the agenda before they are sorted... as the
name suggests. I programmed mine to (a) remove "TODO" from the
displayed text, because otherwise they all would display it, (b) give
it a color based on when it was due modeled after the OmniFocus
colors, and (c) remove it if it was not passed the scheduled time. (c)
is important, because even though Org-Mode claims to have an option
that does this, it only looks at the scheduled @emph{date}, not the
scheduled @emph{time}.

@tt{org-agenda-cmp-user-defined} is used to order them and you can't, by
default, order by the deadline, which I wanted.

Then, I made it so the keybinding I use to view the TODO list (from
anywhere in Emacs) automatically starts column mode and the column
format hides everything except the deadline.

This has solved every problem I had with OmniFocus: it does everything
the same but is lightning fast.

There's one feature it doesn't have that I like, but that I've worked
around. OmniFocus has parallel and ordered task lists, but Org-Mode
only has parallel. This just means duplicating some deadlines.

There's a feature that neither have, but with Org-Mode I can add it
myself: the ability to skip the weekend for daily repeating tasks. I
hope to do this eventually, but I don't have it yet.

You can see my Org-Mode customization starting on
@link["https://github.com/jeapostrophe/exp/blob/master/.emacs.el#L518"]{L518
on Github}.
