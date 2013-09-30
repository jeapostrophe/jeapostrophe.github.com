#lang scribble/manual
@(require (for-label racket/base
                     rackunit
                     racket/bool
                     slideshow
                     racket/gui
                     slideshow/play
                     racket/list)
          "../post.rkt")

@title{How I wrote my RacketCon 2013 Slideshow}
@categories["Racket" "Slideshow" "Languages" "Insanity"]

At this year's @link["http://con.racket-lang.org/"]{RacketCon}, I gave
a presentation on the Racket Package System (@racketmodname[pkg]). I
used a story about a young developer learning about the package system
as a way to show how various practical problems are solved by
packages. 

However, this post is not about the content of the talk, but about how
I programmed it using the @racketmodname[slideshow] system. You may
want to download and look at
@link["https://github.com/jeapostrophe/presentations/tree/master/201309-racketcon"]{the
talk} to get a feel for what I'll be explaining. Although there is a
PDF version, it does not contain the movie on the title screen, so
there's a reason to try and run it locally.

@(the-jump)

The slideshow does two cute things. First, it starts with a movie on
the title screen that is continuously looping. Second, it contains a
dialogue and command-line interaction scripting system to show a long
interaction between the various users. I'll talk about these two
things separately.

@section{The Movie}

In the @racketmodname[slideshow], there is a library for writing
finite animations as functions of time between @racket[0] and
@racket[1] called @racketmodname[slideshow/play]. It has a fun model
that generates @racket[N] slides by discretizing and transitioning
between the slides.

Unfortunately, this means that it can't be used to loop an animation
indefinitely. (At RacketCon, I wanted to put up the title screen
during the break between talks to build anticipation.) However, there
is a function called @racket[interactive] that will embedded a
@racketmodname[racket/gui] @racket[frame%] in a slide. This is
intended to embed an "interactive" element, but I used it to embed a
@racket[canvas%] which I could then dynamically update to draw things.

I could have spent longer and made a movie playing
@racketmodname[racket/gui] widget, but for expediency, I preprocessed
the movie and turned it into a sequence of PNGs using @tt{ffmpeg}.

@section{The Dialogue System}

The more interesting system was the dialogue. Here's a snippet of code
to give you an idea what it looks like:

@racketblock[
(6slide
 (thought dev1 "I want to write a program to simulate my experience in Narshe.")
 (:emacs '("magitek.rkt"))
 (:clear)

 (thought dev1 "Now I need to share my package...")
 (command "scp magitek.rkt server:public-html/")
 (email dev1 "Please try my program at: http://terra.com/magitek.rkt")
 (:clear)

 (thought dev1 "I should expand my simulation to include the treasure house.")
 (:mkdir '("narshe"))
 (:mv '("magitek.rkt") '("narshe" "magitek.rkt"))
 (:emacs '("narshe" "lone-wolf.rkt"))
 (command "scp -r narshe server:public-html/")
 (email dev1 "Please try my program at: http://terra.com/narshe")
 (:clear)
        
 ....)
]

The functions @racket[thought] and @racket[email] create the text
boxes formatted for a thought and an email. The function
@racket[command] creates a command interaction and the functions
@racket[:emacs], @racket[:mkdir], and @racket[:mv] are just thin
wrappers around @racket[command]. Finally, @racket[:clear] clears the
terminal session. Every single one of these functions will generate a
different slide.

The way this works is that @racket[6slide] is a little programming
language interpreter. The programming model is based on mutating a
text box, a list of commands, and a file-system.

@racketblock[
(struct 6state (last-speech commands fs))
(define init-s (6state #f empty (hash)))
]

And the commands in the language are values that are created by the
functions mentioned above:

@racketblock[
(struct *speech (char text))
(struct *command (text outputs))
(struct command-clear ())
(struct fs-set (p v))
]

The interpreter is very straight-forward:

@racketblock[
(define (6exec s a)
  (match-define (6state ls cs fs) s)
  (match a
    [(? *speech?)
     (struct-copy 6state s
                  [last-speech a])]
    [(? *command?)
     (struct-copy 6state s
                  [commands (cons a cs)])]
    [(command-clear)
     (struct-copy 6state s
                  [last-speech (*speech (*speech-char ls) (blank))]
                  [commands empty])]
    [(fs-set p v)
     (struct-copy 6state s
                  [fs (hash-set* fs p v)])]
    [(? list? as)
     (for/fold ([s s]) ([a (in-list as)])
       (6exec s a))]))
]

It just does one unexpected thing, which is that when a command is a
list, they are all executed to completion. This allows me to show
multiple command line additions at the same time near the end of the
talk where the details start getting looser. For instance, I use the
following to show the command associated with an email all at once:

@racketblock[
(list (email dev1 "I just updated my package! The new source is...")
      (command "raco pkg update http://terra.com/narshe.zip"))
]

Since @racket[6exec] defines how to run one command, I just wrap them
all together calling @racket[6render], which renders the state of the
language as a slide, each time:

@racketblock[
(define (6slide . actions)
  (for/fold ([s init-s])
      ([a (in-list actions)])
    (define ns (6exec s a))
    (6render ns)
    ns)
  (void))
]

The weirdest part of the system is the way it tracks what files have
been created or moved, etc throughout the presentation. I originally
intended to display the file-system some how, but couldn't figure out a
good way to do it.

Finally, the way that I generated the character portraits was quite
painful. I downloaded sprite sheets from
@link["http://www.spriters-resource.com/snes/ff6/index.html"]{The
Spriters Resource}. Unfortunately, sheets on that site have no
metadata about where the sprites are inside and they are rarely
consistent for a given game, even when produced by the same ripper. It
makes it very inconvenient to use programmatically. So, I wrote a
little program that would open up the sprite sheet and display it as
zoomed in a possible and let you move around a cursor in a very exact
way to read off the coordinates.

Once I manually did that for all eighteen characters, I entered the
data in and wrote a little function (@racket[char-portrait]) that
extracted the appropriate piece of the sheet and then magnified it
using a scaling algorithm that does not blur at all.

This was painful to do, but I found it a bit beautiful that I could
whip up a Racket GUI program in 57 lines of code that did something
very useful for me. Of course, it is based on my
@postlink["2013-05-13-vi"]{Vi-like Graphics Editor}, so it was quite
easy to do after knowing how to do it already.

Altogether, it took me about ten times longer than the talk itself to
implement the talk. 

But it was worth it.

@section{Yo! It's almost time to go!}

But first let's remember what we did today!

If you want beautiful, unique, and consistent slides, then you should
use @racketmodname[slideshow], a domain-specific language for making
slides.

If you want to go above an beyond, then use a domain-specific
language for making a particular set of slides.

@(the-end)
