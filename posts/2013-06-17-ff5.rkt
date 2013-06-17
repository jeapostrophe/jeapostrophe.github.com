#lang scribble/lp
@(require (for-label racket/base
                     rackunit
                     racket/unit
                     racket/match
                     racket/list
                     data/heap)
          "../post.rkt")

@title{SRAM Hacking in Racket}
@categories["Racket" "Games"]

As I've mentioned before, I like playing video games and especially
old video games. I was recently playing Final Fantasy 5 and wrote a
little program to help me. This program is interesting, because it
performs a very low-level, bit-oriented task you might normally
associate with C.

@(the-jump)

@section{What is Final Fantasy 5?}

@link["http://en.wikipedia.org/wiki/Final_fantasy"]{Final Fantasy} is
a series of role-playing games from the Japanese company Square (now
Square Enix). Since the first in 1987, they've been defined by their
epic stories and technical prowess. Each game is distinct others: they
aren't set in the same universe and they don't have the same gameplay
mechanics, except in very broad strokes. Thus, Final Fantasy is a pure
@emph{brand}: when you buy one, you are just buying the product
of "the team," which has changed so much that it's just a label of
quality from Square.

Since Square is a Japanese company, its games have always been written
in Japanese originally and then translated to English. During the
early 1990s, it was not a given that a game would be translated. For
example, @link["http://en.wikipedia.org/wiki/Final_Fantasy_2"]{Final
Fantasy 2},
@link["http://en.wikipedia.org/wiki/Final_Fantasy_III"]{Final Fantasy
3}, and @link["http://en.wikipedia.org/wiki/Final_Fantasy_V"]{Final
Fantasy 5} were not released in America. (Instead,
@link["http://en.wikipedia.org/wiki/Final_Fantasy_IV"]{Final Fantasy
4} was released as @emph{Final Fantasy 2} and
@link["http://en.wikipedia.org/wiki/Final_Fantasy_VI"]{Final Fantasy
6} as @emph{Final Fantasy 3}. When
@link["http://en.wikipedia.org/wiki/Final_Fantasy_VII"]{Final Fantasy
7} was released in America, it kept its natural number.)

In October 1998, fans hacked the SNES ROM of Final Fantasy 5 to
translate it into English and released
@link["http://www.romhacking.net/translations/353/"]{a patch} so
anyone could play it. This was the first RPG to be fully translated
and I remember hearing about it immediately. I played it a little when
I was young, but didn't really get into it fully.

Fast-forward fifteen years, I decided to play it through.

@section{What does the program do?}

The game features a system where you need to fight monsters to gain
points that increase your power and abilities. There are three kinds
of points: experience that makes you stronger, gil which gives you
purchasing power, and ability points which unlock more skills. In
general, in each battle you will get a lot of experience, a moderate
amount of gil, and a tiny amount of ability points. It's strange,
because the thing which is the least useful (just getting a new
ability), the game is incredibly stingy on dolling out.

Since it is so stingy, I made a meticulous plan for how I would use my
ability points so I wouldn't get any useless abilities. I played for
about fifteen hours and then reached a point in the game where I had
the ideal place to gain ability points. I had about half of the
abilities I planned on and needed about 1,000 more points.

My ideal place allowed me to go on a little cycle:
@itemlist[
 @item{Go to an area with only one kind of enemy.}
 @item{Get in a fight, either against 2 of them or 5 of them.}
 @item{Have the first character who can attack, and who always attacks before the enemies, use a certain spell that always immediately kills them all.}
 @item{Based on the cost of the spell, this allows 8 fights before needing to get more magic power.}
 @item{Use an item that costs 600 Gil to heal.}
 @item{Repeat.}
]

This process has no risk, because the spell always works and there's
only one kind of enemy. If I assume equal numbers of two and five
enemy battles, it gives me the following every cycle:

@itemlist[
 @item{700 experience}
 @item{13,596 gil}
 @item{48 ability points}
]

Given that I had a discovered a way to exploit the game and get
infinite resources, there was only time between me and my goal.

Before reaching 1,000 points to go, I did this cycle for about two
hours while watching a movie with my wife and did about 10 cycles, or
got about 500 points. Interestingly, this is not a significant source
of experience but it is a major source of gil, which is uncommon. So,
getting to my goal would take about four hours.

I wish I could submit a proof to the game or at least write a program
that would play automatically and get what I wanted, but it is
impractical to do either of those things.

Instead, I felt justified in cheating and just giving myself what I
wanted: the 1,000 ability points. As a punishment for not actually
playing, I wouldn't give myself the gil or experience I would earn in
the course of getting the ability points.

@section{What must the program deal with?}

Super Nintendo games are each a unique piece of hardware connects
electrically to the Super Nintendo and can implement arbitrarily
behavior. They've basically just like expansion cards for PCs. (Some
games had powerful additional compute power on them. For instance,
@link["http://en.wikipedia.org/wiki/Super_Mario_RPG:_Legend_of_the_Seven_Stars"]{Super
Mario RPG} contained the
@link["http://en.wikipedia.org/wiki/Nintendo_SA-1#SA1"]{Nintendo SA1}
which was an upgrade SNES CPU.)

However, most games were just a giant ROM chip array where the program
and its data were stored. In games like Final Fantasy where you would
play over many sessions, they needed to have an additional place to
save data that would persist across disconnects. Typically there would
be a RAM chip with a battery connected to it that would safe the
data. The size of these chips is typically very small, because of the
expensive and the drain on the battery. (These batteries a @emph{not}
designed to be replaced. I have many games from the early 1990s where
the battery has never been changed and still works. Nevertheless,
@link["http://www.racketboy.com/retro/how-to-change-snes-game-save-battery"]{it
is possible to replace them}.)

Final Fantasy 5 works just like this and it's where your saves are
stored. If I were using an @link["http://byuu.org/higan/"]{emulator},
it would be trivial to get the save RAM, but I always use a real SNES
when I play my games. Luckily, the
@link["http://www.retrode.org/"]{Retrode} can read the save RAM and
write it back on to the game.

My program modifies the save RAM to increase the number of ability
points each character has. I just dumped the RAM, ran the program, and
then put the RAM back on the cart.

@section{How does it work?}

Since Final Fantasy 5 was previously hacked to do a translation, how
its binary code works is well understood and
@link["http://wiki.superfamicom.org/snes/show/Final+Fantasy+5"]{documented}. Including
the arrangement of the save data.

The save data is 8 KiB and divided into 5 parts.

The first four are the four save slots and are @racket[#x700] bytes
apart, although only @racket[#x600] bytes are used.

@chunk[<save-slot>
       (define (save-slot-start i)
         (unless (< i 4)
           (error 'ff5 "There are only four save slots"))
         (* i #x700))]

The last part, which starts at @racket[#x1FF0] holds a table of
checksums to determine if each save slot is corrupted.

@chunk[<checksum>
       (define checksum-offset #x1FF0)]

The most complicated thing will be to fix this checksum afterwards.

Inside a save slot, there are four uniform blocks for information
about each character. They are stored first in the save slot and are
@racket[#x50] bytes large.

@chunk[<char-slot>
       (define (char-slot-offset i)
         (unless (< i 4)
           (error 'ff5 "There are only four characters"))
         (* i #x50))]

Inside of each of these slots, at byte @racket[#x3B], is a 16-bit
number recording the number of ability points the character
has. Despite being 16-bits, the game only recognizes values up to 999.

@chunk[<abp-offset>
       (define abp-offset #x3B)]

Given all this, it is trivial to modify every character, assuming the
SRAM is loaded as a byte string into @racket[sram], because
@racket[integer->integer-bytes] and @racket[integer-bytes->integer]
allow you to observe the actual bytes for a number with support for
reading and writing to pieces of byte strings.

@chunk[<modify>
       (define save-start (save-slot-start which-save))
       (for ([i (in-range 4)])
         (define char-start (char-slot-offset i))
         (define this-abp-offset 
           (+ save-start char-start abp-offset))
         (printf "~a: ~a -> 999\n"
                 i
                 (integer-bytes->integer
                  sram
                  signed? big-endian?
                  this-abp-offset
                  (+ this-abp-offset 2)))

         (integer->integer-bytes 
          999 2
          signed? big-endian?
          sram this-abp-offset))]

But, if you just do this, then the game will reject the save and
delete it, because the checksum is wrong.

@section{How do you compute the checksum?}

The checksum is a 16-bit number for each save. It is based on summing
the whole @racket[#x600] byte region for the save as 16-bit
numbers. The complicated part of implementing this in Racket is that
when running on the
@link["http://en.wikipedia.org/wiki/Ricoh_5A22"]{Ricoh 5A22}, the
carry bit is accumulated which each addition and that needs to be
accounted for. Similarly, you have to account for 16-bit overflow when
working in Racket with infinite precision integers.

@chunk[<checksum-calc>
       (define-values (final-checksum final-carry)
         (for/fold ([aw 0] [carry 0])
             ([i (in-range (/ #x600 2))])
           (define word-start
             (+ save-start (* i 2)))
           (define rw
             (integer-bytes->integer
              sram
              signed? big-endian?
              word-start
              (+ word-start 2)))
           (define maybe-overflowed-checksum
             (+ aw rw carry))
           (define new-checksum
             (modulo maybe-overflowed-checksum (expt 2 16)))
           (values new-checksum
                   (if (= new-checksum maybe-overflowed-checksum)
                     0
                     1))))]

After we've calculated it, we just write it:

@chunk[<write-check>
       (define checksum-start
         (+ checksum-offset (* which-save 2)))
       (integer->integer-bytes final-checksum 2
                               signed? big-endian?
                               sram checksum-start)]

When we put all this together in a function, it looks like:

@chunk[<abp-hack>
       (define (abp-hack p which-save)
         (define sram (file->bytes p))
         <modify>
         <checksum-calc>  
         <write-check>
         (display-to-file sram p #:exists 'replace))]

When I first did this, I had accidentally defined the endianness
incorrectly. I found a C++ version of this calculation
@link["https://github.com/pbiernat/mu/blob/master/mu.cpp"]{online},
but it has an error because it doesn't abstract from the endianness of
the machine it runs on. In addition, I think it is more complicated.

This program took me about 90 minutes to write, including reading
about the SRAM layout and working out all the little problems, like
endianness. I took about 8 minutes to actually do the dump/run/etc
cycle and get the points that I wanted.

Success!

@section{Yo! It's almost time to go!}

But first let's remember what we learned today!

Since Racket has hexadecimal literals, integer byte operations, and
convenient abstractions for endianness and byte length, it is an
excellent choice for low-level modification of 16-bit binary data.

And, with the power of programming, you can save yourself from 4 hours
of mindless grinding with just 1.5 hours of entertaining programming.

If you'd like to run this exact code at home, you should put it in
this order:

@chunk[<*>
       (require racket/file)

       <save-slot>
       <checksum>
       <char-slot>
       <abp-offset>

       (define signed? #f)
       (define big-endian? #f)

       <abp-hack>]

@(the-end)
