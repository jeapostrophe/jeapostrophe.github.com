#lang scribble/manual
@(require (for-label racket/base
                     rackunit
                     racket/list)
          "../post.rkt")

@title{Player as Programmer III: Rules, Levels, Running, and Multiplayer}
@categories["Games"]

The last @postlink["2013-03-04-player-a"]{two}
@postlink["2013-03-11-pap2"]{weeks} I discussed the utility of the
concept of a player as a programmer when understanding fun in
games. This week, I continue to discuss these concepts.

@(the-jump)

The high-level points from before were:

@bold{Replayability:} The best games are fun to write @emph{and} to
execute.

@bold{Story Heavy Games:} Story is a liability when creating a fun
game, because it takes attention away from the gameplay and is
inherently un-replayable, no matter how good it is.

These proclamations are mostly about what is @emph{not} fun and don't
give a lot of advice about what is fun.

@section{Fun to Write: Foundational Rules}

I think the foundation of fun when writing functions is: reasonable
rules with understandable consequences that reward mastery. One of the
most frustrating experiences when playing games is when there don't
seem to be general rules controlling what's going on, where every
situation is capriciously different than those situations before it,
and where earlier knowledge and experience does more to hurt you than
to help you.

Many puzzle games have this feel, where each situation is so totally
different that there's not really a sense that it's the "same"
game. Many run-and-gun and shoot-em-up games have this feel as well,
when they seem to reward only pure memorization and not the building
of a general skill. The most insulting form of this is the modern
Quick-Time-Event, which is completely unpredictable and follows no
rules or internal logic. Another more modern form of the lack of
coherent rules is when a game has many "mini-games" or special levels
that obey totally different rules. An example close to my heart is
@link["http://en.wikipedia.org/wiki/Bayonetta"]{Bayonetta}, which is
almost a perfect game but suffers from a weird shooting mini-game, a
weird racing level, and a weird flying level. The
@link["http://www.giantbomb.com/turret-sequence/3015-7022/"]{turret
sequence} is an incredibly common lame mini-game.

Strategy and role-playing games seem to obey this framework most
rigidly because they so clearly define their systems in which all game
interaction takes place.

Most platforming and action games tend to obey this fairly well also,
but are far more likely to introduce unique situations on a per-level,
per-encounter basis. These "gimmicks" tend to give variety to the
foundation that might otherwise feel bland. Eventually those gimmicks
stop and the game reaches a vocabulary plateau on which there's a
complex framework to build levels on.

@bold{Summary:} A foundation of rules that rewards mastery and has
clear consequences is necessary to create an environment where
learning can take place, and thus is necessary for a programmer to
work and thrive.

@section{Fun to Write: Levels}

Levels are interesting from a programming perspective. You could
understand each level has its own unique game, because a different
function could be designed to perfectly beat each level. (Indeed, this
is a fairly accurate description of what a
@link["http://en.wikipedia.org/wiki/Tool-assisted_speedrun"]{tool-assisted
speedrun} is: a unique gameplay script for each level.) Yet, when that
is the most productive way for a human to play the game, then it
suggests that the game does not have coherent rules that don't
discriminate between levels. This suggests to me that levels are best
seen as:

@itemize[
@item{A way to gradually train the player in the rules to write the
function they need to be successful at the game.}

@item{A way to constrain the overall game design to obey rules to
create independent experiences.}

@item{A way to lengthen a game that is fun to execute (so you have
more time to execute it)} ]

In my opinion, the game that best demonstrates the strength of the
level concept is
@link["http://en.wikipedia.org/wiki/Super_Mario_Bros."]{Super Mario
Bros.}. This single game in 1985 has an incredibly minimal ruleset and
very few gimmicks, yet creates a wide variety of levels. Each new
version of Mario takes the same rule framework, adds a few new
gimmicks (like Poison Mushrooms, new power ups, new enemies, etc) and
then expands the set of levels to be larger. Each game is not
different, in the sense that it requires a new
training/design/programming phase to learn to play it. Its continued
success suggests that it is a Fun to Execute, since the learning is
relatively minimized each time.

@bold{Summary:} Levels are a tool to structure learning and structure
play, as opposed to a way to divide new and different learning and
play.

@section{Fun to Execute: Why?}

At first, it seems strange that any functions would be fun to execute,
because it so boring normally to be a computer: no one enjoys doing
basic arithmetic or manually stepping through the execution of their
programs. Indeed, it is insulting to our humanity to do things that a
computer could do. This suggests, however, that the functions that are
fun to execute are the ones that a computer could @emph{not} do. This,
I believe, is the key to understanding which functions are fun.

Games where we could easily imagine creating the perfect player
programmatically are unlikely to be fun, because either it is boring
to actively @emph{be} that player and "run the algorithm" or because
it is upsetting to know that the player exists, we could write it and
run it, but to do so is insulting, so we don't, and we don't perform
as well as that player would.

An example of this for me is
@link["http://en.wikipedia.org/wiki/Minesweeper_(video_game)"]{Minesweeper},
because I know exactly what I need to do and it is just a matter of
doing it over and over again.

A more subtle example is something like
@link["http://en.wikipedia.org/wiki/Spelunky"]{Spelunky} where a major
component of the game is "being careful". The ideal player goes
through a monotonous routine over every pit---look down, throw
something, decide if you will jump or use a rope---and there are
similar routines for every other situation. The game is interesting
because it is so boring to be the ideal player, that instead you take
chances (also because of your hubris) and then make mistakes and have
to use more dynamic problem-solving to get out of them. I think the
game is very flawed because to play it "well" is to play it "boring".

Simply because a computer player exists doesn't mean the game isn't
fun for humans. For example, the
@link["http://www.doc.ic.ac.uk/~rb1006/projects:marioai"]{Infinite
Mario AI} doesn't play at all how a human would, so it is not an
attack on Mario's fun that this AI exists. Although it does suggest
how to "unlock" the fun that humans can have: they need to be
overwhelmed to the point where the only way to succeed is to develop
subtle heuristics to guide when to apply the rules you've learned
playing the game a lot.

When I play games I can feel myself doing this very consciously: I'm
doing a level of Mario and I notice a small pattern that I "know" how
to solve, I do it, and keep going. I'm playing a shmup and I see a
bullet pattern coming toward me and I get a feeling to dodge in one
direction or another. I'm playing an action game and get an inkling
that a certain attack is incoming, so I fire up how to avoid that
situation. It is extremely satisfying to build up a large repositories
of such patterns and their response and then build little plans about
how to apply them.

The key, however, is to be able to first overwhelm the player so that
there's nothing better than heuristics at their disposal. There are a
few common techniques to overwhelm:

@itemize[

@item{Overwhelm the space of possibilities by increasing chance, with
randomness or with the complexity of the player response mechanism, so
the player can't effectively simulate.

This can be dangerous, because players rely on a certain level of
internal simulation to be able to predict the consequences of their
own actions. If you take this form of overwhelming too far, then you
risk hurting your rule foundation.

An example of a game that does this extremely well and clearly is a
roguelike such as
@link["http://en.wikipedia.org/wiki/NetHack"]{NetHack} where the
frontier of predictability is very large and close, so the player has
to act in the presence of missing information a lot.

I find that Super Mario Bros. takes the second approach of increasing
the complexity of the player response. Although the Infinite Mario AI
does exactly this, it is not feasible for a player to actually
simulate the exact outcome of all their choices, so they take more
modest and coarse simulations and build their play around them.}

@item{Overwhelm the space of information available to the player, so
they can't effectively track it all discretely and must instead use
heuristics to capture a useful fraction of it.

This can be dangerous, because it is easy to create a situation where
a player is frustrated that they can't harness it all, because they
know that if they did, they could "crunch the numbers" and take the
perfect course. 

I personally find it very frustrating when a RPG has a sub-system that
would be easy to do correctly if the data were just available in a
computer parseable form. In fact, for
@link["http://en.wikipedia.org/wiki/Shin_Megami_Tensei:_Persona_3"]{Persona
3} I wrote
@link["https://github.com/jeapostrophe/exp/blob/master/persona/p3p-db.rkt"]{a
program} to compute the ideal way to combine the monsters for your
goals based on a plain-text database I found in a FAQ. I find the game
to be designed poorly because since this approach is possible, they
don't just embed a solver for it in the game so you can spend your
time doing the @emph{fun} part of the game. (And indeed, in
@link["http://en.wikipedia.org/wiki/Shin_Megami_Tensei:_Persona_4#Persona_4_Golden"]{Persona
4 Golden} they removed the need to repeatedly try combinations until you
were randomly assigned the skills you wanted, now you can just select
them.) Other examples are when you discover a way to increase a
resource infinitely and can't "show the game" so you can just get as
many as you want. For example, in
@link["http://en.wikipedia.org/wiki/Dragon_Quest_IX:_Sentinels_of_the_Starry_Skies"]{Dragon
Quest 9}, once you unlock the teleport spell, the cauldron, and have a
certain (small) amount of money, you can generate an infinite amount
of money by exploiting an arbitrage opportunity with an item being
worth more than its ingredients. It would be nice to just automate
this and get an infinite amount of money than to have to actually do
the mindless process and get the money.

In any case, some games do this quite successfully. I think that
shmups do this with their complex bullet patterns: all the information
you need to dodge precisely is apparent and obvious, if you tracked
everything, you'd be perfect. But there's just
@link["https://www.google.com/search?q=bullet+hell&hl=en&safe=off&tbm=isch&tbo=u&source=univ&sa=X&ei=GKVIUduXHsePiAL414DoBA&ved=0CE8QsAQ&biw=718&bih=817"]{too
much} to actually do that, so you use heuristics and avoid things
using different techniques than actually tracking each bullet.}
]

When I look at games that are almost fun, I almost always find a way
some overwhelming has been taken too far to break down the ability to
form rules and heuristics to play.

@bold{Summary:} A fun game overwhelms the player into discovering and
using heuristics over discrete algorithms by offering overwhelming
possibilities, information, or consequences.

@section{Multiplayer Games}

In general, I would say that all multiplayer games are designed around
overwhelming the possibilities of what your opponents could be
doing---the Fog of War. Some multiplayer games, particularly RTS
games, use overwhelming information as well. Particularly weak RTS
games, like Starcraft, force you to deal with this overwhelming
information discretely (through "micro") to be effective, rather than
actually allowing heuristic strategies to succeed.

Unfortunately, this kind of overwhelming possibility is inherently
based on predicting human action. The problem with that is that humans
are capricious and do not need to follow any rules. Thus, when they
enter your game as an inherent piece of the system, they destroy the
ability to have a firm foundation of rules. This is why playing a game
online can be so frustrating and unsatisfying: you can't actually
train and learn, because your opponents can break rules at any
moment. Another way to look at it, that is commonly used when training
new humans into a game's culture, is that if you want to play against
other people, then you have to play the @emph{person} and not the
game. In the Street Fighter/fighting game community, this is commonly
understood as the "real" game.

For my taste, these are simply not games because they do not lend
themselves to the traditional ways of playing and learning
games. Instead, I like to refer to them as "arenas". Starcraft, Street
Fighter, Call of Duty, etc are new @emph{arenas} in which you can
compete with other humans. Now, due to how games are made and
marketed, it is very common for every multiplayer game to be combined
with a single player game that replaces the humans with AIs that can
be learned. When we're discussing that, then, of course, these arenas
are "games" to me. In many cases, however, they are not good
games. Many fighting games, for instances, are not designed to have
good or interesting AI opponents, since the human arena is the primary
aspiration of the developers.

I don't necessarily have a problem with "arenas" like this, but I
don't have taste for them. I find the experience of playing online to
be very annoying because other people don't have the same linguistic
standards I do, I have to deal with lag and other issues, and in
general I need to coordinate with other people, which is annoying.

I am skeptical, however, of the development of new "arenas" that are
on par with the arenas humans have created over time. It is rare for a
new arena to be better than something that has stood the test of time
like Poker, Chess, Football, Go, etc. In particular, I observe that
many modern multiplayer games that people are excited about have very
limited strategic spaces: the small number of openings for Starcraft
as opposed to the comparable space of Chess openings. I don't think
humans have yet created a computer arena that can realistically
compete with physical arenas, but I agree with the zeitgeist that
Starcraft or DotA is probably the closest we've come.

@bold{Summary:} Multiplayer games are fundamentally broken as learning
environments because humans are capricious and do not need to obey
rules. Therefore, to play and appreciate multiplayer games, you need
to appreciate psychology, which is quite distinct than why normal
games are appreciable.

@section{Fun to Input}

A unique form of fun can be extracted from the way you interact with
certain games. For example, plastic instruments and jumping on giant
buttons while "dancing" can be fun for its own reasons independent of
the fun of the "game". (In fact, most of these games are the most
devoid of fun in normal circumstances: memorizing the discrete order
of button presses with no reason for those buttons over others.) I've
found that most of these forms of fun quickly lose their excitement
and the core input style of the D-pad and buttons is far more
efficient and applicable to a wide variety of games.

I personally find that some popular inputs are negatives for me. For
example, I can't stand touch input because it is so imprecise and
lacks the ability to have individuated digital actions. Furthermore, I
hate to use mice (in general) and so I don't like games that use
mice. Finally, I don't like games that use the keyboard as input,
because the buttons are all so similar, you can't have a feeling
associated with each one, like with a controller. As another point
against the keyboard, it has far too many buttons and doesn't
encourage economy in designers.

On the other hand, I find the arcade stick to be a uniquely satisfying
input that can make almost any game more pleasing through it's nice
clicks and gigantic movements. One of the main reasons I like to play
Street Fighter (and similar) games is just because it is so nice to
execute the commands on the arcade stick.

I don't think input is a significant enough source of fun to be worth
designing around. I think it is far more likely to guarantee your game
is limited in its appeal by choosing a strange input system.

@section{Conclusion}

As Alexander Pope said, to compute is base and to program is human. It
is wonderful to feel that you are learning a system and building a
repository of ability to solve new instances of that system. When the
system is so complex, we can't imagine making computers that solve it
quickly and easily, yet we can do it ourselves, we feel even more
human and wonderful. This is the experience that good games create.

@(the-end)
