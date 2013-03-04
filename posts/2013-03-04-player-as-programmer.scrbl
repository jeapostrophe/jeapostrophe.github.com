#lang scribble/manual
@(require (for-label racket/base
                     rackunit
                     racket/list)
          "../post.rkt")

@title{Player as Programmer}
@categories["Games"]

One of my hobbies is playing video games. Videos games have a bad
reputation with a lot of people as being a particularly bad form of
entertainment, so some people I talk to are surprised when they find
out I play them. In this post, I discuss why I enjoy games against
other forms of entertainment.

@(the-jump)

Compared to other forms of entertainment, video games have many
virtues, especially for an adult with children:

@itemlist[

@item{It is very unlikely you will get injured playing games, unlike
playing sports where violence and injury are expected.}

@item{Video games are played at home, so they keep you in the house
and with your family, as opposed to requiring long trips or excursion
away from your family.}

@item{Video games are extremely cheap. You already have a TV, a modern
game console costs about $200 new, and a single $60 game could
entertain you for over one hundred hours. (For example, I recently
played
@link["http://www.amazon.com/Tales-Graces-f-sony-playstation3/dp/B002I0K2J4/ref=sr_1_cc_1?s=aps&ie=UTF8&qid=1362404577&sr=1-1-catcorr&keywords=tales+of+graces+f"]{Tales
of Graces f} for 115 hours over the course about two months---that's
about 2 hours of entertainment per dollar.) Compare this to a hobby
like skiing which costs over a thousand dollars in fixed-costs for
equipment that will last five years if it is very good and then about
60 or 70 dollars for a one-day pass which can't last more than a few
hours.}

@item{Video games don't require any preparation or coordination with
anyone else and can be played in very small chunks. I have had hobbies
before where I felt like I spent more time organizing the activity
then actually doing it and where if I didn't have more than three
hours to do it, it felt like the session wasn't worth it. In contrast,
many great games can be ready in minutes and can be played for very
small sessions after little kids go to bed, etc.}

]

Of course, games have some problems, which are mostly shared with
other types of popular entertainment: many popular games contain
violence, gore, bad language, etc. Luckily, the game rating agency,
the ESRB, has very thorough reviews (compare
@link["http://www.esrb.org/ratings/synopsis.jsp?Certificate=27877&Title=New%20Super%20Mario%20Bros.%20Wii&searchkeyword=new%20super%20mario"]{New Super Mario Bros. Wii},
@link["http://www.esrb.org/ratings/synopsis.jsp?Certificate=28314&Title=FINAL%20FANTASY%20XIII&searchkeyword=final%20fantasy%20xiii"]{Final
Fantasy XIII}, and
@link["http://www.esrb.org/ratings/synopsis.jsp?Certificate=32552&Title=Call%20of%20Duty%3A%20Black%20Ops%20II&searchkeyword=call%20of%20duty%204"]{Call
of Duty: Black Ops II}), as opposed to movies or TV where the ratings
have no information behind them at all. So, it is very easy to avoid
what you don't want to see. The fact that games are so often consumed
by children means that there are many games targeted at them and you
can have a basically endless supply of clean games if you
want. Furthermore, one of the great things about older games is that
they lack the visual fidelity to produce offensive effects, so they
are incredibly pure and the catalog of good old games is vast.

In any case, the thing that most compels me to play games is that they
are "learning machines": each game presents a unique system of play
that constantly evaluates your performance in that system, encouraging
you to improve with rewards and ratcheting up the challenge as you
progress. It feels good to learn something and master it---games can
provide this experience in a compelling way.

I think of the activity of playing a game as very similar to
programming, and I enjoy it for the same reason: I have a new
challenge and I have to discover a way to approach it that will
succeed. For example, you can think of a game's player as a function
with the type:

@racketblock[
(GameState -> PlayerAction)             
]

That is, the player consumes the @racket[GameState], an
encoding (often visual) of the state of the game, and produces a
@racket[PlayerAction] (often, by pressing buttons on their
controller). In contrast to this "stateless" player, some games
require a "stateful" player that plans and reacts to their plans,
which would be like the type:

@racketblock[
(PlayerMemory GameState -> PlayerMemory PlayerAction)             
]

I think some good examples of these different player styles are
something like @link["http://en.wikipedia.org/wiki/Pong"]{Pong}, where
you can be stateless, and something like
@link["http://en.wikipedia.org/wiki/Civilization_(series)"]{Civilization},
where you need a detailed strategy that is not simply reactionary on
the game state.

When you are playing a game, you are writing this function into your
brain. Since programming is a rewarding activity, this can be a
rewarding activity too, although I think this is uniquely fun because
you are sitting in front of a great test case generator and you can
modify your style continuously.

Something that concerns me a great deal is: which games are fun? Or
from another perspective: which functions are fun? Yet another
distinction is: which functions are fun to @emph{write} and which
functions are fun to @emph{run}? In future posts, I'll explore these
ideas and discuss my theory of what makes games fun and how that can
influence game design.

@(the-end)
