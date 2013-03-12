#lang scribble/manual
@(require (for-label racket/base
                     rackunit
                     racket/list)
          "../post.rkt")

@title{Player as Programmer II: Fun}
@categories["Games"]

@postlink["2013-03-04-player-a"]{Last week} I introduced the basic
framework I use to understand video games. In particular, I discussed
how I see them as either "stateless" functions:

@racketblock[
(GameState -> PlayerAction)             
]

Or, "stateful" functions:
@racketblock[
(PlayerMemory GameState -> PlayerMemory PlayerAction)             
]

In this post I use these notions to describe what makes a game "fun".

@(the-jump)

It is important to realize that playing a game engages you with the
function in two ways: you must write the functions (into your brain)
and then you must run the function (using your brain).

@section{Only Fun to Write}

For some games, it is very amusing to write the function. For example,
young children like to play Tic-Tac-Toe, in part (I believe), because
they enjoy discovering which function is the correct one. In contrast,
adults rarely like to play Tic-Tac-Toe because they've figured out the
correct function and just run it routinely.

There are other games with this same structure that can provide
entertainment for longer, but ultimately fail. I include Sudoku and
most Solitaire derivatives in this category, because it is relatively
easy to master @emph{the} strategy and then be done with game.

Other games don't even have this property. My favourite example is the
card game War, where the @racket[PlayerAction] type has only one
element: meaning that there do not exist different functions to play
the game, because the game lacks any player choice.

In general, games that are only fun to write lack the property
of "replayability". I find that almost all "puzzle" games are in this
category, because once you figure out the puzzle, there is no
enjoyment afterwards. Nevertheless, this does not mean that they are
terrible. I greatly enjoy the Professor Layton games and games like
Picross.

@bold{Summary:} The best games are fun to write @emph{and} to execute.

@section{Story Heavy Games: Fun to Write?}

Another kind of game that is commonly seen as lacking replayability
are games that have a very heavy story component. In these games, the
player extracts enjoyment for seeing unique game states:
@racketblock[
(define (fun game)
  (integral 
   game
   (λ (game-state) 
     (if (is-unique? game-state)
       1
       0))))
]

Traditional PC adventure games are classic examples of this: the
puzzles are identical between runs and the fun is from viewing new
scenarios, dialogue, cut-scenes, etc. Many older games lack any "real"
choice, because there is only one linear sequence of game states that
is possible, modulo puzzle failure. In fact, walkthroughs for these
games often give instructions about how to fail in every unique way,
so you can "see" everything in one playthrough. 

When games like this offer different paths, it is common to hear
complaints that the game forces too much "busy work" on the player who
wants to see something new. The most common complaint of this form is
when you can't skip cut-scenes.  Some modern games of this variety,
such as
@link["http://en.wikipedia.org/wiki/999:_Nine_Hours,_Nine_Persons,_Nine_Doors"]{999:
Nine Hours, Nine Persons, Nine Doors} and its sequel, have tools to
quickly skip pieces of the game that you've already experienced to
optimize replays.

Traditionally the other genre where this lack of replayability is very
common is Japanese roleplaying games (JRPGs). As a common example,
many believe that the player actions of a game like
@link["http://en.wikipedia.org/wiki/Dragon_Quest"]{Dragon Quest} are
boring ("Press X to Win") and the only enjoyment is the unraveling
story. In the PlayStation-era, non-skippable cut-scenes were an
incredibly common complaint against these games.

If a game allows skipping of story segments, it should ensure that
necessary information to make progress in the game (such as where to
go next) is displayed some other way.

I find it very disappointing that some modern trendy games totally
lose the joy of playing the game in favour of experiencing the
story. Games like Journey, the Walking Dead, or Dear Esther are
described to be devoid of interesting gameplay and rest entirely on
their story.

As an aside, I think the stories in most games are pretty bland and
pathetic from a literary perspective, so I recommend that you don't
limit your intake of fiction to just games, because you'll be sampling
from a trivial repertoire.

@bold{Summary:} Story is a liability when creating a fun game, because
it takes attention away from the gameplay and is inherently
un-replayable, no matter how good it is.

@section{Next Time}

Next week we will continue with the analysis of which functions are
fun to execute and discuss how the concept of "level" influences this
framework.

@(the-end)
