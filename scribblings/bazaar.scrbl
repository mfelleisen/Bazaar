#lang scribble/manual

@(require racket/runtime-path)
@(require racket/list)
@(require 2htdp/image)
@(require "shared.rkt")
@(require "ll-style.rkt")

@(require (prefix-in p: Bazaar/Common/pebbles))
@(require (prefix-in p: (submod Bazaar/Common/pebbles examples)))
@(require (prefix-in c: Bazaar/Common/cards))
@(require (prefix-in c: (submod Bazaar/Common/cards examples)))
@(require (prefix-in e: Bazaar/Common/equations))

@(define bazaar-rules "https://boardgamegeek.com/boardgame/287/bazaar")
@(define-runtime-path bazaar.png "ist-bazaar.png")

@(require "spec.rkt")

@; -----------------------------------------------------------------------------
@author{Matthias Felleisen}

@top[#:tag "bazaar"]{@red{The Bazaar Game}}

@margin-note*{@(scale .5 [bitmap/file bazaar.png])}

@; -----------------------------------------------------------------------------

The game is inspired by @link[bazaar-rules]{@emph{Bazaar}}; the actual game
may help develop some intuition but the physical game and the implementation
differ in many ways.

@margin-note{Most of the time when we discuss ideas, the words ``referee'',
``player'', and so on refer to software components @emph{not} people.
To remind you of their inanimate nature, it is best to use ``its'' or
``it''---as in ``its game pieces'' or ``it's taking its turn.''.}

@; -----------------------------------------------------------------------------
@bold{Informal Overview}

The Bazaar game is a trading game for @(~a MIN-PLAYERS) to @(~a MAX-PLAYERS)
players. The playing field is made up of two items:

@itemlist[

@item{cards that players must purchase to get points, and}

@item{equations that allow players to trade what they own for alternatives.}

]

Players own ``pebbles,'' a form of currency and keep them hidden from other
players.  With these pebbles they can acquire the displayed cards; each purchase
yields a number of points depending on what the card displays and on how many
pebbles the player has left.  Once a card is purchased, it is replaced with
another one.  Players can also exchange (some of) their pebbles with a bank
according to the displayed equations.

The player with the highest total score wins when the game is over. 

@; -----------------------------------------------------------------------------
@bold{Game Pieces}

@(define pebbles
  (let* ([all (list p:RED p:GREEN p:YELLOW p:WHITE p:BLUE)]
	 [p (for/list ([c COLORS])
	      (define p (memf (lambda (p) (equal? c (p:pebble-color p))) all))
	      (first p))]
	 [s (map p:render p)])
    s))


@ll-style{
@; ----------------------
Our version of the game comes with pebbles of the following colors: 
@element-join[COLORS]{, }. & @element-join[@pebbles] |
@; ----------------------
There are @(~a PEBBLES#) pebbles overall, an equal number of each kind. & | 
@; ----------------------
A card displays five images of such pebbles, arranged in a
circular manner, optionally decorated with a happy face in the center.
&
@element-join[(map c:render (list c:c-rrbrr c:c-rrbrr*)) "      "] |
@; ----------------------
There are @(~a CARDS#) cards overall. & | 
@; ----------------------
An equation shows two collections of pebbles on the sides of
an @tt{=} sign; each side has at least one and at most @(~a [MAX-EQ-SIDE])
pebbles. The two sides must not contain pebbles of the same color. 
&
@(e:render (e:1eq (list p:RED) (list p:GREEN p:BLUE p:WHITE)))
@nl
@nl
@(e:render (e:1eq (list p:GREEN p:BLUE) (list p:RED p:YELLOW p:WHITE p:YELLOW))) |
@; ----------------------
There are @(~a EQUATIONS#) equations overall. & |
@; ----------------------
}
@; -----------------------------------------------------------------------------

@; -----------------------------------------------------------------------------
@bold{Setting up the Game}

The @emph{referee} picks @(~a EQUATIONS#) equations at random and makes them
available to the players. It also puts @VISIBLE#-as-str cards on the playing field, again
visible to all players. Finally, it endows its bank with all of the colored
pebbles. That's it. 

@; -----------------------------------------------------------------------------
@bold{Playing a Turn}

A player's turn can proceed in one of three ways:

@itemlist[

  @item{A player can draw a random pebble from the bank of remaining pebbles.
  (The physical game comes with a @(~a (length COLORS))-sided die for this
  purpose that shows the @(~a (length COLORS)) colors; our digital players will
  ask the referee to draw randomly instead.)  Subsequently, the player may
  purchase cards from the visible cards by exchanging a matching bunch of
  pebbles with the bank.}

  @item{A player may exchange pebbles with the bank, using any of the applicable
  @(~a EQUATIONS#) equations as exchange rates.  (Equations may be used in
  either direction; that's what ``equation'' means.)  If the bank does not own
  enough pebbles for a particular equation, the corresponding trade cannot take
  place.  Once exchanges are complete, the player can again purchase cards as
  above.}

  @item{Finally, a player may choose to skip obtaining a random pebble or
  applying any exchange equations, and instead purchase a card or even several.}

]

@; -----------------------------------------------------------------------------
Any legal exchange request comes with a price.  Per such request, the referee
removes the bottom-most card from the pile of invisible ones or, if this pile is
empty, it removes all visible cards, thus ending the game.

After a player's turn is over, the referee replaces the acquired cards, if any,
with fresh ones, if any are remaining, and grants the next player a turn.

The referee eliminates any player that violates any rules during a turn.
Elimination means that the player's pebbles disappear from the game. 

@; -----------------------------------------------------------------------------
@bold{Scoring a Turn} A player receives points after buying a card.

@(define points-table (map (lambda (row) (map ~a row)) POINTS))
@(set! points-table
  (cons
   (cons (~a (caar points-table) " or more") (cdar points-table)) 
   (cdr points-table)))

@tabular[#:row-properties '(bottom-border) #:sep @hspace[5] #:style 'boxed
 @cons[
  @list[@t{ pebbles left } @t{ points per plain card } @t{ ... card with face } ]
    @points-table
]]

The referee keeps track of the scores on a per turn basis. The scores of all
players are visible. 

@; -----------------------------------------------------------------------------
@bold{Ending a Game} 

The game ends if
@; -----------------------------------------------------------------------------
@itemlist[

 @;item{the bank has no more pebbles at the end of a player's turn;}

 @item{all players have been kicked at the end of a player's turn;}

 @item{a player has @(~a PLAYER-WINS) points at the end of its turn;}

 @item{no more cards are available for purchase; or}

 @item{the bank is empty and no player can buy a card.}

]
@; -----------------------------------------------------------------------------
Technically, this means that a game may never end if the players pursue
strategies of not trading pebbles or not buying a sufficient number of cards. If
this ever happens, we will modify these instructions to avoid this problem in
the future.
