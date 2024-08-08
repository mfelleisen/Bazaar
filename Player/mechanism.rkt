#lang racket

;; ---------------------------------------------------------------------------------------------------
;; what the core of the player would looke like:

(require Bazaar/Player/strategies)
(require Bazaar/Common/player)
(require (only-in Bazaar/Common/turn-state turn))

(define strategy purchase-size)

#; {Turn {Purchase -> Natural} -> [Option Equation*]}
;; #false denotes a request for a random bebble from bank
#; (list 1eq ...) ; denotes a sequence of left-to-right exchanges 
(define (should-request-pebble-or-trade equations turn-state)
  (match-define [turn bank visibles p _scores] turn-state)
  (define wallet (player-wallet p))
  (cond
    [(should-the-olayer-request-a-random-pebble equations wallet bank)
     (set! *to-be-bought [purchase-cards (buy-cards wallet strategy)])
     #false]
    [else
     (define trades&buys (trade-then-purchase equations wallet bank strategy))
     (set! *to-be-bought (exchange-cards trades&buys))
     (exchange-trades trades&buys)]))

(define *to-be-bought #false)

#; {Turn -> [Listof Card]}
;; the catds that the player wishes to buy, in order 
(define (should-buy-cards turn)
  *to-be-bought)