#lang racket

;; ---------------------------------------------------------------------------------------------------
;; what the core of the player would looke like:

(require Bazaar/Player/strategies)
(require Bazaar/Common/turn-state)
(require (prefix-in p: Bazaar/Common/player))

(define strategy purchase-size)
(define equations '())

#; {Turn {Purchase -> Natural} -> [Option Equation*]}
;; #false denotes a request for a random bebble from bank
#; (list 1eq ...) ; denotes a sequence of left-to-right exchanges 
(define (request-pebble-or-trade turn-state)
  (define bank (turn-bank turn-state))
  (define visibles (turn-cards turn-state))
  (define active   (turn-active turn-state))
  (define wallet   (turn-wallet active))
  (cond
    [(should-the-olayer-request-a-random-pebble equations wallet bank)
     #false]
    [else
     (define trades&buys (trade-then-purchase equations wallet bank strategy))
     (set! *to-be-bought (exchange-cards trades&buys))
     (exchange-trades trades&buys)]))

(define *to-be-bought #false)

#; {Turn -> [Listof Card]}
;; the catds that the player wishes to buy, in order 
(define (buy-which-cards-if-any turn)
  (set! *to-be-bought [purchase-cards (buy-cards (turn-wallet turn) strategy)])
  *to-be-bought)