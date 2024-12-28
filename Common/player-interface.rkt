#lang racket

;; a player interface that the referee can use to service players 
;; ---------------------------------------------------------------------------------------------------

(provide
 #; {type Player}
 player%/c
 player/c

 #; {Any -> Any : a string that satisfies the length and char cosntraint}
 MAX-PLAYER-NAME PLAYER-NAME
 player-name?)

(provide
 (all-from-out Bazaar/Common/actions)
 (all-from-out Bazaar/Common/bags)
 (all-from-out Bazaar/Common/cards)
 (all-from-out Bazaar/Common/equations)
 (all-from-out Bazaar/Common/player)
 (all-from-out Bazaar/Common/rule-book)
 (all-from-out Bazaar/Common/turn-state))

;; ---------------------------------------------------------------------------------------------------
(require Bazaar/scribblings/spec)

(require (prefix-in a: Bazaar/Common/actions))
(require (prefix-in b: Bazaar/Common/bags))
(require (prefix-in c: Bazaar/Common/cards))
(require (prefix-in e: Bazaar/Common/equations))
(require (prefix-in r: Bazaar/Common/rule-book))
(require (prefix-in p: Bazaar/Common/player))
(require (prefix-in t: Bazaar/Common/turn-state))

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define player%/c
  (class/c
   [name                     (->m string?)]
   [setup                    (->m e:equations? void?)]
   [request-pebble-or-trades (->m t:turn? (or/c a:want-pebble? a:trades?))]
   [request-cards            (->m t:turn? a:buy-cards?)]
   [win                      (->m boolean? void?)]))

(define player/c [instanceof/c player%/c])

;; ---------------------------------------------------------------------------------------------------
#; {Any -> (U False Any)}
(define (player-name? x)
  (and (string? x) (and (<= (string-length x) MAX-PLAYER-NAME) (regexp-match PLAYER-NAME-PX x))))

(define PLAYER-NAME-PX (pregexp PLAYER-NAME))

(module+ test
  (check-true (cons? (player-name? "aB0")))
  (check-false (player-name? ""))
  (check-false (player-name? "ouch ouch"))
  (check-false (player-name? "123456789avcdefgAVCDEEDDDEE")))
