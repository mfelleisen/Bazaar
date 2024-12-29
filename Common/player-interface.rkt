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
 (all-from-out Bazaar/Common/pebbles)
 (all-from-out Bazaar/Common/rule-book)
 (all-from-out Bazaar/Common/turn-state))

(module+ examples
  (provide (all-from-out (submod Bazaar/Common/bags examples)))
  (provide (all-from-out (submod Bazaar/Common/cards examples)))
  (provide (all-from-out (submod Bazaar/Common/equations examples)))
  (provide (all-from-out (submod Bazaar/Common/player examples)))
  (provide (all-from-out (submod Bazaar/Common/pebbles examples)))
  (provide (all-from-out (submod Bazaar/Common/rule-book examples)))
  (provide (all-from-out (submod Bazaar/Common/turn-state examples))))

(module+ json
  (provide (all-from-out (submod Bazaar/Common/actions json)))
  (provide (all-from-out (submod Bazaar/Common/bags json)))
  (provide (all-from-out (submod Bazaar/Common/cards json)))
  (provide (all-from-out (submod Bazaar/Common/equations json)))
  (provide (all-from-out (submod Bazaar/Common/player json)))
  (provide (all-from-out (submod Bazaar/Common/pebbles json)))
  (provide (all-from-out (submod Bazaar/Common/turn-state json))))

;; ---------------------------------------------------------------------------------------------------
(require Bazaar/scribblings/spec)

(require (prefix-in a: Bazaar/Common/actions))
(require (prefix-in b: Bazaar/Common/bags))
(require (prefix-in c: Bazaar/Common/cards))
(require (prefix-in e: Bazaar/Common/equations))
(require (prefix-in r: Bazaar/Common/rule-book))
(require (prefix-in p: Bazaar/Common/player))
(require (prefix-in q: Bazaar/Common/pebbles))
(require (prefix-in t: Bazaar/Common/turn-state))

(module+ examples
  (require (submod Bazaar/Common/bags examples))
  (require (submod Bazaar/Common/cards examples))
  (require (submod Bazaar/Common/equations examples))
  (require (submod Bazaar/Common/player examples))
  (require (submod Bazaar/Common/pebbles examples))
  (require (except-in (submod Bazaar/Common/rule-book examples) ForStudents/))
  (require (submod Bazaar/Common/turn-state examples)))

(module+ json
  (require (submod Bazaar/Common/actions json))
  (require (submod Bazaar/Common/bags json))
  (require (submod Bazaar/Common/cards json))
  (require (submod Bazaar/Common/equations json))
  (require (submod Bazaar/Common/player json))
  (require (submod Bazaar/Common/pebbles json))
  (require (submod Bazaar/Common/turn-state json)))

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
