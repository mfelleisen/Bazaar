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

(require Bazaar/Lib/require-2-provide)

;; ---------------------------------------------------------------------------------------------------
(require Bazaar/scribblings/spec)

(require->provide (prefix-in a: Bazaar/Common/actions))
(require->provide (prefix-in b: Bazaar/Common/bags))
(require->provide (prefix-in c: Bazaar/Common/cards))
(require->provide (prefix-in e: Bazaar/Common/equations))
(require->provide (prefix-in r: Bazaar/Common/rule-book))
(require->provide (prefix-in p: Bazaar/Common/player))
(require->provide (prefix-in q: Bazaar/Common/pebbles))
(require->provide (prefix-in t: Bazaar/Common/turn-state))

(module+ examples
  (require->provide (submod Bazaar/Common/bags examples))
  (require->provide (submod Bazaar/Common/cards examples))
  (require->provide (submod Bazaar/Common/equations examples))
  (require->provide (submod Bazaar/Common/player examples))
  (require->provide (submod Bazaar/Common/pebbles examples))
  (require->provide (except-in (submod Bazaar/Common/rule-book examples) ForStudents/))
  (require->provide (submod Bazaar/Common/turn-state examples)))

(module+ json
  (require->provide (submod Bazaar/Common/actions json))
  (require->provide (submod Bazaar/Common/bags json))
  (require->provide (submod Bazaar/Common/cards json))
  (require->provide (submod Bazaar/Common/equations json))
  (require->provide (submod Bazaar/Common/player json))
  (require->provide (submod Bazaar/Common/pebbles json))
  (require->provide (submod Bazaar/Common/turn-state json)))

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
