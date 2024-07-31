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

;; ---------------------------------------------------------------------------------------------------
(require Bazaar/scribblings/spec)
(require (prefix-in a: Bazaar/Common/actions))
(require (prefix-in e: Bazaar/Common/equations))
(require Bazaar/Common/turn-state)

(module+ test
  (require)
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define player%/c
  (class/c
   [name      (->m string?)]
   [setup     (->m (listof e:1eq?) void?)]
   [take-turn (->m turn-state? a:action*?)]
   [win       (->m boolean? void?)]))

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
