#lang racket

;; a data representation of the actions a player may request
;; -----------------------------------------------------------------------------

(provide
 #; {Any -> Boolean?}
 ;; #false denotes the request for a random pebble 
 want-pebble?

 #; {Any -> Boolean}
 #; (list eq1 ...) ; is a request to trade according to these rules in order 
 trades?

 #; {Any -> Boolean}
 #; (list c1 ...) ;; is a request to purchase the specified cards in order 
 buy-cards?)

;; -----------------------------------------------------------------------------
(require (prefix-in c: Bazaar/Common/cards))
(require (prefix-in e: Bazaar/Common/equations))

;; -----------------------------------------------------------------------------
(define want-pebble? false?)
(define trades?      (listof e:1eq?))
(define buy-cards?   (listof c:card?))
