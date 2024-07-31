#lang racket

;; a data representation of the actions a player may request

(provide
 #; {Any -> Boolean}
 want-pebble?

 #; {Any -> Boolean}
 action*?

 #; {type TP = (trade->purchase [Listof 1Eq] [Listof Card])}
 (struct-out trade->purchase))

;; -----------------------------------------------------------------------------
(require (prefix-in c: Bazaar/Common/cards))
(require (prefix-in e: Bazaar/Common/equations))

;; -----------------------------------------------------------------------------
(define want-pebble (gensym 'want-pepple))
(define (want-pebble? x) (eq? x want-pebble))

(struct trade->purchase [trades purchases] #:prefab)
#; {type TP = (trade->purchase [Listof 1Eq] [Listof Card])}

(define (action*? x)
  (or (want-pebble? x)
      (and (trade->purchase? x)
           ((listof e:1eq?) (trade->purchase-trades x))
           ((listof c:card?) (trade->purchase-purchases x)))))