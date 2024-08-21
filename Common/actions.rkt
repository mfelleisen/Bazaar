#lang racket

;; a data representation of the actions a player may request
;; -----------------------------------------------------------------------------

(provide
 #; {Any -> Boolean?}
 ;; #false denotes the request for a random pebble 
 want-pebble?
 want-pebble

 #; {Any -> Boolean}
 #; (list eq1 ...) ; is a request to trade according to these rules in order 
 trades?

 #; {Any -> Boolean}
 #; (list c1 ...) ;; is a request to purchase the specified cards in order 
 buy-cards?)

(module+ json
  (provide
   action->jsexpr
   jsexpr->action))

;; -----------------------------------------------------------------------------
(require (prefix-in c: Bazaar/Common/cards))
(require (prefix-in e: Bazaar/Common/equations))

(module+ json
  (require (submod Bazaar/Common/cards json))
  (require (submod Bazaar/Common/equations json))
  (require Bazaar/Lib/parse-json)
  (require SwDev/Lib/should-be-racket))

(module+ test
  (require (submod ".." json))
  (require (submod Bazaar/Common/cards examples))
  (require (submod Bazaar/Common/equations examples))
  (require rackunit))

;; -----------------------------------------------------------------------------
(define want-pebble  #false)
(define want-pebble? false?)

(define trades?      (listof e:1eq?))

(define buy-cards?   (listof c:card?))

;; -----------------------------------------------------------------------------
(module+ json
  (define (action->jsexpr a)
    (cond
      [(want-pebble? a) a]
      [(trades? a)      (equations->jsexpr a)]
      [(buy-cards? a)   (card*->jsexpr a)]))

  (define (jsexpr->action j)
    (cond
      [(false? j) #false]
      [else (or (dev/null (jsexpr->trades j)) (dev/null (jsexpr->cards j)))]))

  (def/jsexpr-> cards  #:array [(list (app jsexpr->card (? c:card? c)) ...) c])
  (def/jsexpr-> trades #:array [(list (app jsexpr->1eq (? e:1eq? e)) ...) e]))

(module+ test
  (check-equal? (jsexpr->action (action->jsexpr #false)) #false)
  (check-equal? (jsexpr->action (action->jsexpr `[,c-ggggg])) `[,c-ggggg])
  (check-equal? (jsexpr->action (action->jsexpr `[,ggg=r-])) `[,ggg=r-]))
