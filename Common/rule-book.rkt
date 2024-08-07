#lang racket

;; the rules of the Bazaar game: legality and scoring
;; ---------------------------------------------------------------------------------------------------

(provide 
 #; {Card N -> N}
 calculate-points

 #; {[Listof Card] Bag -> [Listof Card]}
 can-buy)

;; ---------------------------------------------------------------------------------------------------
(require Bazaar/scribblings/spec)

(require (prefix-in b: Bazaar/Common/bags))
(require (prefix-in c: Bazaar/Common/cards))

(module+ test
  (require (submod Bazaar/Common/cards examples))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define (calculate-points card pebbles#)
  (for/first ([p POINTS] #:when (>= pebbles# (points-pebbles-left p)))
    (if (c:card-face? card) (points-with-face p) (points-no-face p))))

;; ---------------------------------------------------------------------------------------------------
(define (can-buy cards wallet)
  (for/list ([c cards] #:when (b:subbag? (c:card-pebbles c) wallet))
    c))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-equal? (calculate-points c-rrbrr 0) (points-no-face (last POINTS)))
  (check-equal? (calculate-points c-rrbrr* 0) (points-with-face (last POINTS))))