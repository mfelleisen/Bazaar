#lang racket

;; draft strategy file
;; ---------------------------------------------------------------------------------------------------

#|
question 1: should the player request a random pebble?
  answer 1: only if the player can't perform any trades

question 2: should the player perform trades?
  answer 2: yes, depending on the answers to question 3  

question 3: should the player buy cards?
  answer 3-dumb:  maximize the number of cards a player can purchase ...
  answer 3-silly: maximize the number of points a player can get by purchasing cards ... 
    with maximally 3 pebble trades
|#

#|

given pebbles, generate all possibilities of buying cards
  (why is _all_ possible?)
|#

(require (prefix-in b: Bazaar/Common/bags))
(require (prefix-in c: Bazaar/Common/cards))
(require (prefix-in c: Bazaar/Common/rule-book))
(require (submod Bazaar/Common/bags examples))
(require (submod Bazaar/Common/cards examples))
(require (submod Bazaar/Common/pebbles examples))

(struct node [card points] #:prefab)
#; {type Purchases = (node [Listof Card] Natural)}
#; (node c* n) ; represent the purchase of cards `c` with `n` points for all purchases 

#; {[Setof Card] Bag [ [Listof [Listof Card]] -> [Listof Card] ] -> [Listof Card]}
;; the player wishes to purchase the cards in the specified list order 
(define (buy-cards visibles wallet pick-best)
  (define possible (possible-purchases visibles wallet))
  (cond
    [(empty? possible) '()]
    [else (pick-best possible)]))

#; {[Setof Card] Bag [] -> Purchases}
(define (possible-purchases visibles0 wallet0)
  #; {Setof Card}
  (define possibles '[])

  ;; ACCU in reverse order of possible purchaes from `visibles0` & `wallet0` to `visibles` & `wallet`
  (let p-p/accu ([visibles visibles0] [wallet wallet0] [from-root-to-here '()] [points 0])
    #; [Listof Card]
    (define trades (c:can-buy visibles wallet))
    (cond
      [(empty? trades)
       (define proper-order (reverse from-root-to-here))
       (set! possibles (cons (node proper-order points) possibles))]
      [else
       (for ([t trades])
         (define visibles--  (remove t visibles))
         (define wallet--    (b:bag-minus (c:card-pebbles t) wallet))
         (define points++    (+ (c:calculate-points t (b:bag-size wallet--)) points))
         (define from-root++ (cons t from-root-to-here))
         (p-p/accu visibles-- wallet-- from-root++ points++))]))
  
  possibles)

;; ---------------------------------------------------------------------------------------------------
#; {[NEListof Purchase] -> [Listof Card]}
(define (pick-most-points possible)
  (define first-best (node-points (argmax node-points possible)))
  (define all-best   (filter (λ (p*) (= (node-points p*) first-best)) possible))
  (define just-cards (map node-card all-best))
  (tie-break-for-most-cards just-cards))

;; ---------------------------------------------------------------------------------------------------
;; this strategy picks the most cards, ordered according to the tiebreaker 

#; {[NEListof Purchase] -> [Listof Card]}
(define (pick-most-cards possible)
  (define just-cards (map node-card possible))
  (define first-best (length (argmax length just-cards)))
  (define all-best (filter (λ (p*) (= (length p*) first-best)) just-cards))
  (tie-break-for-most-cards all-best))

#; {[NEListof [Listof Card]] -> [Listof Card]}
;; pick the list of cards that is 
(define (tie-break-for-most-cards all-best)
  (cond
    [(empty? (rest all-best)) (first all-best)]
    [else 
     (define the-best (sort all-best (λ (p q) (<= (length p) (length q)))))
     (define one (first the-best))
     (define two (second the-best))
     (when (and (= (length one) (length two)) (not (equal? one two)))
       (error 'pick-most-cards "found too many bests in ~a" all-best))
     (first (sort the-best cards<=))]))

;; ---------------------------------------------------------------------------------------------------
;; RED, WHITE, BLUE, GREEN, YELLOW

#; {[Listof Card] [Listof Card] -> Boolean}
(define (cards<= purchase-order-1 purchase-order-2)
  (define 1bag (map c:card-pebbles purchase-order-1))
  (define 2bag (map c:card-pebbles purchase-order-2))
  (for/first ([p 1bag] [q 2bag] #:when (1card<= p q))
    #true))

(define (1card<= 1bag 2bag)
  (for/first ([p PEBBLES] #:when (and (b:bag-member? 1bag p) (not (b:bag-member? 2bag p))))
    #true))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  
  (require rackunit)
  (check-equal? (buy-cards (list) (b:bag) pick-most-cards) '())
  (check-equal? (buy-cards (list c-ggggg c-ggggg) b-ggggg pick-most-cards) (list c-ggggg))
  (check-equal? (possible-purchases (list c-ggggg c-ggggg) b-ggggg)
                (list (node (list c-ggggg) 5) (node (list c-ggggg) 5)))

  (check-equal? (buy-cards (list c-ggggg c-ggggg) b-ggggg pick-most-points) (list c-ggggg)))
