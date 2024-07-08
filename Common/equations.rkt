#lang racket

(provide
 #; {type [Equation* X] = [Listof [Equation X]]}
 #; {type [Equation X]}

 render

 1eq)

;; ---------------------------------------------------------------------------------------------------
(require "bags.rkt")
(require pict)

(module+ test (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct 1eq [left right] #:prefab)
#; {type Equation = (1eq Side Side)}
#; {type Side     = b:Bag || (<= 1 (bag-size b) 4)}

#; {Equation* Bag -> [Listof Equation]}
;; return all those equations `e` for which `(subbag? (1eq-left e) bag)` `(subbag? (1eq-right e) bag)`
;;   ordered so that the left satisfies the condition; meaning the same equation can show up twice  
(define (useful equation* bag)
  (define sym (map 1eq-flip equation*))
  (for/fold ([result '()]) ([e (append equation* sym)] #:when (subbag? (1eq-left e) bag))
    (cons e result)))
    
(define (1eq-flip e)
  (1eq (1eq-right e) (1eq-left e)))

(define EQ1 [1eq '[1 1 2] '[3]])

(define (render 1eq render-element)
  (define left (apply hc-append 2 (map render-element (1eq-left 1eq))))
  (define right (apply hc-append 2 (map render-element (1eq-right 1eq))))
  (hc-append 5 left (text "=") right))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (define equations [list EQ1])
  (define reversed  [list [1eq '[3] '[1 1 2]]])

  (render EQ1 (λ (x) (text (~a x))))

  (check-equal? (useful [list [1eq '[1 1 2] '[3]]] '[1 1 2 3]) (append reversed equations)))