#lang racket

(provide
 #; {type [Bag X] = [Listof X]}
 
 subbag?

 bag-minus

 bag-equal?)

(module+ json
  (provide
   #;{[Bag X] [X -> JSexpr] -> JSExpr}
   bag->jsexpr

   #; {[JSExpr [Y -> Boolean] [JSExpr -> X] -> (U False [Bag X])]}
   jsexpr->bag))

;; -----------------------------------------------------------------------------
(module+ test
  (require (submod ".." json))
  (require rackunit))

(module+ json
  (require Bazaar/Lib/parse-json))

;; -----------------------------------------------------------------------------
(define (subbag? b c)
  (and (<= (length b) (length c)) (subset? b c)))

(define (bag-minus b c)
  (for/fold ([b b]) ([x c])
    (remove x b)))

(define (bag-remove b x)
  (remove x b))

(define (bag-equal? b c)
  (and (subbag? b c) (subbag? c b)))

;; ----------------------------------------------------------------------------------------
;; JSON

(module+ json
  (define (bag->jsexpr b e->jsexpr)
    (for/fold ([h (hasheq)]) ([x b])
      (hash-update h (e->jsexpr x) add1 0)))

  (define (jsexpr->bag j predicate? jsexpr->e)
    (def/jsexpr-> bag
      #:hash {[predicate? #:parse e (? natural? n)]}
      (for/fold ([r '()]) ([(k v) j][n n])
         (define x (make-list n k))
         (append x r)))
    (jsexpr->bag j)))


;; -----------------------------------------------------------------------------
(module+ test 
  (check-false (subbag? '[1 1 1 2] '[1 2]))
  (check-true (subbag? '[1 2]  '[1 1 1 2]))

  (check-equal? (bag-minus '[1 1 2] '[1 2]) '[1])
  (check-equal? (bag-minus '[1 2] '[1 1 2]) '[])
  (check-equal? (bag-minus '[1 1 2] '[1]) '[1 2])

  (define b1 '[1 1 2 3])
  (check bag-equal? (jsexpr->bag (bag->jsexpr b1 values) natural? values) b1))