#lang racket

(provide
 #; {type [Bag X] = [Listof X]}
 
 subbag?

 bag-minus)

;; -----------------------------------------------------------------------------
(module+ test (require rackunit))

;; -----------------------------------------------------------------------------
(define (subbag? b c)
  (and (<= (length b) (length c)) (subset? b c)))

(define (bag-minus b c)
  (for/fold ([b b]) ([x c])
    (remove x b)))

(define (bag-remove b x)
  (remove x b))

;; -----------------------------------------------------------------------------
(module+ test 
  (check-false (subbag? '[1 1 1 2] '[1 2]))
  (check-true (subbag? '[1 2]  '[1 1 1 2]))

  (check-equal? (bag-minus '[1 1 2] '[1 2]) '[1])
  (check-equal? (bag-minus '[1 2] '[1 1 2]) '[])
  (check-equal? (bag-minus '[1 1 2] '[1]) '[1 2]))