#lang racket

;; a function that checks orderings on equally sequences of X given an ordering on X

(provide 
 #; {[X X -> Bool] [Listof X] [Listof Y] -> Bool}
 ;; are the two equally long sequences `equal?` up to some position
 ;; and then < at the two places?
 sequence<?)

;; -----------------------------------------------------------------------------
(define (sequence<? < 1lox 2lox)
  (let loop ([c 1lox] [d 2lox])
    (match* (c d)
      [{(cons 1c c) (cons 1d d)} (or (< 1c 1d) (and (equal? 1c 1d) (loop c d)))]
      [(_ _) #false])))

(module+ test
  (require rackunit)

  (check-false (sequence<? < '[] '[]))
  (check-true  (sequence<? < '[1] '[2]))
  (check-true  (sequence<? < '[0 1] '[0 2]))
  (check-false (sequence<? < '[1 1] '[0 2]))
  (check-false (sequence<? < '[] '[1]))
  (check-false (sequence<? < '[1] '[])))