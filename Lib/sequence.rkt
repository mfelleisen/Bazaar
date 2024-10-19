#lang racket

(provide 
 #; {[X X -> Bool] [Listof X] [Listof Y] -> Bool}
 sequence<?)

;; -----------------------------------------------------------------------------
(define (sequence<? < 1loc 2loc)
  (let loop ([c 1loc] [d 2loc])
    (cond
      [(empty? c) #false]
      [(< (first c) (first d)) #true]
      [(equal? (first c) (first d)) (loop (rest c) (rest d))]
      [else #false])))