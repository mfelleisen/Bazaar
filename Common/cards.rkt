#lang racket

(provide
 #; {type Card}

 ;; examples 
 CARD1 CARD2

 #; {Card N -> N}
 calculate-points

 #; {Card -> Pict}
 render)

;; -----------------------------------------------------------------------------
(require "../scribblings/spec.rkt")

(require (prefix-in p: "pebbles.rkt"))
(require pict)
(require pict/face)

(module+ test
  (require rackunit))

;; -----------------------------------------------------------------------------
(struct card [pebbles face?] #:prefab)

(define (calculate-points card pebbles#)
    (for/first ([p POINTS] #:when (>= pebbles# (first p)))
      (if (card-face? card) (third p) (second p))))

(define (render c)
  (define pebbles (map p:render (card-pebbles c)))
  (define angle   (/ (* 2 pi) 5))
  (let* ([pebbles pebbles]
         [s (first pebbles)]
         [pebbles (rest pebbles)]
         [s (vc-append 20 s (apply hc-append 40 (take pebbles 2)))]
         [pebbles (drop pebbles 2)]
         [s (vc-append 20 s (apply hc-append 20 (take pebbles 2)))]
         [w (+ (pict-width s) 10)]
         [h (+ (pict-height s) 10)]
         [r (filled-rectangle w h #:color "turquoise")]
         [s (cc-superimpose r s)]
         [r (filled-rectangle w (quotient h 3) #:color "orange")]
         [s (vc-append r s r)]
         [s (if (card-face? c) (add-face s) s)])
    s))

#; {Pict -> Pict}
(define (add-face s)
  (let* ([s s]
         [f (scale (face* 'normal 'huge #f default-face-color 0 -3) .1)]
         [s (cc-superimpose s (colorize f "silver"))])
    s))

(define CARD1 (card [list p:RED p:RED p:BLUE p:RED p:RED] #false))
(define CARD2 (card [list p:RED p:RED p:BLUE p:RED p:RED] #true))

;; -----------------------------------------------------------------------------
(module+ pict
  (render CARD1)
  (render CARD2))

(module+ test
  (check-equal? (calculate-points CARD1 0) (second (last POINTS)))
  (check-equal? (calculate-points CARD2 0) (third (last POINTS))))
          
  
  