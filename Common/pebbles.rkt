#lang racket

(provide
 #; {type Pebble}

 #; {Pebble -> Pict}
 render 

 RADIUS
 RED GREEN YELLOW WHITE BLUE)

; -----------------------------------------------------------------------------
(require pict)

;; -----------------------------------------------------------------------------
(struct pebble [color] #:prefab)

(define RADIUS 15)

(define RED    (pebble "red"))
(define GREEN  (pebble "green"))
(define YELLOW (pebble "yellow"))
(define WHITE  (pebble "white"))
(define BLUE   (pebble "blue"))

(define (render p)
  (filled-ellipse RADIUS RADIUS #:color (pebble-color p)))

;; -----------------------------------------------------------------------------
(module+ pict
  (render (pebble "red")))