#lang racket

(provide
 #; {type Pebble}

 pebble-color?

 #; {Pebble -> Pict}
 render 

 RADIUS
 RED GREEN YELLOW WHITE BLUE)

(module+ json
  (provide
   pebble->jsexpr
   jsexpr->pebble))

; -----------------------------------------------------------------------------
(require Bazaar/scribblings/spec)
(require pict)

(module+ test
  (require (submod ".." json))
  (require rackunit))

;; -----------------------------------------------------------------------------
(struct pebble [color] #:prefab)

(define RADIUS 15)

(define RED    (pebble "red"))
(define GREEN  (pebble "green"))
(define YELLOW (pebble "yellow"))
(define WHITE  (pebble "white"))
(define BLUE   (pebble "blue"))

(define (pebble-color? x)
  (cons? (member x COLORS)))

(define (render p)
  (filled-ellipse RADIUS RADIUS #:color (pebble-color p)))

;; -----------------------------------------------------------------------------
(module+ json
  (define (pebble->jsexpr p)
    (pebble-color p))

  (define (jsexpr->pebble j)
    (match j
      [(? pebble-color?) (pebble j)]
      [_  (eprintf "jsexpr->pebble: pebble JSExpr expected, given ~a\n" j) #false])))

;; -----------------------------------------------------------------------------
(module+ pict
  (render (pebble "red")))

;; -----------------------------------------------------------------------------
(module+ test
  (check-equal? (jsexpr->pebble (pebble->jsexpr RED)) RED))