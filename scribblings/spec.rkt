(module spec racket/base
  [provide (all-defined-out)]
  (require racket/list)

  ;; -----------------------------------------------------------------------------
  ;; game specific constants 

  [define MIN-PLAYERS 2]
  [define MAX-PLAYERS 6]

  (define RED    "red")
  (define GREEN  "green")
  (define YELLOW "yellow")
  (define WHITE  "white")
  (define BLUE   "blue")

  (define COLORS (list RED WHITE BLUE GREEN YELLOW))
  (define (pebble-color? x) (cons? (member x COLORS)))

  (define EQ-PEBBLES  3)
  (define EQUATIONS# 10)

  (require (only-in racket/math natural?))

  (define PEBBLES# 100)
  (define (pebbles#? x) (and (natural? x) (<= x PEBBLES#)))

  (define CARDS# 20)
  (define (cards#? x) (and (natural? x) (<= x PEBBLES#)))

  (define TRADES# 3)
  (define TRADES#-as-str "three")
  

  (define POINTS
    (list
     (list 3 1 2)
     (list 2 2 3)
     (list 1 3 5)
     (list 0 5 8)))

  (define PLAYER-WINS 20)

  ;; -----------------------------------------------------------------------------
  [define PER-TURN-s 6]

  ;; -----------------------------------------------------------------------------
  [define WAIT-FOR-SIGNUP 3]  ;; chance to send a name 
  [define SERVER-WAIT 20]     ;; seconds per round 

  (define MAX-PLAYER-NAME 20)
  (define PLAYER-NAME "^[a-zA-Z0-9]+$")
  
  (define [OBS] "--show") 

  (define Tmp/ "Tmp/")
  (define PNG ".png"))
