#lang racket

;; the referee

(require (prefix-in gs: Bazaar/Referee/game-state))
(require Qwirkle/Lib/xsend)

(module+ test
  (require (submod Bazaar/Referee/game-state examples))
  (require (submod Bazaar/Common/player examples))
  (require (except-in (submod Bazaar/Common/equations examples) ForStudents/ Tests/))
  (require Bazaar/Player/mechanism)
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
#; {[Listof PlayerObject] Equations GameState -> [List [Listof Player] [Listof Player]]}
(define (referee player* equations gs)
  (let*-values ([(gs0)                           (gs:connect gs player*)]
                [(gs-post-setup setup-drop-outs) (apply values (setup equations gs0 player*))]
                [(gs-post-turns turn-drop-outs)  (apply values (run-turns equations gs-post-setup))]
                [(winners win-lose-drop-outs)    (apply values (inform-players gs-post-turns))])
    [list winners (append setup-drop-outs turn-drop-outs win-lose-drop-outs)]))

;; ---------------------------------------------------------------------------------------------------
#; {Equations GameState -> [List GameState [Listof PlayerObject]]}
(define (setup equations gs0 players)
  (for/fold ([gs gs0] [kicked '()] #:result (list gs kicked)) ([active players])
    (setup-1-player equations active gs kicked)))

#;{Equations PlayerObject GameState [Listof PlayerObject] -> (values GameState [Listof PlayerObject])}
(define (setup-1-player equations active gs kicked)
  (match (xsend active setup equations)
    [(? string?) (values (gs:kick gs) (cons active kicked))]
    [_           (values (gs:rotate gs) kicked)]))

;; ---------------------------------------------------------------------------------------------------
#; {Equations GameState -> [List GameState [Listof PlayerObject]]}
(define (run-turns equations gs-post-setup)
  (let until-end ([gs gs-post-setup] [kicked '()])
    (cond
      [(gs:game-over? gs) (list gs (reverse kicked))]
      [else
       (match (one-turn equations gs)
         [(list gs active) (until-end gs (cons active kicked))]
         [gs (until-end gs kicked)])])))

#; {Equations GameState -> (U GameState [List GameState PlayerObject])}
(define (one-turn equations gs)
  (define active  (gs:game-active gs))
  (define action1 (xsend active request-pebble-or-trades (gs:extract-turn gs)))
  (match (gs:legal-pebble-or-trade-request equations action1 gs)
    [#false (list (gs:kick gs) active)]
    [gs
     (define action2 (xsend active request-cards (gs:extract-turn gs)))
     (match (gs:legal-purchase-request action2 gs)
       [#false (list (gs:kick gs) active)]
       [gs (gs:rotate gs)])]))

;; ---------------------------------------------------------------------------------------------------
#; {GameStatr -> [List [Listof PlayerObject] [Listof PlayerObject]]}
(define (inform-players gs-post-turns)
  (match-define [list winners losers] (gs:determine-winners-and-losers gs-post-turns))
  (match-define [list true-winners w-drop-outs] (final-inform winners #true))
  (match-define [list true-losers l-drop-outs]  (final-inform losers #false))
  (list true-winners (append w-drop-outs l-drop-outs)))

#; {[Listof PlayerObject] Boolean -> [List [Listof PlayerObject] [Listof PlayerObject]]}
(define (final-inform players msg)
  (for/fold ([winners '()] [kicked '()] #:result (list winners kicked)) ([p players])
    (match (xsend p win msg)
      [#false (values winners (cons p kicked))]
      [_      (values (cons p winners) kicked)])))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (define (w+do->names p)
    (let* ([s p]
           [t (map (λ (player) (xsend player name)) (first s))]
           [u (map (λ (player) (xsend player name)) (second s))])
      (list t u)))

  (define 2players [list (new player% [my-name "Adam"]) (new player% [my-name "Eve"])])
  (check-equal? (w+do->names (referee 2players '[] gs-20)) '[["Adam"] []] "1 winner")

  (define 3players (append 2players (list (new player% [my-name "Carl"]))))
  (check-equal? (w+do->names (referee 3players `[] gs-3-zeros)) '[["Carl" "Adam"] []] "2 buys, wins")

  '-----
  (define 6players (append 3players (map (λ (n) (new player% [my-name n])) '["Dan" "Felix" "Grace"])))
  (check-equal? (w+do->names (referee 6players (list ggg=b) gs-6-players)) '[["Adam"] []]))
  
  
