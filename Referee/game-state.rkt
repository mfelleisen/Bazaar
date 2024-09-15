#lang racket

;; the referee's game state representation, including "connections" to the actual players
;; ---------------------------------------------------------------------------------------------------

;; TODO:
;; -- subtract a card from invisible per trade 

(provide
 #; {type GameState}
 game?
 ; game-players
 player-count

 #; {GameState -> Actor}
 game-active 

 #; {GameState Actor -> GameState}
 connect

 #; {GameState -> GameState}
 kick

 #; {GameState -> GameState}
 rotate 

 #; {GameState -> TurnState}
 extract-turn
 
 #; {GameState -> [List [Listof Actor] [Listof Actor]] }
 determine-winners-and-losers

 #; {GameState -> Boolean}
 game-over?

 #; {Equations Action GameState -> GameState}
 legal-pebble-or-trade-request

 #; {[Listof Card] GameState -> GameState}
 legal-purchase-request

 #; {GameState -> Pict}
 render)

(provide
 game-struct->definition)

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  (provide gs0 gs1 gs-no-players gs-20 gs-20-rotate gs1+g-r+1 gs-3-zeros
           gs-6-players gs-10 gs-10++ gs-10-- gs-6-players++++)

  #; {type GameTurnScenarios = [Listof 1Scenario]}
  #; {type 1Scenario         = [List GameState TurnState]}
  (provide ForStudents/ Tests/))

;; ---------------------------------------------------------------------------------------------------
(module+ json
  (provide
   game->jsexpr
   jsexpr->game))

;                                                                                      
;       ;                                  ;                                           
;       ;                                  ;                          ;                
;       ;                                  ;                                           
;    ;;;;   ;;;   ;;;;    ;;;   ; ;;    ;;;;   ;;;   ; ;;    ;;;    ;;;    ;;;    ;;;  
;   ;; ;;  ;;  ;  ;; ;;  ;;  ;  ;;  ;  ;; ;;  ;;  ;  ;;  ;  ;;  ;     ;   ;;  ;  ;   ; 
;   ;   ;  ;   ;; ;   ;  ;   ;; ;   ;  ;   ;  ;   ;; ;   ;  ;         ;   ;   ;; ;     
;   ;   ;  ;;;;;; ;   ;  ;;;;;; ;   ;  ;   ;  ;;;;;; ;   ;  ;         ;   ;;;;;;  ;;;  
;   ;   ;  ;      ;   ;  ;      ;   ;  ;   ;  ;      ;   ;  ;         ;   ;          ; 
;   ;; ;;  ;      ;; ;;  ;      ;   ;  ;; ;;  ;      ;   ;  ;;        ;   ;      ;   ; 
;    ;;;;   ;;;;  ;;;;    ;;;;  ;   ;   ;;;;   ;;;;  ;   ;   ;;;;   ;;;;;  ;;;;   ;;;  
;                 ;                                                                    
;                 ;                                                                    
;                 ;                                                                    

(require Bazaar/scribblings/spec)

(require (submod Bazaar/Common/cards examples))
(require (submod Bazaar/Common/bags examples))
(require (submod Bazaar/Common/player examples))

(require (submod Bazaar/Common/bags json))
(require (submod Bazaar/Common/cards json))

(require (prefix-in a: Bazaar/Common/actions))
(require (prefix-in c: Bazaar/Common/cards))
(require (prefix-in t: Bazaar/Common/turn-state))

(require Bazaar/Lib/configuration)
(require (prefix-in b: Bazaar/Common/bags))
(require (prefix-in p: Bazaar/Common/player))
(require (prefix-in r: Bazaar/Common/rule-book))

(require Bazaar/Lib/configuration)

(require SwDev/Lib/should-be-racket)

(require pict)

(module+ pict
  (require (submod ".." examples)))

(module+ examples
  (require (submod Bazaar/Common/rule-book examples))
  (require (submod Bazaar/Common/turn-state examples)))

(module+ test
  (require (submod ".." examples))
  (require (submod Bazaar/Common/pebbles examples))
  (require rackunit))
  
;                                                   
;                                                   
;          ;;;                                      
;            ;                                      
;   ;;;;     ;    ;;;;   ;   ;   ;;;    ;;;;    ;   
;   ;; ;;    ;        ;  ;   ;  ;;  ;   ;;  ;   ;   
;   ;   ;    ;        ;   ; ;   ;   ;;  ;       ;   
;   ;   ;    ;     ;;;;   ; ;   ;;;;;;  ;    ;;;;;;;
;   ;   ;    ;    ;   ;   ; ;   ;       ;       ;   
;   ;; ;;    ;    ;   ;   ;;    ;       ;       ;   
;   ;;;;      ;;   ;;;;    ;     ;;;;   ;           
;   ;                      ;                        
;   ;                     ;                         
;   ;                    ;;                         

(module player+ racket
  (provide
   (struct-out player+)
   update-player+-wallet
   update-player+-score
   update-player+-cards
   player+-award-bonus
   
   render*
   player+*->jsexpr
   name*
   jsexpr->player+*)
  
  (require (submod Bazaar/Common/player json))
  (require (prefix-in p: Bazaar/Common/player))
  (require Bazaar/Lib/parse-json)
  (require pict)

  (struct player+ [player connection] #:prefab)

  #; {[X] [Player X -> Player] -> Player+ X -> Player+}
  (define ((player+-update-player f) active y)
    (match-define [player+ p c] active)
    (struct-copy player+ active [player (f p y)]))
  
  #; {Player+ Bag -> Player+}
  (define update-player+-wallet (player+-update-player p:update-player-wallet))

  #; {Player N -> Player}
  (define update-player+-score (player+-update-player p:update-player-score))

  #; {Player+ [Listof Card] -> Player+}
  (define update-player+-cards (player+-update-player p:update-player-cards))

  #; {[Player -> Player] -> Player+ -> Player+}
  (define ((player+-award-bonus f) p+) [(player+-update-player (λ (p _) (f p))) p+ '_])
  
  #; {[Listof Player+] -> Pict}
  (define (render* player+*) (-> (listof player+?) (listof pict?))
    (for/fold ([p (blank 1 1)]) ([q player+*])
      (hb-append 5 p (render q))))
  
  #; {Player+ -> Pict}
  (define (render q)
    (match-define [player+ p o] q)
    (define name (if (is-a? o object%) (send o name) "unknown"))
    (p:render p #:name name))

  (define (player+*->jsexpr p*)
    (map (compose player->jsexpr player+-player) p*))

  (define name* (make-parameter 'unknown)) ;; for testing
  (def/jsexpr-> player+*
    #:array [(list (app jsexpr->player (? p:player? p)) ...) (map (λ (p) (player+ p [name*])) p)]))
(require 'player+)

;                                                          
;       ;                                  ;            ;; 
;       ;           ;                      ;           ;   
;       ;           ;                      ;           ;   
;    ;;;;  ;;;;   ;;;;;  ;;;;           ;;;;   ;;;   ;;;;; 
;   ;; ;;      ;    ;        ;         ;; ;;  ;;  ;    ;   
;   ;   ;      ;    ;        ;         ;   ;  ;   ;;   ;   
;   ;   ;   ;;;;    ;     ;;;;         ;   ;  ;;;;;;   ;   
;   ;   ;  ;   ;    ;    ;   ;         ;   ;  ;        ;   
;   ;; ;;  ;   ;    ;    ;   ;         ;; ;;  ;        ;   
;    ;;;;   ;;;;    ;;;   ;;;;          ;;;;   ;;;;    ;   
;                                                          
;                                                          
;                                                          

(define (card-check cards)
  (unless (<= (length cards) CARDS#)
    (error 'jsexpr->card ""))
  cards)

(define (size-check cards)
  (unless (<= (length cards) VISIBLE#)
    (error 'jsexpr->card ""))
  cards)

(struct/description
 game
 [bank     #:to-jsexpr bag->jsexpr   #:from-jsexpr jsexpr->bag   #:is-a "*Pebbles"]
 [visibles #:to-jsexpr card*->jsexpr #:from-jsexpr (compose size-check jsexpr->card*) #:is-a "*Cards"]
 [cards    #:to-jsexpr card*->jsexpr #:from-jsexpr (compose card-check jsexpr->card*) #:is-a "*Cards"]
 [players  #:to-jsexpr player+*->jsexpr #:from-jsexpr jsexpr->player+*
           #:is-a "*Players"])

#; {type GameState = (game Bag [Listof Card] [Listof Card] [Listof Player+])}
#; {type Player+   = (player+ Player {Object with name method take-turn method setup win})}

(define (player-count gs)
  (length (game-players gs)))

(define (connect gs actor)
  (match-define [game bank visibles cards (cons player-without others)] gs)
  (define player-with
    (match player-without
      {[player+ proper _] (player+ proper actor)}
      {_                  (player+ player-without actor)}))
  (game bank visibles cards (cons player-with others)))

(define (game-active gs)
  (player+-connection (first (game-players gs))))

#; {[X] [Player+ X -> Player+] -> GameState X -> GameState}
(define ((game-update-active f) gs y)
  (match-define [cons active others] (game-players gs))
  (struct-copy game gs [players (cons (f active y) others)]))

(module+ examples
  (define gs0 (game b-ggggg (list) (list) (list (player+ p-r6 'unknown))))
  (define gs1 (game b-ggggg [list c-ggggg c-rrbrr c-rgbrg] (list) (list (player+ p-r6 'x))))
  (define gs1+g-r+1 (game b-ggggg [list c-ggggg c-rrbrr c-rgbrg] (list) (list (player+ p-g7 'x))))
  
  (define gs-no-players (game b-ggggg [list c-ggggg c-rrbrr c-rgbrg] (list) (list)))
  (define gs-20 (game b-r [list c-ggggg] (list) (list (player+ p-rrbrr-20 'x) (player+ p-r6 'y))))
  (define gs-20-rotate ;; a final state shouldn't be rotated 
    (game b-r [list c-ggggg] (list) (list (player+ p-r6 'y) (player+ p-rrbrr-20 'x))))
  
  (define gs-10
    (let ([cards3 [list c-ggggg* c-wgwgw c-wgwgw* c-ggggg c-ywywy c-ywywy*]]
          [bank   (b:bag-add b-r b-r)])
      (game bank (take cards3 4) (drop cards3 4) `(,(player+ p-bbbb3 'x) ,(player+ p-r6 'y)))))

  (define gs-10++
    (let ([cards3 [list c-ggggg* c-wgwgw c-wgwgw* c-ggggg c-ywywy c-ywywy*]]
          [bank   (b:bag-add b-bbbb b-bbbbb b-r b-r b-r)])
      (game bank (take cards3 4) (drop cards3 4) `(,(player+ p-bbbb3 'x) ,(player+ p-bbbb3 'y)))))

  (define gs-10--
    (let ([cards3 [list c-ggggg* c-wgwgw c-wgwgw* c-ggggg c-ywywy c-ywywy*]]
          [bank   (b:bag-add b-g b-gw)])
      (game bank (take cards3 4) (drop cards3 4) `(,(player+ p-bbbb3 'x) ,(player+ p-bbbb3 'y)))))

  (define 6-players (map player+ (list p-ggb8 p-ggg5 p-r6 p-g7 p-gw9 p-4xb-3xg4) '[x y z a b c]))
  (define gs-6-players (game b-ggg (list c-ggggg) (list) 6-players))

  (provide gs-6-players++ gs-3-zeros++)

  (define cards2 (b:bag-add cards0 cards1 cards1 cards0 cards1))
  (define gs-6-players++ (game bank0 (take cards2 4) (drop cards2 4) 6-players))
  (define gs-6-players++++ (game bank0 (take ALL-CARDS 4) (drop ALL-CARDS 4) 6-players))

  (define 3-0-players (map player+ (list p-ggggg p-rgbrg p-wyrbb) '[k l m]))
  (define gs-3-zeros (game b-rrbrr (list c-wyrbb c-ggggg) '[] 3-0-players))
  (define gs-3-zeros++ (game b-rrbrr (take cards2 4) (drop cards2 4) 3-0-players)))

(module+ examples ;; test scenarios

  #; {(scenaro+ GameState TurnState msg:String)}
  (define-syntax-rule (scenario+ kind actual expected msg)
    (set! kind (append kind (list [list actual expected msg]))))

  (define ForStudents/ '[])
  (scenario+ ForStudents/ gs0 ts0 "even end of game states can be serialized to JSON")
  (scenario+ ForStudents/ gs1 ts1 "a one-player non-final state")
  (scenario+ ForStudents/ gs-20 ts-20 "a two-palayer state")

  (define Tests/ '[])
  (scenario+ Tests/ gs-20-rotate ts-20-rotate "final again")
  (scenario+ Tests/ gs-6-players ts-6-players "six players")
  (scenario+ Tests/ gs-3-zeros   ts-3-zeros "six players"))

;                                                                        
;      ;;                                                                
;     ;                                              ;;;                 
;     ;                                                ;                 
;   ;;;;;   ;;;;   ;;;  ;;;;;;          ;;;;  ;   ;    ;     ;;;    ;;;  
;     ;     ;;  ; ;; ;; ;  ;  ;         ;;  ; ;   ;    ;    ;;  ;  ;   ; 
;     ;     ;     ;   ; ;  ;  ;         ;     ;   ;    ;    ;   ;; ;     
;     ;     ;     ;   ; ;  ;  ;         ;     ;   ;    ;    ;;;;;;  ;;;  
;     ;     ;     ;   ; ;  ;  ;         ;     ;   ;    ;    ;          ; 
;     ;     ;     ;; ;; ;  ;  ;         ;     ;   ;    ;    ;      ;   ; 
;     ;     ;      ;;;  ;  ;  ;         ;      ;;;;     ;;   ;;;;   ;;;  
;                                                                        
;                                                                        
;                                                                        
;;    
#; {Game -> Boolean}
(define (game-over? gs)
  (define players (map player+-player (game-players gs)))
  (r:game-over? players (append (game-visibles gs) (game-cards gs)) (game-bank gs)))

;; ---------------------------------------------------------------------------------------------------
#; {Equations Action GameState -> (U False [list Pebble GameState] GameState)}
(define (legal-pebble-or-trade-request equations a gs)
  (define ts (extract-turn gs))
  (match (r:legal-pebble-or-trade-request equations a ts)
    [#false #false]
    [(list (? b:bag? wallet) (? b:bag? bank))
     (let* ([gs (struct-copy game gs [bank bank])]
            [gs (update-player-wallet gs wallet)]
            [gs (if (a:want-pebble? a) gs (update-cards gs))])
       gs)]))

;; ---------------------------------------------------------------------------------------------------
#; {[Listof Card] Turn -> }
(define (legal-purchase-request cards gs)
  (define ts (extract-turn gs))
  (match (r:legal-purchase-request cards ts)
    [#false #false]
    [(list delta visibles wallet bank)
     (let* ([gs (update-player-score gs delta)]
            [gs (update-player-wallet gs wallet)]
            [gs (update-player-cards gs cards)]
            [gs (replenish-visibles gs visibles)]
            [gs (struct-copy game gs [bank bank])])
       gs)]))

;; ---------------------------------------------------------------------------------------------------
#; {GameState Bag -> GameState}
(define update-player-wallet (game-update-active update-player+-wallet))

#; {GameState N -> GameState}
(define update-player-score (game-update-active update-player+-score))

#; {HameState [Listof Card] -> GameState}
(define update-player-cards (game-update-active update-player+-cards))

#; {GameState [Lustof Card] -> GameState}
(define (replenish-visibles gs visibles0)
  (define-values (cards visibles) (transfer-cards (game-cards gs) visibles0))
  (struct-copy game gs [cards cards] [visibles visibles]))

#; {[Listof Card] [Listof Card] -> (values [Listof Card] [Listof Card])}
(define (transfer-cards cards visibles0)
  (define N (min (- VISIBLE# (length visibles0)) (length cards)))
  (values (drop cards N) (append visibles0 (take cards N))))

#; {GameState -> GameState}
(define (update-cards gs)
  (match-define [game bank visibles cards players] gs)
  (cond
    [(empty? cards) (game bank '() '() players)]
    [else (game bank visibles (rdc cards) players)]))

;                                                                 
;                                                                 
;                                                ;                
;                                                                 
;    ;;;    ;;;    ;;;   ; ;;   ;;;;    ;;;;   ;;;    ;;;    ;;;  
;   ;   ;  ;;  ;  ;;  ;  ;;  ;      ;   ;;  ;    ;   ;; ;;  ;   ; 
;   ;      ;      ;   ;; ;   ;      ;   ;        ;   ;   ;  ;     
;    ;;;   ;      ;;;;;; ;   ;   ;;;;   ;        ;   ;   ;   ;;;  
;       ;  ;      ;      ;   ;  ;   ;   ;        ;   ;   ;      ; 
;   ;   ;  ;;     ;      ;   ;  ;   ;   ;        ;   ;; ;;  ;   ; 
;    ;;;    ;;;;   ;;;;  ;   ;   ;;;;   ;      ;;;;;  ;;;    ;;;  
;                                                                 
;                                                                 
;                                                                 

(module+ examples ;; for buying cards; incomplete tests -- assumes active player starts with 0 score 
  (provide GameBuyTests/ GameTradeTests/ hidden-cards)

  (define (lift-trades s cards)
    (match-define (list args expected msg) s)
    (match-define (list equations trades ts) args)
    (define gs (lift-ts ts cards))
    (list (list equations trades gs) expected msg))

  #; {BuyScenario/Turn [Listof Card] ->  BuyScenario/Game}
  (define (lift-buy s new-cards)
    (match-define (list args expected msg) s)
    (match-define (list cards ts) args)
    (list (list cards (lift-ts ts new-cards)) expected msg))

  #; {Turn [Listof Card] -> Game}
  (define (lift-ts ts cards)
    (define players (cons (t:turn-active ts) '[]))
    (define +player (map (λ (p) (player+ p 'connection)) players))
    (game (t:turn-bank ts) (append (t:turn-cards ts)) cards +player))

  (define hidden-cards `[,c-ggggg* ,c-rrbrr])
  (define GameTradeTests/ (map (λ (t) (lift-trades t hidden-cards)) TradeTests/))
  (define GameBuyTests/ (map (λ (t) (lift-buy t hidden-cards)) BuyTests/))

  (provide g1 g2 g3)
  (define g1 (lift-ts t1 hidden-cards))
  (define g2 (lift-ts t2 hidden-cards))
  (define g3 (lift-ts t3 hidden-cards)))

;                                     
;                                     
;     ;                    ;          
;     ;                    ;          
;   ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;    ;   ;; ;        ;    ;     
;     ;    ;;;;;;  ;;;     ;     ;;;  
;     ;    ;          ;    ;        ; 
;     ;    ;      ;   ;    ;    ;   ; 
;     ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                     
;                                     
;                                     

(module+ test
  ;; test game over for two cases 
  (check-false (game-over? g1))
  (check-true (game-over? g2))
  (check-false (game-over? gs-3-zeros))
  
  ;; tests for the major entry point
  (define player p-r-9)
  (check-equal? (game-bank (legal-pebble-or-trade-request '() #f g1)) (b:bag))
  (check-equal? (let* ([s (legal-pebble-or-trade-request '() #f g1)]
                       [s (game-players s)]
                       [s (first s)]
                       [s (player+-player s)]
                       [s (p:player-wallet s)])
                  s)
                (b:bag RED RED))
  (check-false (legal-pebble-or-trade-request '() #f g2))
  (check-false (legal-pebble-or-trade-request '() #t g3))
  
  #; {Symbol TradesScenarios -> Void}
  (define (run-trades* t scenario*)
    (eprintf "--------------- ~a\n" t)
    (for ([s scenario*] [i (in-naturals)])
      (match-define (list args expected msg) s)
      (match-define (list equations trades gs) args)

      (match (legal-pebble-or-trade-request equations trades gs)
        [(? false?) (check-false #false expected )~a msg "/illegal"]
        [(game a-bank a-visibles _cards (cons (player+ a-active 'connection) others))
         (match expected
           [(? false?) 'xyz]
           [(list wallet bank)
            (check b:bag-equal? (p:player-wallet a-active) wallet (~a msg "/wallet"))
            (check b:bag-equal? a-bank bank (~a msg "/bank"))])])))
      
  (run-trades* 'TradeTests/ GameTradeTests/)

  #; {Symbol LegalScenarios -> Void}
  (define (run-buy-scenario t scenario*)
    (eprintf "--------------- ~a\n" t)
    (for ([s scenario*] [i (in-naturals)])
      (match-define (list args expected msg) s)
      (match-define (list cards gs) args)
      (match expected
        [(? boolean? expected)
         (check-false (legal-purchase-request cards gs) msg)]
        [(list score e-cards wallet bank)
         (match (legal-purchase-request cards gs)
           [(? false?) (check-false #false expected )~a msg "/illegal"]
           [(game a-bank a-visibles _cards (cons (player+ a-active 'connection) others))
            (check-equal? (p:player-score a-active) score (~a msg "/score"))
            (define cards (append e-cards hidden-cards))
            (check b:bag-equal? (apply b:bag a-visibles) (apply b:bag cards) (~a msg "/cards"))
            (check b:bag-equal? (p:player-wallet a-active) wallet (~a msg "/wallet"))
            (check b:bag-equal? a-bank bank (~a msg "/bank"))])])))

  (run-buy-scenario 'GameBuyTests GameBuyTests/))

;                                                                                             
;      ;;                                                                                     
;     ;                           ;       ;                        ;;;       ;     ;          
;     ;                           ;                                  ;             ;          
;   ;;;;;  ;   ;  ; ;;    ;;;   ;;;;;   ;;;    ;;;   ; ;;   ;;;;     ;     ;;;   ;;;;;  ;   ; 
;     ;    ;   ;  ;;  ;  ;;  ;    ;       ;   ;; ;;  ;;  ;      ;    ;       ;     ;    ;   ; 
;     ;    ;   ;  ;   ;  ;        ;       ;   ;   ;  ;   ;      ;    ;       ;     ;     ; ;  
;     ;    ;   ;  ;   ;  ;        ;       ;   ;   ;  ;   ;   ;;;;    ;       ;     ;     ; ;  
;     ;    ;   ;  ;   ;  ;        ;       ;   ;   ;  ;   ;  ;   ;    ;       ;     ;     ; ;  
;     ;    ;   ;  ;   ;  ;;       ;       ;   ;; ;;  ;   ;  ;   ;    ;       ;     ;     ;;   
;     ;     ;;;;  ;   ;   ;;;;    ;;;   ;;;;;  ;;;   ;   ;   ;;;;     ;;   ;;;;;   ;;;    ;   
;                                                                                         ;   
;                                                                                        ;    
;                                                                                       ;;    

#; {GameState -> GameState}
(define (rotate gs)
  (match-define [game bank visibles cards players] gs)
  (game bank visibles cards (list-rotate+ players)))

;; ---------------------------------------------------------------------------------------------------
#; {GameState -> GameState}
;; ASSUME there is an active player 
(define (kick gs)
  (match-define [game bank visibles cards players] gs)
  (game bank visibles cards (rest players)))

;; ---------------------------------------------------------------------------------------------------
#; {GameState -> TurnState}
(define (extract-turn rs)
  (match-define [game bank visibles _cards player-states] rs)
  (define active (first player-states))
  (define others (rest player-states))
  (t:turn bank visibles (player+-player active) (extract-score others)))

#; {[Listof Player+] -> [Listof Score]}
(define (extract-score players+)
  (map (compose p:player-score player+-player) players+))

;; ---------------------------------------------------------------------------------------------------
#; {GameState [#:award-bonus (Player -> Player)] -> [List [Listof Actor] [Listof Actor]] }
(define (determine-winners-and-losers gs (award-bonus (λ (p) p)))
  (define players (map (player+-award-bonus award-bonus) (game-players gs)))
  (define winners (if (empty? players) '[] (all-argmax player+-score players)))
  (define losers  (if (empty? players) '[] (set-subtract players winners)))
  (list (map player+-connection winners) (map player+-connection losers)))

(define player+-score (compose p:player-score player+-player))

;; ---------------------------------------------------------------------------------------------------
#; {GameState -> Pict}
(define (render gs)
  (match-define [game bank visibles cards player+*] gs)
  (define p-bank     (b:render bank))
  (define p-visibles (c:render* visibles))
  (define p-cards    (c:render* cards))
  (define p-players  (render* player+*))
  (t:combine p-bank p-visibles p-cards p-players))

;                                     
;                                     
;     ;                    ;          
;     ;                    ;          
;   ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;    ;   ;; ;        ;    ;     
;     ;    ;;;;;;  ;;;     ;     ;;;  
;     ;    ;          ;    ;        ; 
;     ;    ;      ;   ;    ;    ;   ; 
;     ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                     
;                                     
;                                     

(module+ pict
  'gs1
  (render gs1))

(module+ test
  (check-equal? (parameterize ([name* 'x]) (jsexpr->game (game->jsexpr gs1))) gs1)

  (check-equal? (rotate gs-20) gs-20-rotate)

  (check-equal? (kick gs1) gs-no-players)
 
  (check-equal? (determine-winners-and-losers gs-20) (list '[x] '[y])))

(module+ test
  #; {Symbol UsefulScenarios {#:check [Equality Thunk Any String -> Void]} -> Void}
  (define (run-scenario* t scenario*)
    (eprintf "--------------- ~a\n" t)
    (for ([s scenario*] [i (in-naturals)])
      (match-define (list gs ts msg) s)
      (show gs ts msg)
      (check-equal? (extract-turn gs) ts msg)))

  (define (show gs ts msg)
    (define p-gs  (render gs))
    (define p-ts  (t:render ts))
    (define is (text "---->" "roman" 12))
    (define all (ht-append 5 p-gs is p-ts))
    (eprintf "~a\n" msg)
    (pretty-print (frame (inset all 10) #:line-width 4) (current-error-port)))
                             
    

  (run-scenario* 'ForStudents ForStudents/)
  (run-scenario* 'Tests Tests/))