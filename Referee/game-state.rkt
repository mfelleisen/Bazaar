#lang racket

;; the referee's game state representation, including "connections" to the actual players
;; ---------------------------------------------------------------------------------------------------

(provide
 #; {type GameState}
 game?

 #; {GameState -> Boolean}
 game-over?

 #; {GameState -> GameState}
 kick

 #; {GameState -> GameState}
 rotate 

 #; {GameState -> TurnState}
 extract-turn)

(module+ examples
  (provide gs0 gs1 gs-no-players gs-20 gs-20-rotate gs1+g-r+1)

  #; {type UsefulScenarios = [Listof 1Scenario]}
  #; {type 1Scenario       = [List GameState TurnState]}
  (provide ForStudents/ Tests/))

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
(require (submod Bazaar/Common/player json))

(require (prefix-in b: Bazaar/Common/bags))
(require (prefix-in p: Bazaar/Common/player))
(require (prefix-in c: Bazaar/Common/cards))

(require (prefix-in ts: Bazaar/Common/turn-state))

(require Bazaar/Lib/configuration)
(require (prefix-in b: Bazaar/Common/bags))
(require (prefix-in p: Bazaar/Common/player))

(require Bazaar/Lib/configuration)
(require Bazaar/Lib/parse-json)

(require SwDev/Lib/list)

(require pict)

(module+ pict
  (require (submod ".." examples)))

(module+ examples
  (require (submod Bazaar/Common/turn-state examples)))

(module+ test
  (require (submod ".." examples))
  (require (submod Bazaar/Common/pebbles examples))
  (require (submod Bazaar/Common/turn-state examples))
  (require rackunit))
  
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

(struct player+ [player connection] #:prefab)

(define (player+*->jsexpr p*)
  (map (compose player->jsexpr player+-player) p*))

(define name* (make-parameter 'unknown)) ;; for testing
(def/jsexpr-> player+*
  #:array [(list (app jsexpr->player (? p:player? p)) ...) (map (λ (p) (player+ p [name*])) p)])

(struct/description
 game
 [bank     #:to-jsexpr bag->jsexpr   #:from-jsexpr jsexpr->bag   #:is-a "*Pebbles"]
 [visibles #:to-jsexpr card*->jsexpr #:from-jsexpr jsexpr->card* #:is-a "*Cards"]
 [cards    #:to-jsexpr card*->jsexpr #:from-jsexpr jsexpr->card* #:is-a "*Cards"]
 [players  #:to-jsexpr player+*->jsexpr #:from-jsexpr jsexpr->player+*
           #:is-a "*Players"])

#; {type GameState = (game Bag [Listof Card] [Listof Card] [Listof Player+])}
#; {type Player+   = (player+ Player {Object with name method take-turn method setup win})}

(module+ examples
  (define gs0 (game b-ggggg (list) (list) (list (player+ p-r6 'unknown))))
  (define gs1 (game b-ggggg [list c-ggggg c-rrbrr c-rgbrg] (list) (list (player+ p-r6 'x))))
  (define gs1+g-r+1 (game b-ggggg [list c-ggggg c-rrbrr c-rgbrg] (list) (list (player+ p-g7 'x))))
  
  (define gs-no-players (game b-ggggg [list c-ggggg c-rrbrr c-rgbrg] (list) (list)))
  (define gs-20 (game b-r [list c-ggggg] (list) (list (player+ p-rrbrr-20 'x) (player+ p-r6 'y))))
  (define gs-20-rotate ;; a final state shouldn't be rotated 
    (game b-r [list c-ggggg] (list) (list (player+ p-r6 'y) (player+ p-rrbrr-20 'x)))))

(module+ examples ;; test scenarios

  #; {(scenaro+ GameState TurnState msg:String)}
  (define-syntax-rule (scenario+ kind actual expected msg)
    (set! kind (append kind (list [list actual expected msg]))))

  (define ForStudents/ '[])
  (scenario+ ForStudents/ gs0 ts0 "even end of game states can be serialized to JSON")
  (scenario+ ForStudents/ gs1 ts1 "a one-palayer state goes JSON")
  (scenario+ ForStudents/ gs-20 ts-20 "a one-palayer state goes JSON")

  (define Tests/ '[])

  )



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
;; update active player: add `delta` to score, add `plus` its pebbles, subtract `minus` 
(define (update-pebbles-and-score gs delta plus minus)
  (match-define [game bank visibles cards players] gs)
  (let* ([s (first players)]
         [o (player+-connection s)]
         [s (player+-player s)]
         [s (p:update-score s delta)]
         [s (p:update-pebbles s plus minus)])
    (game bank visibles cards (cons (player+ s o) (rest players)))))

;; ---------------------------------------------------------------------------------------------------
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
#; {GameState -> Boolean}
(define (game-over? gs)
  (match-define [game _bank visibles cards players] gs)
  (or (empty? players)
      (winning-points? (first players))
      (and (b:bag-empty? cards) (b:bag-empty? visibles))))

#; {Player+ -> Boolean}
(define (winning-points? p+)
  (match-define [player+ p _] p+)
  (p:winning-points? p))

;; ---------------------------------------------------------------------------------------------------
#; {GameState -> TurnState}
(define (extract-turn rs)
  (match-define [game bank visibles _cards player-states] rs)
  (define active (first player-states))
  (define others (rest player-states))
  (ts:turn-state bank visibles (player+-player active) (extract-score others)))

#; {[Listof Player+] -> [Listof Score]}
(define (extract-score players+)
  (map (compose p:player-score player+-player) players+))

;; ---------------------------------------------------------------------------------------------------
#; {GameState -> Pict}
(define (render gs)
  (match-define [game bank visibles cards player+*] gs)
  (define p-bank     (b:render bank))
  (define p-visibles (c:render* visibles))
  (define p-players  (render-players player+*))
  (frame (inset (hb-append 10 p-bank p-visibles (apply hb-append 5 p-players)) 2)))

#; {[Listof Player+] -> [Listof Pict]}
(define/contract (render-players player+*)
  (-> (listof player+?) (listof pict?))
  (for/list ([q player+*])
    (match-define [player+ p o] q)
    (define name (if (is-a? o object%) (send o name) "unknown"))
    (p:render p #:name name)))

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
  
  (check-equal? (update-pebbles-and-score gs1 1 b-g b-r) gs1+g-r+1)
  
  (check-true (game-over? gs0) "no cards left")
  (check-true (game-over? gs-no-players) "no players left ")
  (check-true (game-over? gs-20) "player has enough points")
  
  (check-false (game-over? gs1)))

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
    (define p-ts  (ts:render ts))
    (define is (text "---->" "roman" 12))
    (define all (ht-append 5 p-gs is p-ts))
    (eprintf "~a\n" msg)
    (pretty-print (frame (inset all 10) #:line-width 4) (current-error-port)))
                             
    

  (run-scenario* 'ForStudents ForStudents/))