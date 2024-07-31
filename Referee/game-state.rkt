#lang racket

;; the referee's game state representation, including "connections" to the actual players

(provide
 #; {type GameState}
 game?

 #; {GameState -> TurnState}
 extract-turn)

(module+ examples
  (provide gs1))

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

(require (submod Bazaar/Common/cards examples))
(require (submod Bazaar/Common/bags examples))
(require (submod Bazaar/Common/player examples))

(require (submod Bazaar/Common/bags json))
(require (submod Bazaar/Common/cards json))
(require (submod Bazaar/Common/player json))

(require (prefix-in b: Bazaar/Common/bags))
(require (prefix-in p: Bazaar/Common/player))
(require (prefix-in c: Bazaar/Common/cards))

(require Bazaar/Common/turn-state)

(require Bazaar/Lib/configuration)
(require (prefix-in b: Bazaar/Common/bags))
(require (prefix-in p: Bazaar/Common/player))

(require Bazaar/Lib/configuration)
(require Bazaar/Lib/parse-json)

(require pict)

(module+ pict
  (require (submod ".." examples)))

(module+ test
  (require (submod ".." examples))
  (require (submod Bazaar/Common/turn-state json))
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

(def/jsexpr-> player+*
  #:array [(list (app jsexpr->player (? p:player? p)) ...) (map (λ (p) (player+ p 'unknown)) p)])

(struct/description
 game
 [bank     #:to-jsexpr bag->jsexpr   #:from-jsexpr jsexpr->bag   #:is-a "*Pebbles"]
 [visibles #:to-jsexpr card*->jsexpr #:from-jsexpr jsexpr->card* #:is-a "*Cards"]
 [cards    #:to-jsexpr card*->jsexpr #:from-jsexpr jsexpr->card* #:is-a "*Cards"]
 [players  #:to-jsexpr player+*->jsexpr #:from-jsexpr jsexpr->player+*
           #:is-a "*Players"])

#; {type GameState = (game Bag [Listof Card] [Listof Card] [Listof Player+])}
#; {type Player+   = (player+ Player {Object with name method})}

(module+ examples
  (define gs1 (game b-ggggg [list c-ggggg c-rrbrr c-rgbrg] (list) (list (player+ p-r6 'unknown)))))

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

#; {GameState -> TurnState}
(define (extract-turn rs)
  (match-define [game bank visibles _cards player-states] rs)
  (define active (first player-states))
  (define others (rest player-states))
  (turn-state bank visibles active (extract-score others)))

#; {[Listof Player+] -> [Listof Score]}
(define (extract-score players+)
  (map (compose p:player-score player+-player) players+))

;; ---------------------------------------------------------------------------------------------------
#; {GameState -> Pict}
(define (render rs)
  (match-define [game bank visibles cards player+*] rs)
  (define p-bank     (b:render bank))
  (define p-visibles (c:render* visibles))
  (define p-players  (render-players player+*))
  (frame (inset (hb-append 10 p-bank p-visibles (apply hb-append 5 p-players)) 2)))

#; {[Listof Player+] -> [Listof Pict]}
(define (render-players player+*)
  (for/list ([p player+*])
    (define name (if (is-a? p object%) (send p name) "unknown"))
    (p:render (player+-player p) #:name name)))

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
  (render gs1))

(module+ test
  (check-equal? (jsexpr->game (game->jsexpr gs1)) gs1))