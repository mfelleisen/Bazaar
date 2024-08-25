#lang racket

;; the referee: a state machine that sets up a GameState, iterates over it by granting turns,
;; until the game is over. It then informs the winners and losers of the outcome. 
;; ---------------------------------------------------------------------------------------------------

(provide
 referee/state)

(module+ examples
  #; {RefScenarios  = [Listof 1RefScenario]}
  #; {1RefScenario = [List [List [Listof PlayerObject] Equations GameState] Result]}
  #; {Result       = [List [Listof Player] [Listof Player]]} 
  (provide Simple/ Complex/))

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

(require (prefix-in e: Bazaar/Common/equations))
(require Bazaar/Player/strategies)
(require (prefix-in gs: Bazaar/Referee/game-state))
(require Bazaar/Lib/xsend)

(module+ examples
  (require (except-in (submod Bazaar/Common/equations examples) ForStudents/ Tests/))
  (require (submod Bazaar/Referee/game-state examples))
  (require Bazaar/Player/mechanism)
  (require SwDev/Testing/scenarios))

(module+ test
  (require (submod ".." examples))
  (require (submod Bazaar/Player/mechanism json))
  (require SwDev/Lib/should-be-racket)
  (require rackunit))

;                                                   
;                    ;;                             
;                   ;                               
;                   ;                               
;    ;;;;   ;;;   ;;;;;   ;;;    ;;;;   ;;;    ;;;  
;    ;;  ; ;;  ;    ;    ;;  ;   ;;  ; ;;  ;  ;;  ; 
;    ;     ;   ;;   ;    ;   ;;  ;     ;   ;; ;   ;;
;    ;     ;;;;;;   ;    ;;;;;;  ;     ;;;;;; ;;;;;;
;    ;     ;        ;    ;       ;     ;      ;     
;    ;     ;        ;    ;       ;     ;      ;     
;    ;      ;;;;    ;     ;;;;   ;      ;;;;   ;;;; 
;                                                   
;                                                   
;                                                   

#;
(define (referee player*)
  (define gs0 (gs:create-random-game-state (length players)))
  (define eq0 (e:create-random-equations))
  (referee/state player* eq0 gs0))

#; {[Listof PlayerObject] Equations GameState -> [List [Listof Player] [Listof Player]]}
(define (referee/state player* equations gs)
  (let*-values ([(gs0)                           (gs:connect gs player*)]
                [(gs-post-setup setup-drop-outs) (apply values (setup equations gs0 player*))]
                [(gs-post-turns turn-drop-outs)  (apply values (run-turns equations gs-post-setup))]
                [(winners win-lose-drop-outs)    (apply values (inform-players gs-post-turns))])
    [list winners (append setup-drop-outs turn-drop-outs win-lose-drop-outs)]))

;                                     
;                                     
;                   ;                 
;                   ;                 
;    ;;;    ;;;   ;;;;;  ;   ;  ;;;;  
;   ;   ;  ;;  ;    ;    ;   ;  ;; ;; 
;   ;      ;   ;;   ;    ;   ;  ;   ; 
;    ;;;   ;;;;;;   ;    ;   ;  ;   ; 
;       ;  ;        ;    ;   ;  ;   ; 
;   ;   ;  ;        ;    ;   ;  ;; ;; 
;    ;;;    ;;;;    ;;;   ;;;;  ;;;;  
;                               ;     
;                               ;     
;                               ;     

#; {Equations GameState -> [List GameState [Listof PlayerObject]]}
(define (setup equations gs0 players)
  (for/fold ([gs gs0] [kicked '()] #:result (list gs kicked)) ([active players])
    (setup-1-player equations active gs kicked)))

#;{Equations PlayerObject GameState [Listof PlayerObject] -> (values GameState [Listof PlayerObject])}
(define (setup-1-player equations active gs kicked)
  (match (xsend active setup equations)
    [(? string?) (values (gs:kick gs) (cons active kicked))]
    [_           (values (gs:rotate gs) kicked)]))

;                                     
;                                     
;     ;                               
;     ;                               
;   ;;;;;  ;   ;   ;;;;  ; ;;    ;;;  
;     ;    ;   ;   ;;  ; ;;  ;  ;   ; 
;     ;    ;   ;   ;     ;   ;  ;     
;     ;    ;   ;   ;     ;   ;   ;;;  
;     ;    ;   ;   ;     ;   ;      ; 
;     ;    ;   ;   ;     ;   ;  ;   ; 
;     ;;;   ;;;;   ;     ;   ;   ;;;  
;                                     
;                                     
;                                     

#; {Equations GameState -> [List GameState [Listof PlayerObject]]}
(define (run-turns equations gs-post-setup)
  (let until-end ([gs gs-post-setup] [kicked '()])
    (when [observe] (pretty-print (list (e:render* equations) (gs:render gs)) (current-error-port)))
    (cond
      [(gs:game-over? gs) (list gs kicked)]
      [else
       (match (one-turn equations gs)
         [(list gs active) (until-end gs (cons active kicked))]
         [gs (until-end gs kicked)])])))

#; {Equations GameState -> (U GameState [List GameState PlayerObject])}
(define (one-turn equations gs)
  (define active  (gs:game-active gs))
  (define action1 (xsend active request-pebble-or-trades (gs:extract-turn gs)))
  (when [observe] (eprintf "~a is trading ~a\n" (xsend active name) action1))
  (match (gs:legal-pebble-or-trade-request equations action1 gs)
    [#false (list (gs:kick gs) active)]
    [gs
     (define action2 (xsend active request-cards (gs:extract-turn gs)))
     (when [observe] (eprintf "~a is buying ~a\n" (xsend active name) action2))
     (cond
       [(failed? action2) (list (gs:kick gs) active)]
       [else 
        (match (gs:legal-purchase-request action2 gs)
          [#false  (list (gs:kick gs) active)]
          [gs (gs:rotate gs)])])]))

(define observe (make-parameter #false))

;                                                   
;                                                   
;                                                   
;                                                   
;  ;     ;  ;;;;  ;;;;   ;;;;          ;   ;  ;;;;  
;  ;     ;  ;;  ;     ;  ;; ;;         ;   ;  ;; ;; 
;   ; ; ;   ;         ;  ;   ;         ;   ;  ;   ; 
;   ; ; ;   ;      ;;;;  ;   ;         ;   ;  ;   ; 
;   ;; ;;   ;     ;   ;  ;   ;         ;   ;  ;   ; 
;   ;; ;;   ;     ;   ;  ;; ;;         ;   ;  ;; ;; 
;    ; ;    ;      ;;;;  ;;;;           ;;;;  ;;;;  
;                        ;                    ;     
;                        ;                    ;     
;                        ;                    ;     

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

(module+ examples
  (setup-scenarios simple+ Simple/ Complex/)
  
  (define 2players [list (create-player "Adam" purchase-points) (create-player "Eve" purchase-size)])
  (define 3players (append 2players (list (create-player "Carl"))))
  (define 6players (append 3players (map create-player '["Dan" "Felix" "Grace"])))

  (define adam `[["Adam"] []])
  (define adam-eve `[["Adam" "Eve"] []])
  (define eve `[["Eve"] []])

  [observe #true] (referee/state 2players `[,ggb=rw] gs-10--))

(module+ examples ;; ForStudents/
  (simple+ Simple/ (list 2players '[] gs-20) adam "no action, 1 winner")
  (simple+ Simple/ (list 3players '[] gs-3-zeros) `[["Carl" "Adam"] []] "2 buys, 2 winners")
  (simple+ Simple/ (list 6players `[,ggg=b] gs-6-players) adam "1 trade, 1 buy, 1 winner"))

(module+ examples ;; Tests/
  
  (define strange-1
    #<< here
  scenario 1:
    cards all display colors 1 and 2
    players get colors 3 and 4
    equations use colors 3 and 4
    bank has plenty of colors 3 and 4
     --> players continue to make exchanges unti all cards disappear
 here
    )
  (simple+ Complex/ (list 2players `[,r=bbbb] gs-10++) adam-eve "strange-1")

  (define strange-2
    #<< here
   scenario 2: 
    bank has no colors showing up in any of the equations 
    card has only colors from bank 
    players cannot trade, cannot buy
    players can request pebbles until bank is exchausted 
     --> referee terminates game per force 
 here
    )

  (define eq++ `[,ggg=b ,r=bbbb ,r=gggg])

  (simple+ Complex/ (list 2players `[,ggb=rw] gs-10--) adam-eve "strange-2")
  (simple+ Complex/ (list '[] '[] gs-no-players) `[[] []] "no players, stop immediately")
  (simple+ Complex/ (list 3players eq++ gs-3-zeros++) `[["Adam"] []] "2 buys, 2 winners")
  (simple+ Complex/ (list 6players eq++ gs-6-players++) adam "all get turns"))

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
  #; {Symbol SimpleScenarios -> Void}
  (define (run-scenario* t scenario*)
    (eprintf "--------------- ~a\n" t)
    (for ([s scenario*] [i (in-naturals)])
      (match-define (list args expected msg) s)
      (match-define (list players equations gs) args)
      #;
      (show t i msg equations players gs)
      (check-equal?  (dev/null (w+do->names (referee/state players equations gs))) expected msg)))

  #; {Symbol N Equations* [Listof OlayerObject] GameState -> Void}
  (define (show t i  msg equations players gs)
    (eprintf "~a ~a ~a\n" t msg i)
    (pretty-print (e:render* equations) (current-error-port))
    (pretty-print (gs:render (gs:connect gs players)) (current-error-port))
    (pretty-print (player*->jsexpr players) (current-error-port))
    (eprintf "--------------------------------------------------------\n"))

  #; {[List [Listof PlayerObject] [Listof PlayerObject]] -> [List [Listof String] [Listof String]]}
  (define (w+do->names p)
    (let* ([s p]
           [t (map (λ (player) (xsend player name)) (first s))]
           [u (map (λ (player) (xsend player name)) (second s))])
      (list t u)))

  (run-scenario* 'simple Simple/)
  (run-scenario* 'complex Complex/))
