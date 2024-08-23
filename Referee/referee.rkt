#lang racket

;; the referee: a state machine that sets up a GameState, iterates over it by granting turns,
;; until the game is over. It then informs the winners and losers of the outcome. 
;; ---------------------------------------------------------------------------------------------------

(provide
 referee/state)

(module+ examples
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

(require (prefix-in gs: Bazaar/Referee/game-state))
(require Bazaar/Lib/xsend)

(module+ examples
  (require (except-in (submod Bazaar/Common/equations examples) ForStudents/ Tests/))
  (require (submod Bazaar/Referee/game-state examples))
  (require Bazaar/Player/mechanism)
  (require SwDev/Testing/scenarios))

(module+ test
  (require (submod ".." examples))
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
  (define gs0 (gs:create-random-game-state))
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

    (pretty-print (gs:render gs) (current-error-port))
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
  ;#;
  (eprintf "~a is trading ~a\n" (xsend active name) action1)
  (match (gs:legal-pebble-or-trade-request equations action1 gs)
    [#false (list (gs:kick gs) active)]
    [gs
     (define action2 (xsend active request-cards (gs:extract-turn gs)))
     ;#;
     (eprintf "~a is buying ~a\n" (xsend active name) action2)
     (match (gs:legal-purchase-request action2 gs)
       [#false (list (gs:kick gs) active)]
       [gs (gs:rotate gs)])]))

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
  (define 2players [list (create-player "Adam") (create-player "Eve")])
  (define 3players (append 2players (list (create-player "Carl"))))
  (define 6players (append 3players (map create-player '["Dan" "Felix" "Grace"])))

  (setup-scenarios simple+ Simple/ Complex/)

  (define adam `[["Adam"] []])
  
  (simple+ Simple/ (list 2players '[] gs-20) adam "no action, 1 winner")
  (simple+ Simple/ (list 3players '[] gs-3-zeros) `[["Carl" "Adam"] []] "2 buys, 2 winners")
  (simple+ Simple/ (list 6players `[,ggg=b] gs-6-players) adam "1 trade, 1 buy, 1 winner")

  (define eq++ `[,ggg=b ,r=bbbb ,r=gggg])
  (simple+ Complex/ (list '[] '[] gs-no-players) `[[] []] "no players, stop immediately")
  (simple+ Complex/ (list 3players eq++ gs-3-zeros++) `[["Carl" "Adam"] []] "2 buys, 2 winners")
  (simple+ Complex/ (list 6players eq++ gs-6-players++) adam "all get turns")

  #;
  (referee/state 3players eq++ gs-3-zeros++))

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
      (dev/null
       (check-equal? (w+do->names (referee/state players equations gs)) expected msg))))

  #; {[List [Listof PlayerObject] [Listof PlayerObject]] -> [List [Listof String] [Listof String]]}
  (define (w+do->names p)
    (let* ([s p]
           [t (map (λ (player) (xsend player name)) (first s))]
           [u (map (λ (player) (xsend player name)) (second s))])
      (list t u)))

  (run-scenario* 'simple Simple/))
