#lang racket

;; the referee: a state machine that sets up a GameState, iterates over it by granting turns,
;; until the game is over. It then informs the winners and losers of the outcome. 
;; ---------------------------------------------------------------------------------------------------

#; {Configuration [Listof Player] -> Boolean}
(define (matching-number state0 players)
  (let* ([sop#    (gs:player-count state0)]
         [player# (length players)])
    (cond
      #;
      [(unit-test-mode) #true]
      [(not (= sop# player#))
       (eprintf "player counts dont match:\n in state:   ~a\n in players: ~a\n" sop# player#)
       #false]
      [(not (<= #; MIN-PLAYERS sop# MAX-PLAYERS))
       (eprintf "player count doesn't match Bazaar rules: ~a\n" sop#)
       #false]
      [else #true])))

(provide
 (contract-out
  [referee/state
   (->i ([players (listof player/c)] [eqs (listof e:1eq?)] [gs gs:game?])
        #:pre/name (players) "players must have distince names"
        (distinct? (map (λ (p) (send p name)) players))
        #:pre/name (gs players) "matching number of players"
        (matching-number gs players)
        (r [list/c [listof player/c] [listof player/c]]))]))

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

(require Bazaar/scribblings/spec)

(require (prefix-in e: Bazaar/Common/equations))
(require Bazaar/Common/player-interface)
(require Bazaar/Player/strategies)
(require (prefix-in gs: Bazaar/Referee/game-state))

(require Bazaar/Lib/xsend)

(require SwDev/Contracts/unique)

(module+ examples
  (require (submod ".."))
  (require (except-in (submod Bazaar/Common/equations examples) ForStudents/ Tests/))
  (require (submod Bazaar/Referee/game-state examples))
  (require Bazaar/Player/mechanism)
  (require SwDev/Testing/scenarios))

(module+ test
  (require (submod ".."))
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
  (define return (xsend active setup equations))
  (match return
    [(? failed?) (values (gs:kick gs) (cons active kicked))]
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
      [(? failed?) (values winners (cons p kicked))]
      [(? string?) (values winners (cons p kicked))]
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

  (define eq++ `[,ggg=b ,r=bbbb ,r=gggg])
  
  (define 2players [list (create-player "Adam" purchase-points) (create-player "Eve" purchase-size)])
  (define 3players (append 2players (list (create-player "Carl"))))
  (define 6players (append 3players (map create-player '["Dan" "Felix" "Grace"])))

  #;{String [Purchase -> Natural] String -> PlayerObject}
  (define (create-exn-player name which xn)
    (define factory (retrieve-factory xn exn-raising-table-for-7))
    (create-player name which #:bad factory))
  
  (define z (create-exn-player "Zeina" purchase-points "setup"))
  (define 2p+setup-exn (cons z 2players))
  
  (define y (create-exn-player "Yolanda" purchase-points "request-pebble-or-trades"))
  (define 1p-setup-rpt-exn (list z y (first 2players)))
  
  (define x (create-exn-player "Xena" purchase-size "request-cards"))
  (define 3exn-players (list x y z))
  
  (define w (create-exn-player "Willhelmina" purchase-size "win"))
  (define 2+4-players (list* x y z w 2players))

  (define v (create-exn-player "Veronica" purchase-size "win"))
  (define a-1+5-players (list v x y z w (first 2players)))
  (define b-1+5-players (list (second 2players) v x y z w))
  
  (define u (create-exn-player "Uria" purchase-size "request-cards"))
  (define 6-exn-players (list v x y z w u))

  (define adam `[["Adam"] []])
  (define adam-eve `[["Adam" "Eve"] []])
  (define eve `[["Eve"] []])

  #;
  [observe #true] (referee/state b-1+5-players eq++ gs-6-players++))

(module+ examples ;; ForStudents/
  (simple+ Simple/ (list 2players '[] gs-20) adam "no action, 1 winner")
  (simple+ Simple/ (list 3players '[] gs-3-zeros) `[["Carl" "Adam"] []] "2 buys, 2 winners")
  (simple+ Simple/ (list 2p+setup-exn `[,ggb=rw] gs-3-zeros) `[["Eve"] ["Zeina"]] "setup exn")
  (simple+ Simple/ (list 6players `[,ggg=b] gs-6-players) adam "1 trade, 1 buy, 1 winner")
  (simple+ Simple/ (list 1p-setup-rpt-exn `[,ggb=rw] gs-3-zeros) `[["Adam"] ["Zeina" "Yolanda"]] "2"))

(module+ examples ;; Tests/
  
  (let ([strange-1
    #<< here
  scenario 1:
    cards all display colors 1 and 2
    players get colors 3 and 4
    equations use colors 3 and 4
    bank has plenty of colors 3 and 4
     --> players continue to make exchanges unti all cards disappear
 here
    ])
    (simple+ Complex/ (list 2players `[,r=bbbb] gs-10++) adam-eve "strange-1"))

  (let ([strange-2
    #<< here
   scenario 2: 
    bank has no colors showing up in any of the equations 
    card has only colors from bank 
    players cannot trade, cannot buy
    players can request pebbles until bank is exchausted 
     --> referee terminates game per force 
 here
    ])
    (simple+ Complex/ (list 2players `[,ggb=rw] gs-10--) adam-eve "strange-2"))

  (let ([r `[ [] ["Zeina" "Xena" "Yolanda"]]])
    (simple+ Complex/ (list 3exn-players `[,ggb=rw] gs-3-zeros) r "3 drops"))

  (let ([r `[[] ["Zeina" "Willhelmina" "Xena" "Yolanda"]]])
    (simple+ Complex/ (list 2+4-players eq++ gs-6-players++) r "good one lose, bad wins"))

  (let ([r `[[] ["Zeina" "Willhelmina" "Xena" "Yolanda" "Veronica"]]])
    (simple+ Complex/ (list a-1+5-players eq++ gs-6-players++) r "2 bad winners"))

  (let ([r `[["Eve"] ["Zeina" "Willhelmina" "Xena" "Yolanda" "Veronica"]]])
    (simple+ Complex/ (list b-1+5-players eq++ gs-6-players++) r "an actual winner, five drops"))

  (simple+ Complex/ (list '[] '[] gs-no-players) `[[] []] "no players, stop immediately")
  (simple+ Complex/ (list 3players eq++ gs-3-zeros++) `[["Adam"] []] "2 buys, 2 winners")
  (simple+ Complex/ (list 6players eq++ gs-6-players++) adam "all get turns")

  (let ([r `[[] ["Uria" "Zeina" "Willhelmina" "Xena" "Yolanda" "Veronica"]]])
    (simple+ Complex/ (list 6-exn-players eq++ gs-6-players++) r "6 drop outs")))

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
    (define count 0)
    (eprintf "--------------- ~a\n" t)
    (for ([s scenario*] [i (in-naturals 1)])
      (set! count i) 
      (match-define (list args exp msg) s)
      (match-define (list players equations gs) args)
      #;
      (show t i msg equations players gs)
      (eprintf "-- test ~a ~a\n" msg i)
      (check-equal? (dev/null (w+do->names (referee/state players equations gs))) (sort2 exp) msg))
    (eprintf "done: ~a tests\n" count))

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
      (sort2 (list t u))))

  (define (sort2 l)
    (match-define [list t u] l)
    (list (sort t string<=?) (sort u string<=?)))
    

  (run-scenario* 'simple Simple/)
  (run-scenario* 'complex Complex/))
