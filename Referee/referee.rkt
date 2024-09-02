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
      [(not (<= sop# MAX-PLAYERS))
       (eprintf "player count doesn't match Bazaar rules: ~a\n" sop#)
       #false]
      [else #true])))

(provide
 (contract-out
  [referee/state
   (->i ([players (listof player/c)] [eqs (listof e:1eq?)] [gs gs:game?])
        ([observers (listof any/c)])
        #:pre/name (players) "players must have distince names"
        (distinct? (map (λ (p) (send p name)) players))
        #:pre/name (gs players) "matching number of players"
        (matching-number gs players)
        (r [list/c [listof player/c] [listof player/c]]))]))

(module+ examples
  (provide run-scenario-with-observer void-observer%)

  #; {RefScenarios  = [Listof 1RefScenario]}
  #; {1RefScenario = [List [List [Listof Actor] Equations GameState] Result]}
  #; {Result       = [List [Listof Player] [Listof Player]]} 
  (provide Simple/ Complex/)

  (provide 8Simple/ 8Complex/)

  (provide 9Simple/ 9Complex/))

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

(require Bazaar/Common/player-interface)
(require (prefix-in mo: Bazaar/Referee/manage-observers))
(require (prefix-in e: Bazaar/Common/equations))
(require (prefix-in gs: Bazaar/Referee/game-state))

(require Bazaar/Lib/xsend)

(require SwDev/Contracts/unique)

(module+ examples
  (require (except-in (submod Bazaar/Common/equations examples) ForStudents/ Tests/))
  (require (submod Bazaar/Referee/game-state examples))
  (require Bazaar/Player/mechanism)
  (require Bazaar/Player/strategies)
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

#; {[Listof Actor] Equations GameState [Listof Observer] -> [List [Listof Player] [Listof Player]]}
(define (referee/state actor* equations gs0 (observer* `[]))
  (define mo (new mo:manage-observers%))
  (send mo add* observer*)
  (send mo state 'initial equations 'setup gs0)
  (let*-values ([(gs->setup setup-drop-outs)  (apply values (setup equations gs0 actor* mo))]
                [(gs->turns turn-drop-outs)   (apply values (run-turns equations gs->setup mo))]
                [(winners win-lose-drop-outs) (apply values (inform-players gs->turns))])
    (define drop-outs (append setup-drop-outs turn-drop-outs win-lose-drop-outs))
    (send mo end (get-names winners) (get-names drop-outs))
    [list winners drop-outs]))

#; {[Lisof Actor] -> [Listof String]}
(define (get-names actors)
  (map (λ (x) (send x name)) actors))
  

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

#; {Equations GameState [Listof Actor] MO -> [List GameState [Listof Actor]]}
;; the PlayerOhjects and and the GameState players are aligned
;; OUCH || data structure 
(define (setup equations gs0 actor* observers)
  (for/fold ([gs gs0] [kicked '()] #:result (list gs kicked)) ([active actor*])
    (define name (send active name))
    (match (setup-1-player equations active gs)
      [(? gs:game? gs)
       (send observers state (~a "setup/success: " name) equations 'okay gs)
       (values gs kicked)]
      [(list (? gs:game? gs) active)
       (send observers state (~a "setup/failure: " name) equations 'fail gs)
       (values gs (cons active kicked))])))

#;{Equations Actor GameState -> (values GameState [Listof Actor])}
(define (setup-1-player equations active gs)
  (define return (xsend active setup equations))
  (match return
    [(? failed?) (list (gs:kick gs) active)]
    [(? string?) (list (gs:kick gs) active)]
    [_           (gs:rotate (gs:connect gs active))]))

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

#; {Equations GameState MO -> [List GameState [Listof Actor]]}
(define (run-turns equations gs-post-setup observers)
  (let until-end ([gs gs-post-setup] [kicked '()])
    (cond
      [(gs:game-over? gs) (list gs kicked)]
      [else
       (match (one-turn equations gs observers)
         [(list gs active) (until-end gs (cons active kicked))]
         [(list gs)        (until-end gs kicked)])])))

#; {Equations GameState MO -> (U GameState [List GameState Actor])}
(define (one-turn equations gs observers)
  (define active  (gs:game-active gs))
  (define name    (send active name))
  (define report  (report-to observers equations name))
  ;; let the turn begin:
  (define action1 (xsend active request-pebble-or-trades (gs:extract-turn gs)))
  (define gs++    (gs:legal-pebble-or-trade-request equations action1 gs))
  (cond
    [(false? gs++)
     (report trade-bad action1 (gs:kick gs) active)]
    [(gs:game-over? gs++)
     (report trade-ok-end  action1 gs++)]
    [else
     (report trade-ok action1 gs++)
     (define action2 (xsend active request-cards (gs:extract-turn gs++)))
     (cond
       [(failed? action2)
        (report buy-bad-comm action2 (gs:kick gs++) active)]
       [else 
        (match (gs:legal-purchase-request action2 gs++)
          [#false (report buy-bad-logic action2 (gs:kick gs++) active)]
          [gs++++ (report buy-ok action2 (gs:rotate gs++++))])])]))

#; {MO Equations String-> [String String GameState [[Listof Actor]] -> [Cons Game [Listof Actor]]]}
;; send a message to the observers about `name`s actions and return the game state with the 1 drop-out
(define ((report-to observers equations name) msg action gs++ . active)
  (send observers state (~a name " is " msg) equations action gs++)
  (cons gs++ active))

;; constants for reporting actions 
(define trade-bad     "requesting a pebble or pebble exchanges and failed")
(define trade-ok-end  "requesting a pebble or pebble exchanges with success and ending the game")
(define trade-ok      "requesting a pebble or pebble exchanges with success")
(define buy-bad-comm  "buying cards, failed due to communication problem")
(define buy-bad-logic "buying cards, failed due to game logic problem")
(define buy-ok        "buying cards with success")

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

#; {GameStatr -> [List [Listof Actor] [Listof Actor]]}
(define (inform-players gs-post-turns)
  (match-define [list winners losers] (gs:determine-winners-and-losers gs-post-turns))
  (match-define [list true-winners w-drop-outs] (final-inform winners #true))
  (match-define [list true-losers l-drop-outs]  (final-inform losers #false))
  (list true-winners (append w-drop-outs l-drop-outs)))

#; {[Listof Actor] Boolean -> [List [Listof Actor] [Listof Actor]]}
(define (final-inform players msg)
  (for/fold ([winners '()] [kicked '()] #:result (list winners kicked)) ([p players])
    (match (xsend p win msg)
      [(? failed?) (values winners (cons p kicked))]
      [(? string?) (values winners (cons p kicked))]
      [_           (values (cons p winners) kicked)])))

;                                                                               
;                                                                               
;   ;;;;;;                                                     ;                
;       ;;                                                                      
;       ;          ;;;    ;;;    ;;;   ; ;;   ;;;;    ;;;;   ;;;    ;;;    ;;;  
;       ;         ;   ;  ;;  ;  ;;  ;  ;;  ;      ;   ;;  ;    ;   ;; ;;  ;   ; 
;      ;          ;      ;      ;   ;; ;   ;      ;   ;        ;   ;   ;  ;     
;      ;           ;;;   ;      ;;;;;; ;   ;   ;;;;   ;        ;   ;   ;   ;;;  
;     ;               ;  ;      ;      ;   ;  ;   ;   ;        ;   ;   ;      ; 
;     ;           ;   ;  ;;     ;      ;   ;  ;   ;   ;        ;   ;; ;;  ;   ; 
;    ;             ;;;    ;;;;   ;;;;  ;   ;   ;;;;   ;      ;;;;;  ;;;    ;;;  
;                                                                               
;                                                                               
;                                                                               

(module+ examples ;; players for milestone 7
  (define eq1 `[,ggg=b ,r=bbbb ,r=gggg])
  (define eq2 `[,w=bbbb ,rg=bbbb ,ggb=rw ,ggg=b ,r=bbbb ,r=gggg, ggg=r])

  (define a (create-player "Adam" purchase-points))
  (define b (create-player "Bettina" purchase-points))
  (define c (create-player "Carl"))
  (define d (create-player "Dan"))
  (define e (create-player "Eve" purchase-size))
  (define f (create-player "Felix"))
  (define g (create-player "Grace"))
  (define d-f-g (list d f g))
  
  (define 2players [list a e])
  (define 3players (append 2players (list c)))
  (define 6players (append 3players d-f-g))

  #;{String [Purchase -> Natural] String -> Actor}
  (define (create-exn-player name which xn)
    (define factory (retrieve-factory xn exn-raising-table-for-7))
    (create-player name which #:bad factory))
  
  (define z (create-exn-player "Zeina" purchase-points "setup"))
  (define y (create-exn-player "Yolanda" purchase-points "request-pebble-or-trades"))
  (define x (create-exn-player "Xena" purchase-size "request-cards"))
  (define w (create-exn-player "Willhelmina" purchase-size "win"))
  (define v (create-exn-player "Veronica" purchase-size "win"))
  (define u (create-exn-player "Uria" purchase-size "request-cards"))
  
  (define z-a-e (cons z 2players))
  (define z-y-a (list z y a))
  (define x-y-z (list x y z))
  (define x-y-z-w-a-e (list* x y z w 2players))
  (define v-x-y-z-w-a (list v x y z w a))
  (define v-x-y-z-w-second-of-2 (list e v x y z w))
  (define v-x-y-z-w-u (list v x y z w u)))

(module+ examples ;; outcomes for games abstracted over drop-outs
  (define (ff x) (map (λ (a) (send a name)) x))
  (define [default . x]    `[[] ,(ff x)])
  (define [adam . x]       `[["Adam"] ,(ff x)])
  (define [bettina . x]    `[["Bettina"] ,(ff x)])
  (define [adam-eve . x]   `[["Adam" "Eve"] ,(ff x)])
  (define [carl-adam . x]  `[["Adam" "Carl"] ,(ff x)])
  (define [eve . x]        `[["Eve"] ,(ff x)])
  (define [felix . x]      `[["Felix"] ,(ff x)]))

;; ---------------------------------------------------------------------------------------------------
(module+ examples ;; ForStudents/ in 7
  (setup-scenarios simple+ Simple/ Complex/)

  (simple+ Simple/ (list 2players '[] gs-20) [adam] "no action, 1 winner")
  (simple+ Simple/ (list 3players '[] gs-3-zeros) [carl-adam] "2 buys, 2 winners")
  (simple+ Simple/ (list z-a-e `[,ggb=rw] gs-3-zeros) (eve z) "setup exn")
  (simple+ Simple/ (list 6players `[,ggg=b] gs-6-players) [felix] "1 trade, 1 winner")
  (simple+ Simple/ (list z-y-a `[,ggb=rw] gs-3-zeros) `[["Adam"] ["Zeina" "Yolanda"]] "2"))

;; ---------------------------------------------------------------------------------------------------
(module+ examples ;; Tests/ in 7
  
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
    (simple+ Complex/ (list 2players `[,r=bbbb] gs-10++) [adam-eve] "strange-1"))

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
    (simple+ Complex/ (list 2players `[,ggb=rw] gs-10--) [adam-eve] "strange-2"))

  (simple+ Complex/ (list x-y-z `[,ggb=rw] gs-3-zeros) (default z x y) "3 drops")
  (simple+ Complex/ (list x-y-z-w-a-e eq1 gs-6-players++) (default z w x y) "good ones lose")
  (simple+ Complex/ (list v-x-y-z-w-a eq1 gs-6-players++) (default z w x y v) "2 bad ws")
  (simple+ Complex/ (list v-x-y-z-w-second-of-2 eq1 gs-6-players++) (eve z w x y v) "1 w, 5 drops")
  (simple+ Complex/ (list '[] '[] gs-no-players) [default] "no players, stop immediately")
  (simple+ Complex/ (list 3players eq1 gs-3-zeros++) [eve] "2 buys, 2 winners")
  (simple+ Complex/ (list 6players eq1 gs-6-players++) [adam] "all get turns")
  (simple+ Complex/ (list v-x-y-z-w-u eq1 gs-6-players++) (default u z w x y v) "6 drop outs"))

;                                                                               
;                                                                               
;    ;;;;                                                      ;                
;   ;    ;                                                                      
;   ;    ;         ;;;    ;;;    ;;;   ; ;;   ;;;;    ;;;;   ;;;    ;;;    ;;;  
;   ;    ;        ;   ;  ;;  ;  ;;  ;  ;;  ;      ;   ;;  ;    ;   ;; ;;  ;   ; 
;    ;;;;         ;      ;      ;   ;; ;   ;      ;   ;        ;   ;   ;  ;     
;   ;;  ;;         ;;;   ;      ;;;;;; ;   ;   ;;;;   ;        ;   ;   ;   ;;;  
;   ;    ;            ;  ;      ;      ;   ;  ;   ;   ;        ;   ;   ;      ; 
;   ;    ;        ;   ;  ;;     ;      ;   ;  ;   ;   ;        ;   ;; ;;  ;   ; 
;    ;;;;          ;;;    ;;;;   ;;;;  ;   ;   ;;;;   ;      ;;;;;  ;;;    ;;;  
;                                                                               
;                                                                               
;                                                                               

(module+ examples ;; cheating players for milestone 8
  #;{String [Purchase -> Natural] String -> Actor}
  (define (create-cheating-player name which xn)
    (define factory (retrieve-factory xn cheater-table-for-8))
    (create-player name which #:bad factory))

  (define q (create-cheating-player "Quixote" purchase-size "bank-cannot-trade"))
  (define p (create-cheating-player "Paul" purchase-points "use-non-existent-equation"))
  (define o (create-cheating-player "Olivia" purchase-points "use-non-existent-equation"))
  (define n (create-cheating-player "Norma" purchase-size "wallet-cannot-trade"))
  (define m (create-cheating-player "Michelle" purchase-points "buy-invisible-card"))
  (define l (create-cheating-player "Laura" purchase-size "wallet-cannot-buy-card"))

  (define o-a-e (cons o 2players))
  (define o-p-a (list o p a))
  (define o-p-v-x-y-z (list o v x p y z))
  (define o-p-v-x-a-q (list o p v x a q))
  (define n-o-a-d-f-g (list* n o a d-f-g))
  (define m-l-a-d-f-g (list* m l a d-f-g))
  (define q-p-o-n-m-l (list q p o n m l))
  (define v-n-m       (list v n m)))

(module+ examples ;; ForsTudents/ in 8
  (setup-scenarios 8simple+ 8Simple/ 8Complex/)
  
  (8simple+ 8Simple/ (list o-a-e `[,ggb=rw] gs-3-zeros) (eve o) "wrong eq")
  (8simple+ 8Simple/ (list o-p-v-x-y-z `[,ggb=rw] gs-6-players++) (default o p v x y z) "exn & weq")
  (8simple+ 8Simple/ (list o-p-v-x-a-q `[,ggb=rw] gs-6-players++) (adam o p v x q) "ok, exn & bt"))

(module+ examples ;; Tests/ in 8
  (8simple+ 8Complex/ (list x-y-z `[,ggb=rw] gs-3-zeros) (default z x y) "3 drops")
  (8simple+ 8Complex/ (list q-p-o-n-m-l eq1 gs-6-players++) (default q p o n m l) "all cheating")
  (8simple+ 8Complex/ (list x-y-z-w-a-e eq1 gs-6-players++) (default z w x y) "good ones lose")
  (8simple+ 8Complex/ (list v-x-y-z-w-a eq1 gs-6-players++) (default z w x y v) "2 bad ws")
  (8simple+ 8Complex/ (list v-x-y-z-w-second-of-2 eq1 gs-6-players++) (eve z w x y v) "1 w, 5 drops")
  (8simple+ 8Complex/ (list o-p-v-x-a-q `[,ggb=rw] gs-6-players) (adam o p v x) "ok, exn & bt")
  (8simple+ 8Complex/ (list o-p-a `[,ggb=rw] gs-3-zeros) (adam o p) "wrong eq")
  (8simple+ 8Complex/ (list m-l-a-d-f-g `[,ggb=rw] gs-6-players++) (felix m l) "ok, exn & bt")
  (8simple+ 8Complex/ (list n-o-a-d-f-g `[,ggb=rw] gs-6-players) (felix n o) "ok, wt, weq")
  (8simple+ 8Complex/ (list v-n-m `[,ggb=rw] gs-3-zeros) (default v n) "ok, wt, weq"))

;                                                                               
;                                                                               
;    ;;;;                                                      ;                
;   ;;  ;                                                                       
;   ;    ;         ;;;    ;;;    ;;;   ; ;;   ;;;;    ;;;;   ;;;    ;;;    ;;;  
;   ;    ;        ;   ;  ;;  ;  ;;  ;  ;;  ;      ;   ;;  ;    ;   ;; ;;  ;   ; 
;   ;;  ;;        ;      ;      ;   ;; ;   ;      ;   ;        ;   ;   ;  ;     
;    ;;; ;         ;;;   ;      ;;;;;; ;   ;   ;;;;   ;        ;   ;   ;   ;;;  
;        ;            ;  ;      ;      ;   ;  ;   ;   ;        ;   ;   ;      ; 
;   ;   ;         ;   ;  ;;     ;      ;   ;  ;   ;   ;        ;   ;; ;;  ;   ; 
;    ;;;           ;;;    ;;;;   ;;;;  ;   ;   ;;;;   ;      ;;;;;  ;;;    ;;;  
;                                                                               
;                                                                               
;                                                                               

(module+ examples ;; infinite loops players for milestone 9

  #;{String [Purchase -> Natural] String -> Actor}
  (define (create-inf-player name which xn)
    (define factory (retrieve-factory xn infinite-loop-table-for-9))
    (create-player name which #:bad factory))

  (define t (create-inf-player "Theresa" purchase-size "setup-1"))
  (define s (create-inf-player "Susanne" purchase-size "request-cards-2"))
  (define k (create-inf-player "Klaus" purchase-points "request-cards-1"))
  (define j (create-inf-player "John" purchase-size "request-pebble-or-trades-5"))
  (define i (create-inf-player "Ian" purchase-size "request-pebble-or-trades-3"))
  (define h (create-inf-player "Hannah" purchase-points "win-1"))

  (define t-a-e (cons t 2players))
  (define s-a-e (cons s 2players))

  (define s-t-k-d-f-g (list* s t k d-f-g))
  (define s-t-k-b-f-g (list  s t k f b g))
  (define j-t-k-d-f-g (list* j t k d-f-g))
  (define i-t-k-d-f-g (list* i t k d-f-g))
  (define h-t-k-d-f-g (list  f t k d h g)))

(module+ examples ;; ForsTudents/ in 9
  (setup-scenarios 9simple+ 9Simple/ 9Complex/)
  (9simple+ 9Simple/ (list o-a-e `[,ggb=rw] gs-3-zeros) (eve o) "wrong eq")
  (9simple+ 9Simple/ (list s-t-k-d-f-g eq1 gs-6-players++) (felix s t k) "2 for 9")
  (9simple+ 9Simple/ (list s-t-k-b-f-g eq2 gs-6-players++) (bettina s t k) "bettina"))
   
(module+ examples ;; Tests/ in 9
  (9simple+ 9Complex/ (list x-y-z-w-a-e eq1 gs-6-players++) (default z w x y) "good ones lose")
  (9simple+ 9Complex/ (list v-x-y-z-w-a eq1 gs-6-players++) (default z w x y v) "2 bad ws")
  (9simple+ 9Complex/ (list v-x-y-z-w-second-of-2 eq1 gs-6-players++) (eve z w x y v) "1 w, 5 drops")
  (9simple+ 9Complex/ (list o-p-v-x-a-q `[,ggb=rw] gs-6-players) (adam o p v x) "ok, exn & bt")
  (9simple+ 9Complex/ (list o-p-a `[,ggb=rw] gs-3-zeros) (adam o p) "wrong eq")
  (9simple+ 9Complex/ (list `[,a  ,b ,c] eq2 gs-3-zeros) `[["Adam" "Bettina" "Carl"] []] "just fine")
  (9simple+ 9Complex/ (list s-a-e eq1 gs-3-zeros) `[["Adam" "Eve" "Susanne"] []] "inf 2")
  (9simple+ 9Complex/ (list i-t-k-d-f-g eq2 gs-6-players++++) (felix i t k) "grace: 2 trades, 2 buys")
  (9simple+ 9Complex/ (list j-t-k-d-f-g eq2 gs-6-players++++) (felix j t k) "large")
  (9simple+ 9Complex/ (list h-t-k-d-f-g eq2 gs-6-players++++) (default h t k) "win lose"))

;                                                                                      
;          ;                                                                           
;          ;                                                                 ;         
;          ;                                                                           
;    ;;;   ;;;;    ;;;           ;;;    ;;;    ;;;   ; ;;   ;;;;    ;;;;   ;;;    ;;;  
;   ;; ;;  ;; ;;  ;   ;         ;   ;  ;;  ;  ;;  ;  ;;  ;      ;   ;;  ;    ;   ;; ;; 
;   ;   ;  ;   ;  ;             ;      ;      ;   ;; ;   ;      ;   ;        ;   ;   ; 
;   ;   ;  ;   ;   ;;;           ;;;   ;      ;;;;;; ;   ;   ;;;;   ;        ;   ;   ; 
;   ;   ;  ;   ;      ;             ;  ;      ;      ;   ;  ;   ;   ;        ;   ;   ; 
;   ;; ;;  ;; ;;  ;   ;         ;   ;  ;;     ;      ;   ;  ;   ;   ;        ;   ;; ;; 
;    ;;;   ;;;;    ;;;           ;;;    ;;;;   ;;;;  ;   ;   ;;;;   ;      ;;;;;  ;;;  
;                                                                                      
;                                                                                      
;                                                                                      

(module+ examples
  (define void-observer%
  (class object%
    (super-new)
    (define/public (state msg equations action gs)
      (eprintf "state ~a : ~a\n" msg action))
    (define/public (end winners drop-outs)
      (eprintf "the end:\n ~a\n ~a\n" winners drop-outs))))

  (define (run-scenario-with-observer scenarios i observer)
    (match-define [list args expected msg] (list-ref scenarios (sub1 i)))
    (match-define [list actors equations gs] args)
    (referee/state actors equations gs (list observer))
    (eprintf "should yield ~a\n [~a]\n" expected msg))

  #;
  (run-scenario-with-observer Simple/ 5 (new void-observer%)))
  
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
      (eprintf "-- test ~a  ~a: ~a\n" msg t i)
      (define o (list (new void-observer%)))
      (check-equal? (dev/null (-->names (referee/state players equations gs #;o))) (sort2 exp) msg))
    (eprintf "done: ~a tests\n" count))
  
  #; {[List [Listof Actor] [Listof Actor]] -> [List [Listof String] [Listof String]]}
  (define (-->names s)
    (sort2 (list (get-names (first s)) (get-names (second s)))))

  (define (sort2 l)
    (match-define [list t u] l)
    (list (sort t string<=?) (sort u string<=?))))

(module+ test
  (run-scenario* 'simple Simple/)
  (run-scenario* 'complex Complex/))

(module+ test
  (run-scenario* '8simple 8Simple/)
  (run-scenario* '8complex 8Complex/))

(module+ test
  (run-scenario* '9simple 9Simple/)
  (run-scenario* '9complex 9Complex/))
