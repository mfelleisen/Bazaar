#lang racket

;; the rules of the Bazaar game: legality and scoring
;; ---------------------------------------------------------------------------------------------------

(provide 
 (contract-out
  [game-over?
   ;; determines whether the game is over, i.e.
   ;; (1) all players have been eliminated at the end of a player’s turn;
   ;; (2) a player has 20 points at the end of its turn;
   ;; (3) all cards have been bought up; or
   ;; (4) the bank is empty and no player can buy
   (-> (listof p:player?) (listof c:card?) b:bag? boolean?)]

  (legal-pebble-or-trade-request
   ;; determines whether a pebble request or a requested series of trades is
   ;; (1) legal according to the equations
   ;; (2) feasible for the active player's wallet && the current state of the  bank
   ;; and if so, computes the resulting pebble or wallet and bag 
   (-> (listof e:1eq?) any/c t:turn? (or/c #f (list/c q:pebble? b:bag?) (list/c b:bag? b:bag?))))

  [legal-purchase-request
   ;; determine whether a series of card purchases is
   ;; (1) legal accroding to the visible cards
   ;; (2) feasible for the active player's wallet
   ;; and if so, computes the resulting score, visibles, wallet, and state of the bank
   (-> (listof c:card?) t:turn? (or/c #false (list/c natural? (listof c:card?) b:bag? b:bag?)))]

  [calculate-points
   ;; determine the number of points that the purchase of a card yields 
   (-> c:card? natural? natural?)]
  
  [can-buy
   ;; selects all those cards that can be be bought with this bag (as a first card)
   (-> (listof c:card?) b:bag? (listof c:card?))]))

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  #;
  (scenario+ TradeTests/ (list equations trades turn) (or/c #false (list bag bag)))
  (provide TradeTests/ ForStudents/))

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

(require (prefix-in a: Bazaar/Common/actions))
(require (prefix-in b: Bazaar/Common/bags))
(require (prefix-in c: Bazaar/Common/cards))
(require (prefix-in e: Bazaar/Common/equations))
(require (prefix-in p: Bazaar/Common/player))
(require (prefix-in q: Bazaar/Common/pebbles))
(require (prefix-in t: Bazaar/Common/turn-state))

(require SwDev/Lib/should-be-racket)

(module+ examples
  (require (submod Bazaar/Common/bags examples))
  (require (submod Bazaar/Common/cards examples))
  (require (submod Bazaar/Common/equations examples))
  (require (submod Bazaar/Common/player examples))
  (require SwDev/Testing/scenarios))

(module+ test
  (require (submod ".." examples))
  (require (submod Bazaar/Common/bags examples))
  (require (submod Bazaar/Common/cards examples))
  (require (except-in (submod Bazaar/Common/equations examples) ForStudents/))
  (require (submod Bazaar/Common/pebbles examples))
  (require rackunit))

;                                                                                      
;                                                                      ;               
;   ;;;                         ;;;             ;                      ;               
;     ;                           ;             ;                      ;               
;     ;     ;;;    ;;;;  ;;;;     ;           ;;;;;   ;;;;  ;;;;    ;;;;   ;;;    ;;;  
;     ;    ;;  ;  ;;  ;      ;    ;             ;     ;;  ;     ;  ;; ;;  ;;  ;  ;   ; 
;     ;    ;   ;; ;   ;      ;    ;             ;     ;         ;  ;   ;  ;   ;; ;     
;     ;    ;;;;;; ;   ;   ;;;;    ;             ;     ;      ;;;;  ;   ;  ;;;;;;  ;;;  
;     ;    ;      ;   ;  ;   ;    ;             ;     ;     ;   ;  ;   ;  ;          ; 
;     ;    ;      ;; ;;  ;   ;    ;             ;     ;     ;   ;  ;; ;;  ;      ;   ; 
;      ;;   ;;;;   ;;;;   ;;;;     ;;           ;;;   ;      ;;;;   ;;;;   ;;;;   ;;;  
;                     ;                                                                
;                  ;  ;                                                                
;                   ;;                                                                 

(define (legal-pebble-or-trade-request equations request ts)
  (cond
    [(a:want-pebble? request)
     (legal-pebble-request ts)]
    [(a:trades? request)
     (legal-trades-request equations request ts)]
    [else
     #false]))

#; {Turn -> (U False [List Pebble Bag])}
(define (legal-pebble-request ts)
  (define bank (t:turn-bank ts))
  (cond
    [(b:bag-empty? bank) #false]
    [else
     (define p (b:bag (b:bag-pick-random bank)))
     (list (b:bag-add (p:player-wallet (t:turn-active ts)) p) (b:bag-minus bank p))]))

#; ((listof e:1eq?) (listof e:1eq?) t:turn? . -> . (or/c #false (list/c b:bag? b:bag?)))
;; check legality of a trades relative to the equations and state of the wallet/bank
(define (legal-trades-request equations trades ts)
  (let/ec failure
    (unless (subset? trades equations) (failure #false))
    (define bank0   (t:turn-bank ts))
    (define wallet0 (p:player-wallet (t:turn-active ts)))
    (for/fold ([wallet wallet0] [bank bank0] #:result (list wallet bank)) ([t trades])
      (define left  (e:1eq-left t))
      (define right (e:1eq-right t))
      (unless (b:subbag? left wallet) (failure #false))
      (unless (b:subbag? right bank) (failure #false))
      (b:bag-transfer wallet bank left right))))

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  (setup-scenarios scenario+ TradeTests/ ForStudents/)
  
  (define eqs  (list r-g=4xb))
  (define eqs- (list r-g=4xb-))
  (define bad  (list r=4xg))
  (define two  (append eqs- bad))

  (let ([t (t:turn b-bbbbb '[] (p:player b-rg 0) '[])])
    (scenario+ ForStudents/ (list eqs- eqs t) (list b-bbbb (b:bag-add b-rg b-b)) "1 ok trade"))

  (let ([t (t:turn b-rg '[] (p:player b-ggg 9) '[])])
    (scenario+ ForStudents/ (list eqs- '() t) (list b-ggg b-rg) "no trades"))

  (let ([t (t:turn b-bbbb '[] (p:player b-r 9) '[])])
    (scenario+ ForStudents/ (list eqs- eqs t) #false "bad wallet/trade"))

  (scenario+ TradeTests/ (list eqs- eqs (t:turn b-r '[] (p:player b-rg 9) '[])) #f "bad bank/trade")

  (let ([t (t:turn b-r '[] (p:player b-r 9) '[])])
    (scenario+ TradeTests/ (list eqs- bad t) #false "trade not covered by equations"))

  (define 5turn  (t:turn (b:bag-add b-gggg b-bbbbb) '[] (p:player (b:bag-add b-r b-r) 9) '[]))
  (let ([wallet (b:bag-add b-ggg b-bbbb)]
        [bank  (b:bag-add (b:bag-add b-rg b-b) b-r)])
    (scenario+ TradeTests/ (list two (append bad eqs) 5turn) `[,wallet ,bank] "2 trades"))
  
  (scenario+ TradeTests/ (list two (append bad eqs-) 5turn) #false "2 trades bad")

  (provide t1 t2 t3)
  (define player (p:player b-r 9))
  (define t1 (t:turn b-r '[] player '[]))
  (define t2 (t:turn (b:bag) '[] player '[]))
  (define t3 (t:turn (b:bag) '[] player '[])))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-equal? (list r-g=4xb) (list r-g=4xb-) "regression")

  ;; tests for the major entry point
  (check-equal? (legal-pebble-or-trade-request '() #f t1) `[,(b:bag RED RED) []])
  (check-false (legal-pebble-or-trade-request '() #f t2))
  (check-false (legal-pebble-or-trade-request '() #t t3))

  #; {Symbol LegalScenarios -> Void}
  (define (run-trades* t scenario*)
    (define legal-trades legal-pebble-or-trade-request)
    (eprintf "--------------- ~a\n" t)
    (for ([s scenario*] [i (in-naturals)])
      (match-define (list args expected msg) s)
      (match-define (list equations trades turn) args)
      #;
      (show i equations trades turn expected)
      (match expected
        [(? boolean? expected)
         (check-false (legal-trades equations trades turn) msg)]
        [(list wallet bank)
         (check b:bag-equal? (first (legal-trades equations trades turn)) wallet (~a msg "/wallet"))
         (check b:bag-equal? (second (legal-trades equations trades turn)) bank (~a msg "/bank"))])))

  (run-trades* 'TradeTests/ TradeTests/)
  (run-trades* 'Students/ ForStudents/))

;                                                                        
;                                             ;                          
;   ;;;                         ;;;           ;                          
;     ;                           ;           ;                          
;     ;     ;;;    ;;;;  ;;;;     ;           ;;;;   ;   ;  ;   ;   ;;;  
;     ;    ;;  ;  ;;  ;      ;    ;           ;; ;;  ;   ;  ;   ;  ;   ; 
;     ;    ;   ;; ;   ;      ;    ;           ;   ;  ;   ;   ; ;   ;     
;     ;    ;;;;;; ;   ;   ;;;;    ;           ;   ;  ;   ;   ; ;    ;;;  
;     ;    ;      ;   ;  ;   ;    ;           ;   ;  ;   ;   ; ;       ; 
;     ;    ;      ;; ;;  ;   ;    ;           ;; ;;  ;   ;   ;;    ;   ; 
;      ;;   ;;;;   ;;;;   ;;;;     ;;         ;;;;    ;;;;    ;     ;;;  
;                     ;                                       ;          
;                  ;  ;                                      ;           
;                   ;;                                      ;;           

(define (legal-purchase-request cards0 ts)
  (define bank0    (t:turn-bank ts))
  (define visibles (t:turn-cards ts))
  (define wallet0  (p:player-wallet (t:turn-active ts)))
  (let/ec failure
    (for/fold ([δ 0] [c visibles] [w wallet0] [b bank0] #:result (list δ c w b)) ([1card cards0])
      (unless (b:subbag? (apply b:bag c) (apply b:bag visibles)) (failure #false))
      (buy-1-card 1card δ c w b failure))))

#; {Card N [Setof Card] Bag Bag {} -> (values N {Setof Card} Bag Bag)}
(define (buy-1-card c score cards wallet bank failure)
  (unless (can-buy-1 c wallet) (failure #false))
  (define-values [wallet++ bank++] (b:bag-transfer wallet bank (c:card-pebbles c) '[]))
  (define delta (calculate-points c (b:bag-size wallet++)))
  (values (+ score delta) (remove c cards) wallet++ bank++))

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  (provide BuyTests/)

  #; {type BuyScenaro = [List [List Cards Turn] (U False [List N Cards Bag Bag]) String]}
  (setup-scenarios buy-s+ BuyTests/)

  (let* ([wallet '()]
         [bank   (b:bag-add b-bbbbb b-rg)]
         [exp    `[5 [] ,wallet ,bank]])
    (buy-s+ BuyTests/ `[[,c-bbbbb] ,(t:turn b-rg `[,c-bbbbb] (p:player b-bbbbb 0) '[])]  exp "1"))

  (let* ([cards `[,c-bbbbb ,c-ggggg]]
         [pebbles (b:bag-add b-bbbbb b-ggggg)]
         [wallet '()]
         [bank    (b:bag-add (b:bag-add b-bbbbb b-rg) b-ggggg)]
         [exp `[6 [] ,wallet ,bank]])
    (buy-s+ BuyTests/ `[,cards ,(t:turn b-rg (reverse cards) (p:player pebbles 0) '[])] exp "2+"))

  (let* ([cards `[,c-bbbbb ,c-ggggg]]
         [wallet '()]
         [bank   (b:bag-add b-bbbbb b-rg)])
    (buy-s+ BuyTests/ `[,cards ,(t:turn b-rg (reverse cards) (p:player b-bbbbb 0) '[])]  #false "2-"))
  
  (let* ([cards `[,c-bbbbb ,c-ggggg]] ;; buy the same card twice 
         [wallet '()]
         [bank   (b:bag-add b-bbbbb b-rg)])
    (buy-s+ BuyTests/ `[,cards ,(t:turn b-rg `[,c-bbbbb ,c-bbbbb] (p:player b-bbbbb 0) '[])]  #f "2"))
  
  (buy-s+ BuyTests/ `[,(list c-bbbbb) ,(t:turn b-bbbbb `[] (p:player b-rg 0) '[])] #f "∉visibles")
  (buy-s+ BuyTests/ `[[,c-bbbbb] ,(t:turn b-rg `[,c-bbbbb] (p:player b-rg 0) '[])] #false "")
  (let* ([cards `(,c-ggggg)]
         [turn  (t:turn b-rrbrr (list c-wyrbb c-ggggg)  p-ggggg (list 0 0))]
         [exp  `[5 [,c-wyrbb] [] ,(b:bag-add b-rrbrr b-ggggg)]])
    (buy-s+ BuyTests/ `[,cards ,turn] exp "regression")))

;; ---------------------------------------------------------------------------------------------------
(module+ test

  (let*-values ([(delta cards wallet bank)
                 (buy-1-card c-ggggg 0 (list c-wyrbb c-ggggg) b-ggggg b-rrbrr values)])
    (check-equal? delta 5)
    (check-equal? cards [list c-wyrbb])
    (check-equal? wallet (b:bag))
    (check-equal? bank (b:bag-add b-rrbrr b-ggggg)))

  #; {Symbol LegalScenarios -> Void}
  (define (run-buy-scenario t scenario*)
    (eprintf "--------------- ~a\n" t)
    (for ([s scenario*] [i (in-naturals)])
      (match-define (list args expected msg) s)
      (match-define (list cards ts) args)
      #;
      (show i equations trades turn expected)
      (match expected
        [(? boolean? expected)
         (check-false (legal-purchase-request cards ts) msg)]
        [(list score e-cards wallet bank)
         (define x (legal-purchase-request cards ts))
         (cond
           [(false? x) 'xxx]
           [else 
            (check-equal? (first x) score (~a msg "/score"))
            (check b:bag-equal? (apply b:bag (second x)) (apply b:bag e-cards) (~a msg "/cards"))
            (check b:bag-equal? (third x) wallet (~a msg "/wallet"))
            (check b:bag-equal? (fourth x) bank (~a msg "/bank"))])])))

  (run-buy-scenario 'BuyTests BuyTests/))

;                                            
;                                            
;                    ;            ;          
;                                 ;          
;   ;;;;    ;;;    ;;;   ; ;;   ;;;;;   ;;;  
;   ;; ;;  ;; ;;     ;   ;;  ;    ;    ;   ; 
;   ;   ;  ;   ;     ;   ;   ;    ;    ;     
;   ;   ;  ;   ;     ;   ;   ;    ;     ;;;  
;   ;   ;  ;   ;     ;   ;   ;    ;        ; 
;   ;; ;;  ;; ;;     ;   ;   ;    ;    ;   ; 
;   ;;;;    ;;;    ;;;;; ;   ;    ;;;   ;;;  
;   ;                                        
;   ;                                        
;   ;                                        

(define (calculate-points card pebbles#)
  (for/first ([p POINTS] #:when (>= pebbles# (points-pebbles-left p)))
    (if (c:card-face? card) (points-with-face p) (points-no-face p))))

(module+ test
  (check-equal? (calculate-points c-rrbrr 0) (points-no-face (last POINTS)))
  (check-equal? (calculate-points c-rrbrr* 0) (points-with-face (last POINTS)))
  (check-equal? (calculate-points c-rrbrr* 6) (points-with-face (first POINTS))))

;                       
;   ;                   
;   ;                   
;   ;                   
;   ;;;;   ;   ;  ;   ; 
;   ;; ;;  ;   ;  ;   ; 
;   ;   ;  ;   ;   ; ;  
;   ;   ;  ;   ;   ; ;  
;   ;   ;  ;   ;   ; ;  
;   ;; ;;  ;   ;   ;;   
;   ;;;;    ;;;;    ;   
;                   ;   
;                  ;    
;                 ;;    

(define (can-buy cards wallet)
  (for/list ([c cards] #:when (can-buy-1 c wallet))
    c))

(define (can-buy-1 c wallet)
  (b:subbag? (c:card-pebbles c) wallet))

(module+ test
  (check-equal? (can-buy (list c-rrbrr c-ggggg) b-rg) (list))
  (check-equal? (can-buy (list c-rrbrr c-ggggg) b-rrbrr) (list c-rrbrr)))

;                                                                               
;                     ;                   ;;                                    
;                     ;                  ;                                      
;                     ;                  ;                                      
;    ;;;   ; ;;    ;;;;          ;;;   ;;;;;          ;;;;  ;;;;  ;;;;;;   ;;;  
;   ;;  ;  ;;  ;  ;; ;;         ;; ;;    ;           ;;  ;      ; ;  ;  ; ;;  ; 
;   ;   ;; ;   ;  ;   ;         ;   ;    ;           ;   ;      ; ;  ;  ; ;   ;;
;   ;;;;;; ;   ;  ;   ;         ;   ;    ;           ;   ;   ;;;; ;  ;  ; ;;;;;;
;   ;      ;   ;  ;   ;         ;   ;    ;           ;   ;  ;   ; ;  ;  ; ;     
;   ;      ;   ;  ;; ;;         ;; ;;    ;           ;; ;;  ;   ; ;  ;  ; ;     
;    ;;;;  ;   ;   ;;;;          ;;;     ;            ;;;;   ;;;; ;  ;  ;  ;;;; 
;                                                        ;                      
;                                                     ;  ;                      
;                                                      ;;                       

(require SwDev/Debugging/spy)

(define (game-over? player* card* bank)
  (or (all-players-eliminated? player*)
      (a-player-has-good-score? player*)
      (all-cards-bought? card*)
      (no-pebbles-no-buyers player* card* bank)))

#; {[Listof Player] -> Boolean}
(define all-players-eliminated? empty?)

#; {[Listof Player] -> Boolean}
(define (a-player-has-good-score? player*)
  (ormap (λ (p) (>= (p:player-score p) PLAYER-WINS)) player*))

#; {[Setof Card] -> Boolea}
(define all-cards-bought? empty?)

#; {[Listof Player] [Setof Card] Bag -> Boolean}
(define (no-pebbles-no-buyers player* card* bank)
  (and (b:bag-empty? bank)
       (no-buyers player* card* )))

#; {[Listof Player] [Setof Card] -> Boolean}
(define (no-buyers player* card* )
  (andmap (λ (p) (empty? (can-buy card* (p:player-wallet p)))) player*))

(module+ examples
  (provide EoG/)
  
  (setup-scenarios eog-s+ EoG/)

  (eog-s+ EoG/ (list '[] '[] (b:bag)) #true "all players eliminated")
  (eog-s+ EoG/ (list `[,p-bbbbb3] '[] (b:bag)) #true "all cards gone")
  (eog-s+ EoG/ (list `[,p-rrbrr-20] '[] (b:bag)) #true "player has score")
  (eog-s+ EoG/ (list `[,p-bbbbb3] `[,c-bbbbb] (b:bag)) #false "player can buy card")
  (eog-s+ EoG/ (list `[,p-bbbbb3] `[,c-ggggg] (b:bag)) #true "no pebbles, no buyers"))

(module+ test

  #; {Symbol LegalScenarios -> Void}
  (define (run-eog-s t scenario*)
    (eprintf "--------------- ~a\n" t)
    (for ([s scenario*] [i (in-naturals)])
      (match-define (list args expected msg) s)
      (match-define (list player* card* bank) args)
      (check-equal? (game-over? player* card* bank) expected msg)))

  (run-eog-s 'EoG-Tests EoG/))

;                                                   
;                                                   
;             ;                                     
;                                                   
;  ;     ;  ;;;   ; ;;   ; ;;    ;;;    ;;;;   ;;;  
;  ;     ;    ;   ;;  ;  ;;  ;  ;;  ;   ;;  ; ;   ; 
;   ; ; ;     ;   ;   ;  ;   ;  ;   ;;  ;     ;     
;   ; ; ;     ;   ;   ;  ;   ;  ;;;;;;  ;      ;;;  
;   ;; ;;     ;   ;   ;  ;   ;  ;       ;         ; 
;   ;; ;;     ;   ;   ;  ;   ;  ;       ;     ;   ; 
;    ; ;    ;;;;; ;   ;  ;   ;   ;;;;   ;      ;;;  
;                                                   
;                                                   
;                                                   

(define (winners player*)
  (all-argmax p:player-score player*))

(module+ examples
  (provide Winners/)
  (setup-scenarios win-s* Winners/)

  (win-s* Winners/ `[,p-bbbbb3 ,p-rrbrr-20 ,p-rrbrr-20] `[,p-rrbrr-20 ,p-rrbrr-20] "2 winners"))

(module+ test
  (define (run-win-s t scenario*)
    (eprintf "--------------- ~a\n" t)
    (for ([s scenario*] [i (in-naturals)])
      (match-define (list args expected msg) s)
      (check-equal? (winners args) expected msg)))

  (run-win-s 'Winners Winners/))
    
  
