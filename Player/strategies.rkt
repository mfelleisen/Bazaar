#lang racket

;; draft strategy file
;; ---------------------------------------------------------------------------------------------------

#|
question 1: should the player make a request for a random pebble?
  answer 1: only if the player can't perform any trades

question 2: should the player perform trades? 
  answer 2: yes, depending on the answers to question 3  

question 3: should the player buy cards?
  answer 3-points: maximize the number of points a player can get by purchasing cards ... 
  answer 3-beans:  maximize the number of cards a player can purchase ...
    with maximally 3 pebble trades
|#

;; ---------------------------------------------------------------------------------------------------
;; what the core of the player would looke like:

(require Bazaar/Common/player)
(require (only-in Bazaar/Common/turn-state turn))

#; {Turn {Bag -> Purchases} -> [Option Equation*]}
;; #false denotes a request for a random bebble from bank
#; (list 1eq ...) ; denotes a sequence of left-to-right exchanges 
(define (should-request-pebble-or-trade equations turn-state buy-with-wallet)
  (match-define [turn bank visibles p _scores] turn-state)
  (define wallet (player-wallet p))
  (cond
    [(should-the-olayer-request-a-random-pebble equations wallet bank)
     (match-define [purchase cards _]
       (buy-cards wallet buy-with-wallet))
     (set! *to-be-bought cards)
     #false]
    [else
     (match-define [exchange trades [purchase cards _]]
       (trade-then-purchase equations wallet bank buy-with-wallet))
     (set! *to-be-bought cards)
     trades]))

(define *to-be-bought #false)

#; {Turn -> [Listof Card]}
;; the catds that the player wishes to buy, in order 
(define (should-buy-cards turn)
  *to-be-bought)
;; ---------------------------------------------------------------------------------------------------

(module+ examples
  (provide
   #; {type Trade&BuyScenario = [List Equation* Cards Wallet:bag Bank:bag Policy]}
   ForStudents/
   Tests/))

(require (prefix-in b: Bazaar/Common/bags))
(require (prefix-in c: Bazaar/Common/cards))
(require (prefix-in e: Bazaar/Common/equations))
(require (prefix-in c: Bazaar/Common/rule-book))

(require (submod Bazaar/Common/bags examples))
(require (submod Bazaar/Common/cards examples))
(require (submod Bazaar/Common/equations examples))

(require SwDev/Lib/should-be-racket)

(module+ examples
  (require SwDev/Testing/scenarios))

(module+ test
  (require (submod ".." examples)))

;                                                                                             
;                 ;                                                                           
;                 ;                                                                ;      ;;; 
;                 ;                                                                ;     ;   ;
;  ;;;;;;  ;;;;   ;  ;    ;;;           ;;;;   ;;;    ;;;;  ;   ;   ;;;    ;;;   ;;;;;       ;
;  ;  ;  ;     ;  ;  ;   ;;  ;          ;;  ; ;;  ;  ;; ;;  ;   ;  ;;  ;  ;   ;    ;       ;; 
;  ;  ;  ;     ;  ; ;    ;   ;;         ;     ;   ;; ;   ;  ;   ;  ;   ;; ;        ;      ;;  
;  ;  ;  ;  ;;;;  ;;;    ;;;;;;         ;     ;;;;;; ;   ;  ;   ;  ;;;;;;  ;;;     ;      ;   
;  ;  ;  ; ;   ;  ; ;    ;              ;     ;      ;   ;  ;   ;  ;          ;    ;          
;  ;  ;  ; ;   ;  ;  ;   ;              ;     ;      ;; ;;  ;   ;  ;      ;   ;    ;      ;   
;  ;  ;  ;  ;;;;  ;   ;   ;;;;          ;      ;;;;   ;;;;   ;;;;   ;;;;   ;;;     ;;;    ;   
;                                                        ;                                    
;                                                        ;                                    
;                                                        ;                                    

(define (should-the-olayer-request-a-random-pebble equations wallet0 bank0)
  (cons? (e:useful equations wallet0 bank0)))

;                                                                               
;                            ;                              ;                   
;     ;                      ;                  ;;;         ;                   
;     ;                      ;                 ;            ;                   
;   ;;;;;   ;;;;  ;;;;    ;;;;   ;;;           ;            ;;;;   ;   ;  ;   ; 
;     ;     ;;  ;     ;  ;; ;;  ;;  ;          ;;           ;; ;;  ;   ;  ;   ; 
;     ;     ;         ;  ;   ;  ;   ;;         ;;           ;   ;  ;   ;   ; ;  
;     ;     ;      ;;;;  ;   ;  ;;;;;;        ;  ; ;        ;   ;  ;   ;   ; ;  
;     ;     ;     ;   ;  ;   ;  ;             ;  ;;;        ;   ;  ;   ;   ; ;  
;     ;     ;     ;   ;  ;; ;;  ;             ;;  ;         ;; ;;  ;   ;   ;;   
;     ;;;   ;      ;;;;   ;;;;   ;;;;          ;;; ;        ;;;;    ;;;;    ;   
;                                                                           ;   
;                                                                          ;    
;                                                                         ;;    

(struct exchange [trade purchase] #:prefab)
#; {type Exchange = (exchange Equation* Purchases)}
#; (exchange e* p)
;; applying the series of Equations e*, left to right, to the wallet yields as best purchases p

(define (exchange-trade# ex)
  (length (exchange-trade ex)))

(define (exchange-value ex)
  (purchase-points (exchange-purchase ex)))

(define (exchange-cards ex)
  (purchase-cards (exchange-purchase ex)))

(define (null-exchange) (exchange '() null-purchases))

(module+ examples
  (setup-scenarios scenario+ Tests/ ForStudents/)

  (define equations (list r-g=4xb 3xg=r ggb=rw))
  (define cards     (list c-rrbrr* c-ggggg))
  
  (scenario+ ForStudents/ (list equations cards b-4xb-3xg b-rg 'points) (null-exchange) "points 0")

  (define wal (b:bag-add b-rg b-rg b-rg b-4xb-3xg))
  (define ban (b:bag-add b-ggggg b-rrbrr b-rg b-rg))
  (define res-1 [exchange '() (purchase (list c-ggggg) 1)])
  (scenario+ ForStudents/ (list equations cards wal ban 'cards) res-1 "cards 1")
  
  (define res-2 [exchange `(,3xg=r) (purchase (list c-rrbrr*) 2)])
  (scenario+ ForStudents/ (list equations cards wal ban 'points) res-2 "points 2"))

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

#; {Equation* Bag Bag {Bag -> Purchases} -> Exchange}
(define #; /contract (trade-then-purchase equations wallet0 bank0 buy-with-wallet)
  (-> (listof e:1eq?) b:bag? b:bag? (-> b:bag? (λ (x) (purchase? x))) exchange?)
  (define pts (possible-trades equations wallet0 bank0 buy-with-wallet))
  (cond
    [(empty? pts) '()]
    [else (tie-breaker-trade-then-purchase wallet0 pts)]))

#; {Equation* Bag Bag {Bag -> Purchases} -> [Listof [Listof Exchange]]}
;; determine all possible exchanges that apply up to a certain depth 
(define #; /contract (possible-trades equations wallet0 bank0 buy-with-wallet)
  (-> (listof e:1eq?) b:bag? b:bag? (-> b:bag? (λ (x) (purchase? x))) (listof (listof exchange?)))
  #; [Listof [Listof Exchange]]
  (define possibles '[])
  (define p-so-far0 (list (exchange '() (buy-with-wallet wallet0))))
  (let p-t/accu ([wallet wallet0] [bank bank0] [trades-so-far '()] [p-so-far p-so-far0] [fuel 6])
    (define rules (e:useful equations wallet bank))
    (cond
      [(or (empty? rules) (zero? fuel))
       (set! possibles (cons (reverse p-so-far) possibles))]
      [else
       (for ([x rules])
         (define-values (wallet++ bank++) (b:bag-transfer wallet bank (e:1eq-left x) (e:1eq-right x)))
         (define trades   (cons x trades-so-far))
         (define xchange* (cons (exchange (reverse trades) (buy-with-wallet wallet++)) p-so-far))
         (p-t/accu wallet++ bank++ trades xchange* (sub1 fuel)))]))

  possibles)

#; {[Listof Card] [[Listof Purchases] -> Purchases] -> Bag -> Purchases}
;; a function for scoring a list of exchanges 
(define ((buy-from-wallet visibles pick-best) wallet)
  (buy-cards visibles wallet pick-best))

;                                                                                      
;                               ;                           ;                          
;     ;       ;                 ;                           ;         ;                
;     ;                         ;                           ;                          
;   ;;;;;   ;;;    ;;;          ;;;;    ;;;;   ;;;   ;;;;   ;  ;    ;;;   ; ;;    ;;;; 
;     ;       ;   ;;  ;         ;; ;;   ;;  ; ;;  ;      ;  ;  ;      ;   ;;  ;  ;;  ; 
;     ;       ;   ;   ;;        ;   ;   ;     ;   ;;     ;  ; ;       ;   ;   ;  ;   ; 
;     ;       ;   ;;;;;;        ;   ;   ;     ;;;;;;  ;;;;  ;;;       ;   ;   ;  ;   ; 
;     ;       ;   ;             ;   ;   ;     ;      ;   ;  ; ;       ;   ;   ;  ;   ; 
;     ;       ;   ;             ;; ;;   ;     ;      ;   ;  ;  ;      ;   ;   ;  ;; ;; 
;     ;;;   ;;;;;  ;;;;         ;;;;    ;      ;;;;   ;;;;  ;   ;   ;;;;; ;   ;   ;;;; 
;                                                                                    ; 
;                                                                                 ;  ; 
;                                                                                  ;;  

#; {[Listof [Listof Exchange]] -> [Listof Exchange]}
(define #;/contract (tie-breaker-trade-then-purchase wallet0 pts)
  (-> b:bag? (listof (listof exchange?)) exchange?)
  (define the-bests (best-value pts))
  (define shortest  (smallest-number-of-trades the-bests))
  (define richest   (most-pebbles-left wallet0 shortest))
  (when (not (all-equal? richest))
    (error 'trade-then-purchase "tie breaking failed ~a\n" (list wallet0 pts)))
  (first richest))

#; {[Listof [Listof Exchange]] -> [Listof Exchange]}
(define (best-value pts)
  (define best-each (map (λ (ex*) (argmax exchange-value ex*)) pts))
  (define the-max   (exchange-value (argmax exchange-value best-each)))
  (filter (λ (ex) (= (exchange-value ex) the-max)) best-each))

#; {[Listof Exchange] -> [Listof Exchange]}
(define (most-pebbles-left wallet exchanges)
  (define pl (pebbles-left wallet)) 
  (define xx (pl (argmax pl exchanges)))
  (filter (λ (ex) (= (pl ex) xx)) exchanges))

#; {Bag -> Exchange -> Natural}
(define ((pebbles-left wallet) ex)
  (match-define [exchange trade purchases] ex)
  (define wallet++
    (for/fold ([wallet wallet]) ([t trade])
      (define-values (wallet++ _) (b:bag-transfer wallet (b:bag) (e:1eq-left t) (e:1eq-right t)))
      wallet++))
  (b:bag-size wallet++))

#; {[Listof Exchange] -> [Listof Exchange]}
(define (smallest-number-of-trades the-bests)
  (define the-min (exchange-trade# (argmin exchange-trade# the-bests)))
  (filter (λ (ex) (= (exchange-trade# ex) the-min)) the-bests))

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
  #; {Symbol Trade&BuyScenarios {#:check [Equality Thunk Any String -> Void]} -> Void}
  (define (run-scenario* t scenario* #:check (C (λ (equal? act exp m) (check equal? [act] exp m))))
    (eprintf "--------------- ~a\n" t)
    (for ([s scenario*] [i (in-naturals)])
      (match-define (list args expected msg) s)
      (match-define (list equations cards wallet bank policy) args)
      (define score-1  (if (eq? policy 'points) pick-most-points pick-most-cards))
      (define buy-from (buy-from-wallet cards score-1))
      (check-equal? (trade-then-purchase equations wallet bank buy-from) expected msg)))

  (run-scenario* 'ForStudents ForStudents/)
  (run-scenario* 'Tests Tests/))

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
                                 
(struct purchase [cards points] #:prefab)
#; {type Purchases = (purchase [Listof Card] Natural)}
#; (node c* n) ; represent the purchase of cards `c` with `n` points for all purchases

(define (purchase-length p)
  (length (purchase-cards p)))

(define null-purchases (purchase '() 0))

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

#; {[Setof Card] Bag [ [Listof Purchases] -> Purchases ] -> Purchases}
;; the player wishes to purchase the cards in the specified list order 
(define (buy-cards visibles wallet pick-best)
  #; {[Listof Purchases]}
  (define possible (possible-purchases visibles wallet))
  (cond
    [(empty? possible) null-purchases]
    [else (pick-best possible)]))

#; {[Setof Card] Bag [] -> Purchases}
(define (possible-purchases visibles0 wallet0)
  #; {Setof Card}
  (define possibles '[])

  ;; ACCU in reverse order of possible purchaes from `visibles0` & `wallet0` to `visibles` & `wallet`
  (let p-p/accu ([visibles visibles0] [wallet wallet0] [from-root-to-here '()] [points 0])
    #; [Listof Card]
    (define trades (c:can-buy visibles wallet))
    (cond
      [(empty? trades)
       (define proper-order (reverse from-root-to-here))
       (set! possibles (cons (purchase proper-order points) possibles))]
      [else
       (for ([t trades])
         (define visibles--  (remove t visibles))
         (define wallet--    (b:bag-minus wallet (c:card-pebbles t)))
         (define left-over   (b:bag-size wallet--))
         (define points++    (+ (c:calculate-points t left-over) points))
         (define from-root++ (cons t from-root-to-here))
         (p-p/accu visibles-- wallet-- from-root++ points++))]))
  
  possibles)

;; ---------------------------------------------------------------------------------------------------
#; {[Purchase -> Natural] -> [NEListof Purchase]  -> Purchase}
(define ((pick-most f) possible)
  (define first-best (f (argmax f possible)))
  (define all-best   (filter (λ (p*) (= (f p*) first-best)) possible))
  (define just-cards (map purchase-cards all-best))
  (cond
    [(empty? (rest just-cards)) (purchase (first just-cards) first-best)]
    [else 
     (define sorted  (tie-breaker-for-purchases just-cards))
     (when (not (all-equal? sorted))
       (error 'tie-breaker-for-purchases "tie breaking failed, ~a" all-best))
     (purchase (first sorted) first-best)]))

#; {[NEListof Purchases] -> [Listof Card]}
(define pick-most-points (pick-most purchase-points))

#; {[NEListof Purchases] -> [Listof Card]}
(define pick-most-cards (pick-most purchase-length))

;                                                                               
;                               ;                           ;                   
;     ;       ;                 ;                           ;                   
;     ;                         ;                           ;                   
;   ;;;;;   ;;;    ;;;          ;;;;    ;;;;   ;;;   ;;;;   ;  ;    ;;;    ;;;; 
;     ;       ;   ;;  ;         ;; ;;   ;;  ; ;;  ;      ;  ;  ;   ;;  ;   ;;  ;
;     ;       ;   ;   ;;        ;   ;   ;     ;   ;;     ;  ; ;    ;   ;;  ;    
;     ;       ;   ;;;;;;        ;   ;   ;     ;;;;;;  ;;;;  ;;;    ;;;;;;  ;    
;     ;       ;   ;             ;   ;   ;     ;      ;   ;  ; ;    ;       ;    
;     ;       ;   ;             ;; ;;   ;     ;      ;   ;  ;  ;   ;       ;    
;     ;;;   ;;;;;  ;;;;         ;;;;    ;      ;;;;   ;;;;  ;   ;   ;;;;   ;    
;                                                                               
;                                                                               
;                                                                               

#; {[NEListof [Listof Card]] -> [Listof Card]}
;; pick the list of cards that is 
(define (tie-breaker-for-purchases all-best)
  (sort all-best cards<=))

#; {[Listof Card] [Listof Card] -> Boolean}
;; one card sequence is below another seq if the cards are below each other in order 
(define (cards<= purchase-order-1 purchase-order-2)
  (define 1bag (map c:card-pebbles purchase-order-1))
  (define 2bag (map c:card-pebbles purchase-order-2))
  (for/first ([p 1bag] [q 2bag] #:when (c:1card<= p q))
    #true))

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
  
  (require rackunit)
  (check-equal? (buy-cards (list) (b:bag) pick-most-cards)
                (purchase '() 0))
  (check-equal? (buy-cards (list c-ggggg c-ggggg) b-ggggg pick-most-cards)
                (purchase (list c-ggggg) 1))
  (check-equal? (possible-purchases (list c-ggggg c-ggggg) b-ggggg)
                (list (purchase (list c-ggggg) 5) (purchase (list c-ggggg) 5)))

  (check-equal? (buy-cards (list c-ggggg c-ggggg) b-ggggg pick-most-points)
                (purchase (list c-ggggg) 5)))
