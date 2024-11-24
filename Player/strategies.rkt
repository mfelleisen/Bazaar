#lang racket

;; two strategies for a Bazaar player for choosing
;;  (1) whether to request
;;     (1a) a random pebble from the bank
;;     or
;;     (1b) a series of exchanges
;;  (2) whether to buy cards and which one
;; parameterized over a maximization function for just the card purchases 

;; ---------------------------------------------------------------------------------------------------

(provide
 #; {(class/c
      (init-field
       (equations Equations)
       (which-max [Purchase -> Natural]))
      (should-the-player-request-a-random-pebble
       (->m Turn Boolean))
      (trade-then-purchase
       ;; an exchange result contains both a sequence of pebble trades and card purchases
       ;; the player can memoize the latter & need not call the `buy-cards` method once pebbles arrive
       (->m Turn (or/c #false Exchange)))
      (buy-cards
       (->m Turn [Listof Card])))}
 ;; strategy% object are created from the equations that the player receives and the max function
 ;; they provided answers for the three questions that the player needs to take a turn 
 strategy%

 #; {type Exchange = (exchange [Listof Equation] Purchase)}
 ;; an exchange represents a request for pebble trades, equations used from left to right,
 ;; plus a request for purchasing cards in a given order 
 exchange?
 exchange-cards
 exchange-trades
 exchange-purchase

 #; {type Purchase}
 purchase?
 ; purchase-cards
 purchase-walletω

 #; {Purchase -> Natural}
 ;; the value of the cards purchased 
 purchase-points
 
 #; {Purchase -> Natural}
 ;; the number of cards purchased 
 purchase-size)

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  (provide
   #; {type Trade&BuyScenario = [List Equation* [Listof Card] Wallet:bag Bank:bag Policy]}
   ForStudents/
   Tests/))

;; ---------------------------------------------------------------------------------------------------
(module+ json
  (provide
   PCards PPoints
   policy->jsexpr
   jsexpr->policy))
  
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

(require (prefix-in b: Bazaar/Common/bags))
(require (prefix-in c: Bazaar/Common/cards))
(require (prefix-in e: Bazaar/Common/equations))
(require (prefix-in c: Bazaar/Common/rule-book))
(require (prefix-in p: Bazaar/Common/player))
(require (prefix-in t: Bazaar/Common/turn-state))

(require Bazaar/Lib/tie-breaking)

(require SwDev/Lib/should-be-racket)
(require pict)

(module+ examples
  (require (submod ".."))
  (require (submod Bazaar/Common/bags examples))
  (require (submod Bazaar/Common/cards examples))
  (require (submod Bazaar/Common/equations examples))
  (require (submod Bazaar/Common/turn-state examples))

  (require SwDev/Testing/scenarios))

(module+ test
  (require (submod ".." examples))
  (require (submod ".." json))
  (require (submod Bazaar/Common/bags examples))
  (require (submod Bazaar/Common/cards examples))
  (require rackunit))

;                                                                 
;          ;                                                      
;     ;    ;                           ;;;                        
;     ;    ;                             ;                        
;   ;;;;;  ; ;;    ;;;           ;;;     ;    ;;;;    ;;;    ;;;  
;     ;    ;;  ;  ;;  ;         ;;  ;    ;        ;  ;   ;  ;   ; 
;     ;    ;   ;  ;   ;;        ;        ;        ;  ;      ;     
;     ;    ;   ;  ;;;;;;        ;        ;     ;;;;   ;;;    ;;;  
;     ;    ;   ;  ;             ;        ;    ;   ;      ;      ; 
;     ;    ;   ;  ;             ;;       ;    ;   ;  ;   ;  ;   ; 
;     ;;;  ;   ;   ;;;;          ;;;;     ;;   ;;;;   ;;;    ;;;  
;                                                                 
;                                                                 
;                                                                 

(define strategy%
  (class object%
    (init-field equations which) 
    (super-new)
    
    (define/public (should-the-player-request-a-random-pebble turn)
      (define bank0   (t:turn-bank turn))
      (define wallet0 (p:player-wallet (t:turn-active turn)))
      (f-should-the-player-request-a-random-pebble equations wallet0 bank0))

    (define/public (trade-then-purchase turn)
      (define bank0    (t:turn-bank turn))
      (define visibles (t:turn-cards turn))
      (define wallet0  (p:player-wallet (t:turn-active turn)))
      (f-trade-then-purchase equations visibles wallet0 bank0 which))

    (define/public (buy-cards turn)
      (define visibles (t:turn-cards turn))
      (define wallet   (p:player-wallet (t:turn-active turn)))
      (purchase-cards (f-buy-cards visibles wallet which)))))

(module+ test
  (check-true (is-a? (new strategy% [equations '()] [which purchase-points]) strategy%) "just new"))
      

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

(define (f-should-the-player-request-a-random-pebble equations wallet0 bank0)
  (and (not (b:bag-empty? bank0))
       (empty? (e:useful equations wallet0 bank0))))


;                                                                 
;       ;                                                         
;       ;           ;                                             
;       ;           ;                                             
;    ;;;;  ;;;;   ;;;;;  ;;;;           ;;;;   ;;;   ;;;;         
;   ;; ;;      ;    ;        ;          ;;  ; ;;  ;  ;; ;;        
;   ;   ;      ;    ;        ;          ;     ;   ;; ;   ;        
;   ;   ;   ;;;;    ;     ;;;;          ;     ;;;;;; ;   ;        
;   ;   ;  ;   ;    ;    ;   ;          ;     ;      ;   ;        
;   ;; ;;  ;   ;    ;    ;   ;          ;     ;      ;; ;;   ;;   
;    ;;;;   ;;;;    ;;;   ;;;;          ;      ;;;;  ;;;;    ;;   
;                                                    ;            
;                                                    ;            
;                                                    ;            

(struct exchange [trades purchase] #:transparent)

#; {type Exchange = (exchange Equation* Purchases)}
#; (exchange e* p)
;; applying the series of Equations e*, left to right, to the wallet yields as best purchases p

(define (exchange-cards ex)
  (purchase-cards (exchange-purchase ex)))

(struct purchase [cards points walletω] #:transparent
  #:methods gen:equal+hash
  [(define equal-proc
     (λ (x y recursive-equal?)
       (and
        (equal? (purchase-cards x) (purchase-cards y))
        (b:bag-equal? (purchase-walletω x) (purchase-walletω y)))))
   (define (hash-proc x re-hash)
     (+ (* 1000 (re-hash (purchase-cards x)))
        (* 10 (re-hash (purchase-cards x)))))
   (define (hash2-proc x re-hash2)
     (+ (* 891 (re-hash2 (purchase-cards x)))
        (* 999 (re-hash2 (purchase-cards x)))))])

#; {type Purchases = (purchase [Listof Card] Natural Bag)}
#; (node (list c ...) n w)
;; represents the purchase of cards `c` .. in order with `n` points for all purchases, final wallet w
;; NOTE two different orderings of `cards` are equivalent _if_ they yield the same number of points &
;; result in the same wallet

(define (purchase-size p)
  (length (purchase-cards p)))

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
  (setup-scenarios scenario+ Tests/ ForStudents/)
  
  (define equations (list rg=bbbb ggg=r ggb=rw))
  
  #;{Bag 1Eq ... -> (values Equation* Bag)}
  ;; assume all turns are created from bank0 except for strat-t1
  (define (wallet-<--->-bank0 w0 . e...)
    (let*-values ([(e1) ggg=r-]
                  [(e2) ggg=r-]
                  [(ω _)
                   (for/fold ([w w0] [bank bank0]) ([e e...])
                     (b:bag-transfer w bank (e:1eq-left e) (e:1eq-right e)))])
      (values e... ω))))

#; (b:bag-transfer wallet bank (e:1eq-left x) (e:1eq-right x))

(module+ examples ;; for students
  ;; even though trades are possible, none will yield a wallet that allows card purchases
  (let*-values ([(r) (exchange '[] (purchase '[] 0 b-4xb-3xg))])
    (scenario+ ForStudents/ (list equations strat-t1 purchase-points) r "no trades"))

  ;; the player can buy a card for 2 point w/o trading
  (let*-values ([(b) (b:bag-add b-6g-3r-4b b-r)]
                [(b) (b:bag-minus b b-ggg)]
                [(b) (b:bag-minus b (c:card-pebbles (first cards0)))]
                [(r) (exchange `(,ggg=r) (purchase `[,(first cards0)] 2 b))])
    (void))
  
  (let*-values ([(r) (exchange '() (purchase `[,(cadr cards0)] 1 (b:bag-minus b-6g-3r-4b b-ggggg)))])
    (scenario+ ForStudents/ (list equations strat-t2 purchase-size) r "cards 1"))

  ;; the player must trade once to buy a card for 2 points
  (let*-values ([(t ω) (wallet-<--->-bank0 b-6g-3r-4b ggg=r)]
                [(r) (exchange t (purchase `[,(first cards0)] 2 (b:bag-minus ω b-rrbrr)))])
    (scenario+ ForStudents/ (list equations strat-t2 purchase-points) r "points 2")))

(module+ examples ;; for testing students
  ;; the player must make 2 trades to buy a card for 1 point
  (define wallet-test (b:bag-add b-rr b-yyw))
  
  (let*-values ([(t ω) (wallet-<--->-bank0 wallet-test ggg=r- ggg=r-)]
                [(r) [exchange t (purchase `[,(third cards1)] 1 (b:bag-minus ω b-ggggg))]])
    (scenario+ Tests/ (list equations strat-t5 purchase-points) r "t 1"))

  (let*-values ([(e) (list r=bbbb r=gggg)]
                [(r) (exchange (list r=bbbb) (purchase (list c-rbbbb) 5 (b:bag)))])
    (scenario+ Tests/ (list e xben-4 purchase-points) r "ben's test"))

  (let*-values ([(e) (list r=gggg r=bbbb)]
                [(w) b-4r-2y-1w]
                [(r) (exchange (list r=bbbb r=gggg) (purchase (list c-bbbbb c-ggggg) 6 (b:bag)))])
    (scenario+ Tests/ (list e xstrat-2 purchase-points) r "2 rules, 2 cards, score 6, wallet: 0")))

(module+ examples ;; for checking tie breaking 
  (provide Extras/)

  (require (submod Bazaar/Common/pebbles examples))

  (setup-scenarios extra+ Extras/)

  ;; a player can buy 2 cards for 3 points if it makes three trades
  (let*-values ([(t w1) (wallet-<--->-bank0 (b:bag-add wallet-test b-rr) ggg=r- ggg=r- rg=bbbb)]
                [(ω) (b:bag-minus (b:bag-minus w1 b-yyrwb) b-ggggg)]
                [(r) [exchange t (purchase (rest cards1) 3 ω)]])
    (extra+ Extras/ (list equations strat-t4 purchase-points) r "t 2"))

  ;; the player must make 2 trades to buy a card for 2 points; an alterantive would yield only 1 point
  (let*-values ([(t ω) (wallet-<--->-bank0 (b:bag-add wallet-test b-r) ggg=r- rg=bbbb)]
                [(r) [exchange t (purchase `[,(second cards1)] 2 (b:bag-minus ω b-yyrwb))]])
    (extra+ Extras/ (list equations strat-t3 purchase-points) r "x 2"))

  (let*-values ([(e) (list r=gggg)]
                [(r) (exchange '[] (purchase (list c-ggggg*) 2 (b:bag-add (b:bag WHITE) b-bbbb)))])
    (extra+ Extras/ (list e xstrat-1 purchase-size) r "same # of cards, cards differ in face (2)"))
  
  (let*-values ([(e) (list ggg=r- rg=bbbb)]
                [(w) (b:bag-add b-b b-b b-b)]
                [(r) (exchange (list ggg=r- ggg=r- rg=bbbb) (purchase (list c-yyrwg* c-ggggg) 3 w))])
    (extra+ Extras/ (list e xstrat-3 purchase-points) r "3 rules, 2 cards, score 3, wallet 3b"))
  
  )

;                                                          
;      ;                               ;                   
;                          ;           ;                   
;                          ;           ;                   
;    ;;;   ;   ;   ;;;   ;;;;;         ;;;;   ;   ;  ;   ; 
;      ;   ;   ;  ;   ;    ;           ;; ;;  ;   ;  ;   ; 
;      ;   ;   ;  ;        ;           ;   ;  ;   ;   ; ;  
;      ;   ;   ;   ;;;     ;           ;   ;  ;   ;   ; ;  
;      ;   ;   ;      ;    ;           ;   ;  ;   ;   ; ;  
;      ;   ;   ;  ;   ;    ;           ;; ;;  ;   ;   ;;   
;      ;    ;;;;   ;;;     ;;;         ;;;;    ;;;;    ;   
;      ;                                               ;   
;      ;                                              ;    
;    ;;                                              ;;    

(define (f-buy-cards visibles wallet value)
  (define best
    (new collector%
         [e0    (purchase '[] 0 wallet)]
         [score value]
         [break tie-breaker-for-purchases]))
  (possible-purchases visibles wallet best)
  (send best done))

#; {[Listtof Card] Bag [Purchases-> Real] -> [Listof Purchases]}
;; EFFECT gather the best card purchases in `best`
;; imperatively accumulate all paths of cards from root to leafs, evaluate, turn into purchases
;; this may return the "no cards can be purchased" result 
(define (possible-purchases visibles0 wallet0 best)
  ;; ACCU in reverse order of possible purchaes from `visibles0` & `wallet0` to `visibles` & `wallet`

  (define (possible-purchases visibles wallet from-root-to-here points)
    (define possible-buys #; [Listof Card] (c:can-buy visibles wallet))
    (cond
      [(empty? possible-buys)
       (define e1 (purchase (reverse from-root-to-here) points wallet))
       (send best add-if-better e1)]
      [else
       (for ([t possible-buys])
         (define-values [δ visibles-- wallet-- _] (c:buy-1-card t 0 visibles wallet (b:bag)))
         (possible-purchases visibles-- wallet-- (cons t from-root-to-here) (+ δ points)))]))

  (possible-purchases visibles0 wallet0 '[] 0))

;; ---------------------------------------------------------------------------------------------------
#; {[NEListof Purchases] -> Purchases}
;; pick the pruchase that has the most poinnts, tthe largest wallet, the best wallet, or best cards
;; if all equals?, the list is passed on to the fail continuation `fk` if it exists
;; otherwise just pick the first one 
(define (tie-breaker-for-purchases lop #:selector (selector identity) #:continue (fk #false))
  (define (also-point-max lop) (all-argmax (compose purchase-points selector) lop))
  (define (wallet-size lop) (all-argmax (λ (x) (b:bag-size (purchase-walletω (selector x)))) lop))
  (define (smallest-wallet lop) (pick-smallest lop b:bag<? (compose purchase-walletω selector) fk))
  (define (best-cards lop) (pick-smallest lop c:card*<? (compose purchase-cards selector) fk))
  (tie-breaker (list also-point-max wallet-size smallest-wallet best-cards) lop #:continue fk))

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
  (check-equal? (f-buy-cards (list) (b:bag) purchase-size)
                (purchase '() 0 (b:bag)))
  (check-equal? (f-buy-cards (list c-ggggg c-ggggg) b-ggggg purchase-size)
                (purchase (list c-ggggg) 5 (b:bag)))

  (check-equal? (f-buy-cards (list c-ggggg c-ggggg) b-ggggg purchase-points)
                (purchase (list c-ggggg) 5 (b:bag)))

  (check-equal? (f-buy-cards (list c-ggggg c-ggggg) (b:bag-add b-ggggg  b-ggggg) purchase-points)
                (purchase (list c-ggggg c-ggggg) 6 (b:bag))))

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

;; determine all possible exchanges between the `wallet` and the `bank` that are feasible
;; up to `SearchDepth` and maximize at each node in this search tree
;; what the player can buy according to `value`, for now:
;; -- maximize points that player can get with a particular sequencing of card purchases
;; -- maximize the number of cards that player can get with a particular sequencing of card purchases
(define (f-trade-then-purchase equations visibles wallet0 bank0 value)
  (define (buy-with-wallet w) (f-buy-cards visibles w value))
  (define best
    (new collector%
         [e0    (exchange '() (buy-with-wallet wallet0))]
         [score (compose value exchange-purchase)]
         [break tie-breaker-trade-then-purchase]))
  (possible-trades-cards equations wallet0 bank0 visibles buy-with-wallet best)
  (send best done))

;; ---------------------------------------------------------------------------------------------------
#; {Equation* Bag Bag {Bag -> [Listof Card] Collector} -> Void}
;; EFFECT gather the best exchanges (trades plus card purchases) in `best`
(define (possible-trades-cards equations wallet0 bank0 visibles buy-with-wallet best)
  (define (possible-trades-cards/accu fuel wallet bank trades-so-far)
    ;; imperatively gather the best exchanges from root to leafs
    (define rules (e:useful equations wallet bank))
    (cond
      [(or (empty? rules) (zero? fuel)) (void)]
      [else
       (for ([r rules]) 
         (define-values (wallet++ bank++) (b:bag-transfer wallet bank (e:1eq-left r) (e:1eq-right r)))
         (define trades   (cons r trades-so-far))
         (define cards    (buy-with-wallet wallet++))
         (send best add-if-better (exchange (reverse trades) cards))
         ;; the buying does _not_ apply to the wallet or bank because once the player buys cards
         ;; it can no longer trade pebbles 
         (possible-trades-cards/accu (sub1 fuel) wallet++ bank++ trades))]))

  (possible-trades-cards/accu (SearchDepth) wallet0 bank0 '())) 

;; ---------------------------------------------------------------------------------------------------
#; {[NEListof Exchange] -> Exchange}
;; given all possible exchange paths, break ties among the embedded trades-buys in three steps
(define (tie-breaker-trade-then-purchase lo-ex-0)
  (define (smallest-number-of-trades lop) (all-argmin (λ (ex) (length (exchange-trades ex))) lop))
  (define (smallest-trade lop) (pick-smallest lop e:equations<? exchange-trades #false))
  (let* ([step lo-ex-0]
         [step #; 1 (tie-breaker (list smallest-number-of-trades) step #:continue 'yes)]
         [step #; 2 (tie-breaker-for-purchases step #:selector exchange-purchase #:continue 'yes)]
         [step #; 3 (tie-breaker (list smallest-trade) step #:continue #false)])
    step))

(module+ test
  (define c
    (new collector%
         [e0 (exchange '() (f-buy-cards '[] (b:bag) purchase-points))]
         [score (compose purchase-points exchange-purchase)]
         [break tie-breaker-trade-then-purchase]))

  (require (only-in (submod Bazaar/Common/equations examples) www=yy ww=yyy))

  (define ex1 (exchange `[,www=yy ,ww=yyy] (purchase `[,c-yyyyy*] 8 (b:bag))))
  (define ex2 (exchange `[,ww=yyy ,www=yy] (purchase `[,c-yyyyy*] 8 (b:bag))))

  '----------------------
  (send c add-if-better ex1)
  (send c add-if-better ex2)
  (check-equal? (send c done) ex2)
  (check-false (equal? ex1 ex2))
  '----------------------)



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
  (check-true (f-should-the-player-request-a-random-pebble '[] b-4xb-3xg b-rg) "trades possible")
  
  #; {Symbol Trade&BuyScenarios -> Void}
  (define (run-scenario* t scenario*)
    (eprintf "--------------- ~a\n" t)
    (define count 0)
    (for ([s scenario*] [i (in-naturals)])
      (set! count (+ count 1))
      (match-define (list args expected msg) s)
      (match-define (list equations turn policy) args)
      (define bank   (t:turn-bank turn))
      (define cards  (t:turn-cards turn))
      (define wallet (p:player-wallet (t:turn-active turn)))
      #;
      (show expected equations cards wallet bank policy msg)

      (f-trade-then-purchase equations cards wallet bank policy)

      (check-equal? (f-trade-then-purchase equations cards wallet bank policy) expected msg))
    (eprintf "~a tests completed\n" count))

  #;{Exchange Equation* [Setof Card] Bag Bag Policy String -> Void}
  (define (show expected equations cards wallet bank policy msg)
    (define p-equations (e:render* equations))
    (define p-cards     (c:render* cards))
    (define p-wallet    (frame (b:render wallet)))
    (define p-bank      (frame (b:render bank)))
    (eprintf "---- ~a with policy ~a\n" msg policy)
    (pretty-print (frame (inset (hb-append 10 p-equations p-cards p-wallet p-bank) 2)))
    (pretty-print expected)))


(module+ test
  (run-scenario* 'ForStudents ForStudents/)
  (run-scenario* 'Tests Tests/)
  (run-scenario* 'Extras Extras/))

(module+ stress ;; test

  (require (submod ".." json))
  (require (submod Bazaar/Common/equations json))
  (require (submod Bazaar/Common/turn-state json))
  (require json)

  (define ee (e:random-equation*))
  (define tt (t:random-turn))
  (define cc (t:turn-cards tt))
  (define ww (p:player-wallet (t:turn-active tt)))
  (define bb (t:turn-bank tt))
  (define po purchase-points)

  (define j-ee (equations->jsexpr ee))
  (define j-tt (turn->jsexpr tt))
  (define j-po (policy->jsexpr po))
  
  (time (f-trade-then-purchase ee cc ww bb po)))

;                              
;      ;                       
;                              
;                              
;    ;;;    ;;;    ;;;   ; ;;  
;      ;   ;   ;  ;; ;;  ;;  ; 
;      ;   ;      ;   ;  ;   ; 
;      ;    ;;;   ;   ;  ;   ; 
;      ;       ;  ;   ;  ;   ; 
;      ;   ;   ;  ;; ;;  ;   ; 
;      ;    ;;;    ;;;   ;   ; 
;      ;                       
;      ;                       
;    ;;                        

(module+ json
  (define PCards (~a (object-name purchase-size)))
  (define PPoints (~a (object-name purchase-points)))
  
  (define (policy->jsexpr p)
    (cond
      [(equal? p purchase-size) PCards]
      [(equal? p purchase-points) PPoints]
      [else (error 'policy->jsexpr "policy expected, given ~a" p)]))

  (define (jsexpr->policy p)
    (cond
      [(equal? p PCards) purchase-size]
      [(equal? p PPoints) purchase-points]
      [else #false])))

(module+ test
  (check-equal? (jsexpr->policy (policy->jsexpr purchase-size)) purchase-size)
  (check-equal? (jsexpr->policy (policy->jsexpr purchase-points)) purchase-points))
