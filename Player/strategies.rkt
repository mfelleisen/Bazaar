#lang racket

;; two strategies for a Bazaar player for choosing
;;  (1) whether to request a random pebble from the bank or a series of exchanges
;;  (2) whether to buy cards and which one 

;; ---------------------------------------------------------------------------------------------------

(provide
 #; {Equation* Bag Bag -> Boolean}
 should-the-player-request-a-random-pebble

 #; {Equation* [Setof Card] Bag Bag {Purchase -> Natural} -> Exchange}
 trade-then-purchase

 #; {[Setof Card] Bag [Purchase -> Natural] -> Purchases}
 ;; the player wishes to purchase the cards in the specified list order 
 buy-cards

 #; {type Exchange}
 exchange?
 exchange-cards
 exchange-trades

 #; {type Purchase}
 purchase?
 purchase-cards

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

(require (submod Bazaar/Common/bags examples))
(require (submod Bazaar/Common/cards examples))
(require (submod Bazaar/Common/equations examples))

(require SwDev/Lib/should-be-racket)

(module+ examples
  (require (submod ".."))
  (require SwDev/Testing/scenarios)
  (require rackunit))

(module+ test
  (require (submod ".." examples))
  (require (submod ".." json))
  (require rackunit))

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

(define (should-the-player-request-a-random-pebble equations wallet0 bank0)
  (empty? (e:useful equations wallet0 bank0)))

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

(struct exchange [trades purchase] #:prefab)
#; {type Exchange = (exchange Equation* Purchases)}
#; (exchange e* p)
;; applying the series of Equations e*, left to right, to the wallet yields as best purchases p

;; the number of trades per exchane 
(define (exchange-trade# ex)
  (length (exchange-trades ex)))

(define (exchange-cards ex)
  (purchase-cards (exchange-purchase ex)))

;; the numerical value of an exchange 
(define (exchange-value ex)
  (purchase-points (exchange-purchase ex)))

(define (null-exchange) (exchange '() null-purchases))

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
  (setup-scenarios scenario+ Tests/ ForStudents/ Extras/)
  
  (define equations (list r-g=4xb 3xg=r ggb=rw))
  (define cards0    (list c-rrbrr* c-ggggg))
  (define bank0     (b:bag-add b-bbbbb b-ggggg b-rrbrr b-rg b-rg))

  #;{Bag 1Eq ... -> (values Equation* Bag)}
  (define (create-wallet-from-transfers w0 . e...)
    (let*-values ([(e1) 3xg=r-]
                  [(e2) 3xg=r-]
                  [(ω _)
                   (for/fold ([w w0] [bank bank0]) ([e e...])
                     (b:bag-transfer w bank (e:1eq-left e) (e:1eq-right e)))])
      (values e... ω))))

#; (b:bag-transfer wallet bank (e:1eq-left x) (e:1eq-right x))

(module+ examples ;; for students
  ;; even though trades are possible, none will yield a wallet that allows card purchases
  (define wallet0 (b:bag-add b-rg b-rg b-rg b-4xb-3xg))
  
  (let*-values ([(r) (exchange '[] (purchase '[] 0 b-4xb-3xg))])
    (scenario+ ForStudents/ (list equations cards0 b-4xb-3xg b-rg purchase-points) r "no trades"))

  ;; the player can buy a card for 1 point w/o trading
  (let*-values ([(r) (exchange '() (purchase (list c-ggggg) 1 (b:bag-minus wallet0 b-ggggg)))])
    (scenario+ ForStudents/ (list equations cards0 wallet0 bank0 purchase-size) r "cards 1"))

  ;; the player must trade once to buy a card for 2 points
  (let*-values ([(t w) (create-wallet-from-transfers wallet0 3xg=r)]
                [(r) (exchange t (purchase (list c-rrbrr*) 2 (b:bag-minus w b-rrbrr)))])
    (scenario+ ForStudents/ (list equations cards0 wallet0 bank0 purchase-points) r "points 2")))

(module+ examples ;; for testing students
  ;; the player must make 2 trades to buy a card for 1 point
  (define cards-test  (list c-rbbbb c-yyrwg* c-ggggg))
  (define wallet-test (b:bag-add b-rr b-yyw))
 
  (let*-values ([(w0) wallet-test]
                [(t ω) (create-wallet-from-transfers w0 3xg=r- 3xg=r-)]
                [(r) [exchange t (purchase (list c-ggggg) 1 (b:bag-minus ω b-ggggg))]])
    (scenario+ Tests/ (list equations cards-test w0 bank0 purchase-points) r "t 1"))

  ;; the player must make 2 trades to buy a card for 2 points; an alterantive would yield only 1 point
  (let*-values ([(w0) (b:bag-add wallet-test b-r)]
                [(t ω) (create-wallet-from-transfers w0 3xg=r- r-g=4xb)]
                [(r) [exchange t (purchase (list c-yyrwg*) 2 (b:bag-minus ω b-yyrwg))]])
    (scenario+ Tests/ (list equations cards-test w0 bank0 purchase-points) r "x 2"))

  ;; a player can buy 2 cards for 3 points if it makes three trades
  (let*-values ([(w0) (b:bag-add wallet-test b-rr)]
                [(t w1) (create-wallet-from-transfers w0  3xg=r- r-g=4xb 3xg=r-)]
                [(ω) (b:bag-minus (b:bag-minus w1 b-yyrwg) b-ggggg)]
                [(r)  [exchange t (purchase (list c-yyrwg* c-ggggg) 3 ω)]])
    (scenario+ Tests/ (list equations cards-test w0 bank0 purchase-points) r "t 2")))

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

(define (trade-then-purchase equations visibles wallet0 bank0 which)
  (define pts (possible-trades equations wallet0 bank0 (λ (w) (buy-cards visibles w which))))
  (cond
    [(empty? pts) '()]
    [else (tie-breaker-trade-then-purchase pts which)]))

#; {Equation* Bag Bag {Bag -> Purchases} -> [Setof Exchange]}

;; determine all possible exchanges between the `wallet` and the `bank` that are feasible
;; up to `SearchDepth` of a generative tree and maximize at each node in this search tree
;; what the player buys according to `buy-with-wallet`, for now:
;; -- maximize points that player can get with a particular sequencing of card purchases
;; -- maximize the number of cards that player can get with a particular sequencing of card purchases

(define (possible-trades equations wallet0 bank0 buy-with-wallet)
  #; [Listof [Listof Exchange]]
  (define *possibles `[,(exchange '() (buy-with-wallet wallet0))])
  ;; imperatively accumulate all from root to leafs
  
  (let p-t/accu ([wallet wallet0] [bank bank0] [trades-so-far '()] [fuel (SearchDepth)])
    (define rules (e:useful equations wallet bank))
    (cond
      [(or (empty? rules) (zero? fuel)) (void)]
      [else
       (for ([x rules])
         (define-values (wallet++ bank++) (b:bag-transfer wallet bank (e:1eq-left x) (e:1eq-right x)))
         (define trades   (cons x trades-so-far))
         (define xchange  (exchange (reverse trades) (buy-with-wallet wallet++)))
         (set! *possibles (cons xchange *possibles))
         ;; the buying does _not_ apply to the wallet or bank because once the player buys cards
         ;; it can no longer trade pebbles 
         (p-t/accu wallet++ bank++ trades (sub1 fuel)))]))

  *possibles)

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

#; {[Listof Exchange] -> Exchange}
;; given all possible exchange paths, break ties among the embedded trades-buys as follows:
;; -- get the best node from each path
;; -- from those pick the ones with the smallest number of trades
;; -- from those pick the ones that leave the player with the most pebbles
;; -- finally pick the one with the smallest wallet according to bag< 
(define (tie-breaker-trade-then-purchase pts which)
  (define the-bests (single pts best-value which))
  (define shortest  (single the-bests smallest-number-of-trades))
  (define richest   (single shortest most-pebbles-left))
  (single richest #:upgrade identity pick-smallest-according-to-bag<))

#; {[Listof Exchange] -> [Listof Exchange]}
(define (best-value pts which)
  (all-argmax (compose which exchange-purchase) pts))

;; ---------------------------------------------------------------------------------------------------
#; {[Listof Exchange] -> [Listof Exchange]}
(define (smallest-number-of-trades the-bests)
  (all-argmin exchange-trade# the-bests))

;; ---------------------------------------------------------------------------------------------------
#; {[Listof Exchange] -> [Listof Exchange]}
(define (most-pebbles-left exchanges)
  (all-argmax (compose b:bag-size purchase-walletω exchange-purchase) exchanges))

;; ---------------------------------------------------------------------------------------------------
#; {[Listof Exchange] -> Exchange}
(define (pick-smallest-according-to-bag< richest)
  (first (sort richest b:bag< #:key (compose purchase-walletω exchange-purchase))))

;; ---------------------------------------------------------------------------------------------------
(define (single id f  #:upgrade (u list) . e)
  (if (empty? (rest id)) (u (first id)) (apply f id e)))

(module+ test ;; Ben's example concerning tie breaking 
  (define w `[,BLUE ,BLUE ,BLUE])
  (define e1 (exchange (list 3xg=r- r-g=4xb 3xg=r-) (purchase (list c-yyrwg* c-ggggg) 3 w)))
  (define e2 (exchange (list 3xg=r- 3xg=r- r-g=4xb) (purchase (list c-yyrwg* c-ggggg) 3 w)))
  (define selector (compose purchase-walletω exchange-purchase))
  (check-true (b:bag< (selector e1) (selector e2))))
  
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
  (check-true (should-the-player-request-a-random-pebble '[] b-4xb-3xg b-rg) "trades possible")
  
  #; {Symbol Trade&BuyScenarios {#:check [Equality Thunk Any String -> Void]} -> Void}
  (define (run-scenario* t scenario* #:check (C (λ (equal? act exp m) (check equal? [act] exp m))))
    (eprintf "--------------- ~a\n" t)
    (define count 0)
    (for ([s scenario*] [i (in-naturals)])
      (set! count (+ count 1))
      (match-define (list args expected msg) s)
      (match-define (list equations cards wallet bank policy) args)
      (check-equal? (trade-then-purchase equations cards wallet bank policy) expected msg))
    (eprintf "~a tests completed\n" count))
  
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
                                 
(struct purchase [cards points walletω] #:prefab)
#; {type Purchases = (purchase [Listof Card] Natural Bag)}
#; (node (list c ...) n w)
;; represents the purchase of cards `c` .. in order with `n` points for all purchases, final wallet w

(define (purchase-size p)
  (length (purchase-cards p)))

(define null-purchases (purchase '() 0 (b:bag)))

(module+ json
  (define PS (~a (object-name purchase-size)))
  (define PP (~a (object-name purchase-points)))
  
  (define (policy->jsexpr p)
    (cond
      [(equal? p purchase-size) PS]
      [(equal? p purchase-points) PP]
      [else (error 'policy->jsexpr "policy expected, given ~a" p)]))

  (define (jsexpr->policy p)
    (cond
      [(equal? p PS) purchase-size]
      [(equal? p PP) purchase-points]
      [else #false])))

(module+ test
  (check-equal? (jsexpr->policy (policy->jsexpr purchase-size)) purchase-size)
  (check-equal? (jsexpr->policy (policy->jsexpr purchase-points)) purchase-points))


;                                                                        
;                                                                        
;                                               ;                        
;                                               ;                        
;   ;;;;   ;;;;    ;;;;  ;;;;  ;;;;;;   ;;;   ;;;;;   ;;;    ;;;;   ;;;  
;   ;; ;;      ;   ;;  ;     ; ;  ;  ; ;;  ;    ;    ;;  ;   ;;  ; ;   ; 
;   ;   ;      ;   ;         ; ;  ;  ; ;   ;;   ;    ;   ;;  ;     ;     
;   ;   ;   ;;;;   ;      ;;;; ;  ;  ; ;;;;;;   ;    ;;;;;;  ;      ;;;  
;   ;   ;  ;   ;   ;     ;   ; ;  ;  ; ;        ;    ;       ;         ; 
;   ;; ;;  ;   ;   ;     ;   ; ;  ;  ; ;        ;    ;       ;     ;   ; 
;   ;;;;    ;;;;   ;      ;;;; ;  ;  ;  ;;;;    ;;;   ;;;;   ;      ;;;  
;   ;                                                                    
;   ;                                                                    
;   ;                                                                    

#; {[Purchase -> Natural] -> [NEListof Purchase]  -> Purchase}
(define ((pick-most f) possible)
  (define first-best (f (argmax f possible)))
  (define all-best   (filter (λ (p*) (= (f p*) first-best)) possible))
  (define just-cards (map purchase-cards all-best))
  (cond
    [(empty? (rest all-best)) (first all-best)]
    [else (tie-breaker-for-purchases all-best)]))

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

(define (buy-cards visibles wallet which)
  #; {[Listof Purchases]}
  (define possible (possible-purchases visibles wallet))
  (cond
    [(empty? possible) null-purchases]
    [else ((pick-most which) possible)]))

#; {[Setof Card] Bag [] -> [Listof Purchases]}
;; imperatively accumulate all paths of cards from root to leafs, evaluate, turn into purchases
(define (possible-purchases visibles0 wallet0)
  #; {Listof Purchases}
  (define *possibles '[])
  
  ;; ACCU in reverse order of possible purchaes from `visibles0` & `wallet0` to `visibles` & `wallet`
  (let p-p/accu ([visibles visibles0] [wallet wallet0] [from-root-to-here '()] [points 0])
    #; [Listof Card]
    (define possible-buys (c:can-buy visibles wallet))
    (cond
      [(empty? possible-buys)
       (define proper-order (reverse from-root-to-here))
       (set! *possibles (cons (purchase proper-order points wallet) *possibles))]
      [else
       (for ([t possible-buys])
         (define-values [visibles-- wallet-- more] (purchase-1-card t visibles wallet))
         (p-p/accu visibles-- wallet-- (cons t from-root-to-here) (+ more points)))]))
  
  *possibles)

#; {Card [Setof Card] Bag -> [Setof Card] Bag Natural}
;;  ASSUME `c` is in `visibles`
(define (purchase-1-card c visibles wallet)
  (define visibles--  (remove c visibles))
  (define wallet--    (b:bag-minus wallet (c:card-pebbles c)))
  (define points      (c:calculate-points c (b:bag-size wallet--)))
  (values visibles-- wallet-- points))
  


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

#; {[NEListof Purchases] -> Purchases}
;; pick the list of cards that is best according to some whimsical ordering of card sequences
(define (tie-breaker-for-purchases all-best)
  (first (sort all-best c:cards< #:key purchase-cards)))

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
  (check-equal? (buy-cards (list) (b:bag) purchase-size)
                (purchase '() 0 (b:bag)))
  (check-equal? (buy-cards (list c-ggggg c-ggggg) b-ggggg purchase-size)
                (purchase (list c-ggggg) 5 (b:bag)))
  
  (check-equal? (possible-purchases (list c-ggggg c-ggggg) b-ggggg)
                (list (purchase (list c-ggggg) 5 (b:bag)) (purchase (list c-ggggg) 5 (b:bag))))

  (check-equal? (buy-cards (list c-ggggg c-ggggg) b-ggggg purchase-points)
                (purchase (list c-ggggg) 5 (b:bag)))

  (check-equal? (buy-cards (list c-ggggg c-ggggg) (b:bag-add b-ggggg  b-ggggg) purchase-points)
                (purchase (list c-ggggg c-ggggg) 6 (b:bag))))
