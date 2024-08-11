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
  (define equations (list r-g=4xb 3xg=r ggb=rw)))

(module+ examples ;; for students 
  ;; even though trades are possible, none will yield a wallet that allows card purchases
  (define ns        (null-exchange))
  (define cards-for (list c-rrbrr* c-ggggg))
  (check-true (should-the-player-request-a-random-pebble equations b-4xb-3xg b-rg) "trades possible")
  (scenario+ ForStudents/ (list equations cards-for b-4xb-3xg b-rg purchase-points) ns "no trades")

  ;; the player can buy a card for 1 point w/o trading 
  (define wal (b:bag-add b-rg b-rg b-rg b-4xb-3xg))
  (define ban (b:bag-add b-bbbbb b-ggggg b-rrbrr b-rg b-rg))
  (define res-1 [exchange '() (purchase (list c-ggggg) 1)])
  (scenario+ ForStudents/ (list equations cards-for wal ban purchase-size) res-1 "cards 1")

  ;; the player must trade once to buy a card for 2 points 
  (define res-2 [exchange `(,3xg=r) (purchase (list c-rrbrr*) 2)])
  (scenario+ ForStudents/ (list equations cards-for wal ban purchase-points) res-2 "points 2"))

(module+ examples ;; for testing students
  ;; the player must make 2 trades to buy a card for 1 point 
  (define cards-test (list c-rbbbb c-yyrwg* c-ggggg))
  (define wal-test   (b:bag-add b-rr b-yyw))
  (define res-test   [exchange (list 3xg=r- 3xg=r-) (purchase (list c-ggggg) 1)])
  (scenario+ Tests/ (list equations cards-test wal-test ban purchase-points) res-test "t 1")

  ;; the player must make 2 trades to buy a card for 2 points; an alterantive would yield only 1 point
  (define wal-test2  (b:bag-add wal-test b-r))
  (define res-test2  [exchange (list 3xg=r- r-g=4xb) (purchase (list c-yyrwg*) 2)])
  (scenario+ Tests/ (list equations cards-test wal-test2 ban purchase-points) res-test2 "x 2")

  ;; a player can buy 2 cards for 3 points if it makes three trades 
  (define cards-test3 (list c-rbbbb* c-yyrwg* c-ggggg))
  (define wal-test3  (b:bag-add wal-test b-rr))
  (define res-test3  [exchange (list 3xg=r- r-g=4xb 3xg=r-) (purchase (list c-yyrwg* c-ggggg) 3)])
  (scenario+ Tests/ (list equations cards-test3 wal-test3 ban purchase-points) res-test3 "t 2"))

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
    [else (tie-breaker-trade-then-purchase wallet0 pts which)]))

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
(define (tie-breaker-trade-then-purchase wallet0 pts which)
  (define the-bests (single pts best-value which))
  (define shortest  (single the-bests smallest-number-of-trades))
  (define richest   (single shortest #:upgrade (λ (x) `[[0 ,x]]) most-pebbles-left wallet0))
  (single richest #:upgrade second pick-smallest-according-to-bag<))

#; {[Listof Exchange] -> [Listof Exchange]}
(define (best-value pts which)
  (all-argmax (compose which exchange-purchase) pts))

;; ---------------------------------------------------------------------------------------------------
#; {[Listof Exchange] -> [Listof Exchange]}
(define (smallest-number-of-trades the-bests)
  (all-argmin exchange-trade# the-bests))

;; ---------------------------------------------------------------------------------------------------
#; {[Listof Exchange] -> [Listof [List Natural Exchange]]}
(define (most-pebbles-left exchanges wallet)
  (define exchanges-with-pebbles-left (map (pebbles-left wallet) exchanges))
  (all-argmax (compose b:bag-size first) exchanges-with-pebbles-left))

#; {Bag -> Exchange -> [List Bag Exchange]}
(define ((pebbles-left wallet0) ex)
  (match-define [exchange trade purchases] ex)
  (for/fold ([wallet wallet0] #:result (list wallet ex)) ([t trade])
    (define-values (wallet++ _) (b:bag-transfer wallet (b:bag) (e:1eq-left t) (e:1eq-right t)))
    wallet++))

;; ---------------------------------------------------------------------------------------------------
#; {[Listof [List Natural Exchange]] -> Exchange}
(define (pick-smallest-according-to-bag< richest)
  (second (first (sort richest b:bag< #:key first))))

;; ---------------------------------------------------------------------------------------------------
(define (single id f  #:upgrade (u list) . e)
  (if (empty? (rest id)) (u (first id)) (apply f id e)))
  
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
                                 
(struct purchase [cards points] #:prefab)
#; {type Purchases = (purchase [Listof Card] Natural)}
#; (node c* n) ; represent the purchase of cards `c` with `n` points for all purchases

(define (purchase-size p)
  (length (purchase-cards p)))

(define null-purchases (purchase '() 0))

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
    [(empty? (rest just-cards)) (purchase (first just-cards) first-best)]
    [else 
     (define sorted (tie-breaker-for-purchases just-cards))
     (purchase (first sorted) first-best)]))

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

#; {[Setof Card] Bag [] -> Purchases}
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
       (set! *possibles (cons (purchase proper-order points) *possibles))]
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

#; {[NEListof [Listof Card]] -> [Listof [List Card Card N]]}
;; pick the list of cards that is best according to some whimsical ordering of card sequences
(define (tie-breaker-for-purchases all-best)
  (sort all-best c:cards<))

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
                (purchase '() 0))
  (check-equal? (buy-cards (list c-ggggg c-ggggg) b-ggggg purchase-size)
                (purchase (list c-ggggg) 1))
  
  (check-equal? (possible-purchases (list c-ggggg c-ggggg) b-ggggg)
                (list (purchase (list c-ggggg) 5) (purchase (list c-ggggg) 5)))

  (check-equal? (buy-cards (list c-ggggg c-ggggg) b-ggggg purchase-points)
                (purchase (list c-ggggg) 5))

  (check-equal? (buy-cards (list c-ggggg c-ggggg) (b:bag-add b-ggggg  b-ggggg) purchase-points)
                (purchase (list c-ggggg c-ggggg) 6)))
