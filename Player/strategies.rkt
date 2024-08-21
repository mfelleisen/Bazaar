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
 exchange-purchase

 #; {type Purchase}
 purchase?
 purchase-cards
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

(require (submod Bazaar/Common/bags examples))
(require (submod Bazaar/Common/cards examples))
(require (submod Bazaar/Common/equations examples))

(require SwDev/Lib/should-be-racket)
(require pict)

(module+ examples
  (require (submod ".."))
  (require SwDev/Testing/scenarios))

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
  (and (not (b:bag-empty? bank0))
       (empty? (e:useful equations wallet0 bank0))))

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

(struct exchange [trades purchase]
  #:transparent
  #:methods gen:equal+hash
  [(define equal-proc
     (λ (x y recursive-equal?)
       (and
        (b:bag-equal? (apply b:bag (exchange-trades x)) (apply b:bag (exchange-trades y)))
        (equal? (exchange-purchase x) (exchange-purchase y)))))
   (define (hash-proc x re-hash)
     (+ (* 1000 (re-hash (exchange-purchase x)))
        (* 10 (re-hash (exchange-purchase x)))))
   (define (hash2-proc x re-hash2)
     (+ (* 891 (re-hash2 (exchange-purchase x)))
        (* 999 (re-hash2 (exchange-purchase x)))))])
  
#; {type Exchange = (exchange Equation* Purchases)}
#; (exchange e* p)
;; applying the series of Equations e*, left to right, to the wallet yields as best purchases p

(define (exchange-cards ex)
  (purchase-cards (exchange-purchase ex)))


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
  
  (define equations (list rg=bbbb ggg=r ggb=rw))
  (define cards0    (list c-rrbrr* c-ggggg))
  (define bank0     (b:bag-add b-bbbbb b-ggggg b-rrbrr b-rg b-rg))

  #;{Bag 1Eq ... -> (values Equation* Bag)}
  (define (create-wallet-from-transfers w0 . e...)
    (let*-values ([(e1) ggg=r-]
                  [(e2) ggg=r-]
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
  (let*-values ([(t w) (create-wallet-from-transfers wallet0 ggg=r)]
                [(r) (exchange t (purchase (list c-rrbrr*) 2 (b:bag-minus w b-rrbrr)))])
    (scenario+ ForStudents/ (list equations cards0 wallet0 bank0 purchase-points) r "points 2")))

(module+ examples ;; for testing students
  ;; the player must make 2 trades to buy a card for 1 point
  (define cards-test  (list c-rbbbb c-yyrwg* c-ggggg))
  (define wallet-test (b:bag-add b-rr b-yyw))
 
  (let*-values ([(w0) wallet-test]
                [(t ω) (create-wallet-from-transfers w0 ggg=r- ggg=r-)]
                [(r) [exchange t (purchase (list c-ggggg) 1 (b:bag-minus ω b-ggggg))]])
    (scenario+ Tests/ (list equations cards-test w0 bank0 purchase-points) r "t 1"))

  ;; the player must make 2 trades to buy a card for 2 points; an alterantive would yield only 1 point
  (let*-values ([(w0) (b:bag-add wallet-test b-r)]
                [(t ω) (create-wallet-from-transfers w0 ggg=r- rg=bbbb)]
                [(r) [exchange t (purchase (list c-yyrwg*) 2 (b:bag-minus ω b-yyrwb))]])
    (scenario+ Tests/ (list equations cards-test w0 bank0 purchase-points) r "x 2"))

  ;; a player can buy 2 cards for 3 points if it makes three trades
  (let*-values ([(w0) (b:bag-add wallet-test b-rr)]
                [(t w1) (create-wallet-from-transfers w0  ggg=r- rg=bbbb ggg=r-)]
                [(ω) (b:bag-minus (b:bag-minus w1 b-yyrwb) b-ggggg)]
                [(r)  [exchange t (purchase (list c-yyrwg* c-ggggg) 3 ω)]])
    (scenario+ Tests/ (list equations cards-test w0 bank0 purchase-points) r "t 2")))

(module+ examples ;; for checking tie breaking 
  (provide Extras/)

  (let*-values ([(e) (list r=gggg r=bbbb)]
                [(c) (list c-bbbbb c-ggggg)]
                [(w) (b:bag-add (b:bag-add b-rr b-b) b-g)]
                [(b) (b:bag-add b-ggggg b-rbbbb)])
    (scenario+ Extras/ (list e c w b purchase-points) 1 "2 rules, 2 cards, score 6, wallet: 0"))

  (let*-values ([(e) (list ggg=r- rg=bbbb)]
                [(c) (list c-yyrwg* c-ggggg)]
                [(w) (b:bag-add wallet-test b-rr)])
    (define e1 (exchange (list ggg=r- rg=bbbb ggg=r-) (purchase (list c-yyrwg* c-ggggg) 3 w)))
    (define e2 (exchange (list ggg=r- ggg=r- rg=bbbb) (purchase (list c-yyrwg* c-ggggg) 3 w)))
    (scenario+ Extras/ (list e c w bank0 purchase-points) 1 "3 rules, 2 cards, score 3, wallet 3b")))

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
  (match (possible-trades equations wallet0 bank0 visibles which)
    [(list ex) ex]
    [possibles (tie-breaker-trade-then-purchase possibles)]))

;; ---------------------------------------------------------------------------------------------------
#; {Equation* Bag Bag {Purchases -> Real} -> [NESetof Exchange]}

;; determine all possible exchanges between the `wallet` and the `bank` that are feasible
;; up to `SearchDepth` of a generative tree and maximize at each node in this search tree
;; what the player buys according to `which`, for now:
;; -- maximize points that player can get with a particular sequencing of card purchases
;; -- maximize the number of cards that player can get with a particular sequencing of card purchases

(define (possible-trades equations wallet0 bank0 visibles which)
  (define buy-with-wallet (λ (w) (buy-cards visibles w which)))
  #; [Listof [Listof Exchange]]
  (define ex0 (exchange '() (buy-with-wallet wallet0)))
  (define best-which-es (new collector% [e0 ex0] [score (compose which exchange-purchase)]))
  ;; imperatively the best exchanges from root to leafs
  
  (let p-t/accu ([wallet wallet0] [bank bank0] [trades-so-far '()] [fuel (SearchDepth)])
    (define rules (e:useful equations wallet bank))
    (cond
      [(or (empty? rules) (zero? fuel)) (void)]
      [else
       (for ([x rules])
         (define-values (wallet++ bank++) (b:bag-transfer wallet bank (e:1eq-left x) (e:1eq-right x)))
         (define trades   (cons x trades-so-far))
         (define cards    (buy-with-wallet wallet++))
         (send best-which-es add-if-better (exchange (reverse trades) cards))
         ;; the buying does _not_ apply to the wallet or bank because once the player buys cards
         ;; it can no longer trade pebbles 
         (p-t/accu wallet++ bank++ trades (sub1 fuel)))]))

  (send best-which-es done))

;; ---------------------------------------------------------------------------------------------------
#; {[NEListof Exchange] -> Exchange}
;; given all possible exchange paths, break ties among the embedded trades-buys as follows:
;; -- from those pick the ones with the smallest number of trades
;; -- from those pick the ones that leave the player with the most pebbles

(define (tie-breaker-trade-then-purchase possibles0)
  (define f* (list smallest-number-of-trades most-pebbles-left))
  (tie-breaker f* possibles0))

#; {[Listof Exchange] -> [Listof Exchange]}
(define (smallest-number-of-trades the-bests)
  (all-argmin (λ (ex) (length (exchange-trades ex))) the-bests))

#; {[Listof Exchange] -> [Listof Exchange]}
(define (most-pebbles-left exchanges)
  (all-argmax (compose b:bag-size purchase-walletω exchange-purchase) exchanges))

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
  
  #; {Symbol Trade&BuyScenarios -> Void}
  (define (run-scenario* t scenario*)
    (eprintf "--------------- ~a\n" t)
    (define count 0)
    (for ([s scenario*] [i (in-naturals)])
      (set! count (+ count 1))
      (match-define (list args expected msg) s)
      (match-define (list equations cards wallet bank policy) args)
      #;
      (show expected equations cards wallet bank policy msg)
      (check-equal? (trade-then-purchase equations cards wallet bank policy) expected msg))
    (eprintf "~a tests completed\n" count))

  #;{Exchange Equation* [Setof Card] Bag Bag Policy String -> Void}
  (define (show expected equations cards wallet bank policy msg)
    (define p-equations (e:render* equations))
    (define p-cards     (c:render* cards))
    (define p-wallet    (frame (b:render wallet)))
    (define p-bank      (frame (b:render bank)))
    (eprintf "---- ~a with policy ~a\n" msg policy)
    (pretty-print (frame (inset (hb-append 10 p-equations p-cards p-wallet p-bank) 2)))
    (pretty-print expected))

  #; {Symbol Trade&BuyScenarios -> Void}
  (define (run-ties* t scenario*)
    (eprintf "--------------- ~a\n" t)
    (define count 0)
    (for ([s scenario*] [i (in-naturals)])
      (set! count (+ count 1))
      (match-define (list args expected msg) s)
      (match-define (list equations cards wallet bank policy) args)
      (check-equal? (let* ([s (possible-trades equations wallet bank cards policy)]
                           [s (tie-breaker-trade-then-purchase s)]
                           [s (if (exchange? s) 1 (length s))])
                      s)
                    expected
                    msg))
    (eprintf "~a tests completed\n" count))
  
  (run-scenario* 'ForStudents ForStudents/)
  (run-scenario* 'Tests Tests/)
  
  (run-ties* 'Extras Extras/))


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
                                 
(struct purchase [cards points walletω] #:transparent
  #:methods gen:equal+hash
  [(define equal-proc
     (λ (x y recursive-equal?)
       (and
        (b:bag-equal? (apply b:bag (purchase-cards x)) (apply b:bag (purchase-cards y)))
        (= (purchase-points x) (purchase-points y))
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

(module+ test
  (check-equal? (purchase `[,c-ggggg ,c-yyrwg] 3 b-bbbb) (purchase `[,c-yyrwg ,c-ggggg] 3 b-bbbb)))

(define (purchase-size p)
  (length (purchase-cards p)))

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
  (match (possible-purchases visibles wallet which)
    [(list p) p]
    [possibles (tie-breaker-for-purchases possibles)]))

#; {[Setof Card] Bag [Purchases-> Real] -> [Listof Purchases]}
;; imperatively accumulate all paths of cards from root to leafs, evaluate, turn into purchases
;; this may return the "no cards can be purchased" result 
(define (possible-purchases visibles0 wallet0 which)
  (define max-ish (new collector% [e0 (purchase '[] 0 wallet0)] [score which]))
  
  ;; ACCU in reverse order of possible purchaes from `visibles0` & `wallet0` to `visibles` & `wallet`
  (let p-p/accu ([visibles visibles0] [wallet wallet0] [from-root-to-here '()] [points 0])
    #; [Listof Card]
    (define possible-buys (c:can-buy visibles wallet))
    (cond
      [(empty? possible-buys)
       (define e1 (purchase (reverse from-root-to-here) points wallet))
       (send max-ish add-if-better e1)]
      [else
       (for ([t possible-buys])
         (define-values [visibles-- wallet-- more] (purchase-1-card t visibles wallet))
         (p-p/accu visibles-- wallet-- (cons t from-root-to-here) (+ more points)))]))

  (send max-ish done))

;; ---------------------------------------------------------------------------------------------------
#; {Card [Setof Card] Bag -> [Setof Card] Bag Natural}
;;  ASSUME `c` is in `visibles`
(define (purchase-1-card c visibles wallet)
  (define visibles--  (remove c visibles))
  (define wallet--    (b:bag-minus wallet (c:card-pebbles c)))
  (define points      (c:calculate-points c (b:bag-size wallet--)))
  (values visibles-- wallet-- points))

;; ---------------------------------------------------------------------------------------------------
#; {[NEListof Purchases] -> Purchases}
;; pick the list of cards that is best according to some whimsical ordering of card sequences
(define (tie-breaker-for-purchases lop)
  (tie-breaker (list #;wallet-index) lop))

#; {Purchase -> Natural}
;; would compute some polynomial over the colors in a player's wallet 
(define (wallet-index p)
  (all-argmin (λ (x) (for/sum ([w p]) (values (purchase-walletω w)))) p))

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
  
  (check-equal? (possible-purchases (list c-ggggg c-ggggg) b-ggggg purchase-points)
                (list (purchase (list c-ggggg) 5 (b:bag)) #; (purchase (list c-ggggg) 5 (b:bag))))

  (check-equal? (buy-cards (list c-ggggg c-ggggg) b-ggggg purchase-points)
                (purchase (list c-ggggg) 5 (b:bag)))

  (check-equal? (buy-cards (list c-ggggg c-ggggg) (b:bag-add b-ggggg  b-ggggg) purchase-points)
                (purchase (list c-ggggg c-ggggg) 6 (b:bag))))

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

#; {[nEListof (X -> Real)] [NEListof X] -> X}
#; (tie-breaker (f1 ... fN) lox)
;; apply the fj-s to lox until 
#; (fi ... (f1 lox) ...)
;; yields singleton list; otherwise error 
(define (tie-breaker f*0 lox0)
  (let while ([lox lox0] [f* f*0])
    (match lox 
      [(list one) one]
      [_ (if (empty? f*)
             [error 'tie-breakder "given ~a\n" (with-output-to-string (λ () (pretty-print lox0)))]
             (while ((first f*) lox) (rest f*)))])))

;                                                                 
;                                                                 
;                 ;;;    ;;;                    ;                 
;                   ;      ;                    ;                 
;    ;;;    ;;;     ;      ;     ;;;    ;;;   ;;;;;   ;;;    ;;;; 
;   ;;  ;  ;; ;;    ;      ;    ;;  ;  ;;  ;    ;    ;; ;;   ;;  ;
;   ;      ;   ;    ;      ;    ;   ;; ;        ;    ;   ;   ;    
;   ;      ;   ;    ;      ;    ;;;;;; ;        ;    ;   ;   ;    
;   ;      ;   ;    ;      ;    ;      ;        ;    ;   ;   ;    
;   ;;     ;; ;;    ;      ;    ;      ;;       ;    ;; ;;   ;    
;    ;;;;   ;;;      ;;     ;;   ;;;;   ;;;;    ;;;   ;;;    ;    
;                                                                 
;                                                                 
;                                                                 

#; {class (X)
     [init {e0 X}]
     [init-field {score (X -> Real)}]
     [done (->m [Listof X])]
     [add-if-better (->m X Void)]}
;; collect the best elements according to `score` 
(define collector%
  (class object% (init e0) (init-field score)
    (field [*best-score (score e0)])
    (field [*possibles [set e0]])
    (super-new)
    
    (define/public (done)
      (set->list *possibles))

    (define/public (add-if-better e1)
      (define v1 (score e1))
      (cond
        [(> v1 *best-score)
         (set!-values (*best-score *possibles) (values v1 `[,e1]))]
        [(= v1 *best-score)
         (set! *possibles (set-add *possibles e1))]
        [else (void)]))))
