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

(provide
 #; {Equation* Bag Bag -> Exchange}
 should-the-olayer-request-a-random-pebble

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
  (define ns        (null-exchange))
  
  (define cards-for (list c-rrbrr* c-ggggg))
  (scenario+ ForStudents/ (list equations cards-for b-4xb-3xg b-rg purchase-points) ns "points 0")

  (define wal (b:bag-add b-rg b-rg b-rg b-4xb-3xg))
  (define ban (b:bag-add b-bbbbb b-ggggg b-rrbrr b-rg b-rg))
  (define res-1 [exchange '() (purchase (list c-ggggg) 1)])
  (scenario+ ForStudents/ (list equations cards-for wal ban purchase-size) res-1 "cards 1")
  
  (define res-2 [exchange `(,3xg=r) (purchase (list c-rrbrr*) 2)])
  (scenario+ ForStudents/ (list equations cards-for wal ban purchase-points) res-2 "points 2")

  (define cards-test (list c-rbbbb c-yyrwg* c-ggggg))
  (define wal-test   (b:bag-add b-rr b-yyw))
  (define res-test   [exchange (list 3xg=r- 3xg=r-) (purchase (list c-ggggg) 1)])
  (scenario+ Tests/ (list equations cards-test wal-test ban purchase-points) res-test "t 1")

  (define wal-test2  (b:bag-add wal-test b-r))
  (define res-test2  [exchange (list 3xg=r- r-g=4xb) (purchase (list c-yyrwg*) 2)])
  (scenario+ Tests/ (list equations cards-test wal-test2 ban purchase-points) res-test2 "x 2")

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

(define #; /contract (trade-then-purchase equations visibles wallet0 bank0 which)
  (-> (listof e:1eq?) b:bag? b:bag? (-> b:bag? (λ (x) (purchase? x))) exchange?)
  (define pts (possible-trades equations wallet0 bank0 (λ (w) (buy-cards visibles w which))))
  (cond
    [(empty? pts) '()]
    [else (tie-breaker-trade-then-purchase wallet0 pts)]))

#; {Equation* Bag Bag {Bag -> Purchases} -> [Listof [Listof Exchange]]}
;; determine all possible exchanges that apply up to a certain depth
;; and then maxi
(define #; /contract (possible-trades equations wallet0 bank0 buy-with-wallet)
  (-> (listof e:1eq?) b:bag? b:bag? (-> b:bag? (λ (x) (purchase? x))) (listof (listof exchange?)))
  #; [Listof [Listof Exchange]]
  (define possibles '[])
  (define p-so-far0 (list (exchange '() (buy-with-wallet wallet0))))
  (define fuel0    (SearchDepth))
  (let p-t/accu ([wallet wallet0] [bank bank0] [trades-so-far '()] [p-so-far p-so-far0] [fuel fuel0])
    (define rules (e:useful equations wallet bank))
    (cond
      [(or (empty? rules) (zero? fuel))
       (set! possibles (cons (reverse p-so-far) possibles))]
      [else
       (for ([x rules])
         (define-values (wallet++ bank++) (b:bag-transfer wallet bank (e:1eq-left x) (e:1eq-right x)))
         (define trades   (cons x trades-so-far))
         (define best-buy (buy-with-wallet wallet++))
         (define xchange* (cons (exchange (reverse trades) best-buy) p-so-far))
         (p-t/accu wallet++ bank++ trades xchange* (sub1 fuel)))]))

  possibles)

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
  (define sorted    (sort-by-lhs richest))
  (first sorted))

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

#; {[Listof Exchange] -> [Listof Exchange]}
(define (sort-by-lhs richest)
  (sort richest b:bag<= #:key (λ (ex) (map e:1eq-left (exchange-trades ex)))))

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
         (define points++    (+ (c:calculate-points t (b:bag-size wallet--)) points))
         (define from-root++ (cons t from-root-to-here))
         (p-p/accu visibles-- wallet-- from-root++ points++))]))
  
  possibles)

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
;; pick the list of cards that is 
(define (tie-breaker-for-purchases all-best)
  (sort all-best cards<=))

#; {[Listof Card] [Listof Card] -> Natural}
;; one card sequence is below another seq if the cards are below each other in order 
(define (cards<= 1cards 2cards)
  (for/first ([p 1cards] [q 2cards] #:when (c:1card<= p q)) #true))

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
