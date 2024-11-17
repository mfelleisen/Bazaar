#lang racket

;; a data representation of the information that the referee sends to the active player
;; ---------------------------------------------------------------------------------------------------

(provide
 #; {type TurnState}
 turn?
 (contract-out
  [turn (-> b:bag? (listof c:card?) p:player? (listof natural?) turn?)])
 turn-active
 turn-bank
 turn-cards
 turn-wallet
 turn-scores

 #; {-> (values Bag Turn)}
 random-turn

 render

 combine)

;; ---------------------------------------------------------------------------------------------------
(provide ;; for milestone definitions 
 turn-struct->definition)

;; ---------------------------------------------------------------------------------------------------
(module+ json
  (provide
   turn->jsexpr
   jsexpr->turn))

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  (provide ts0 ts1 ts-20 ts-20-rotate ts-6-players ts-3-zeros))
  
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

(require (submod Bazaar/Common/bags json))
(require (submod Bazaar/Common/cards json))
(require (submod Bazaar/Common/player json))

(require (submod Bazaar/Common/cards examples))
(require (submod Bazaar/Common/bags examples))
(require (submod Bazaar/Common/player examples))

(require (prefix-in b: Bazaar/Common/bags))
(require (prefix-in c: Bazaar/Common/cards))
(require (prefix-in p: Bazaar/Common/player))

(require Bazaar/Lib/configuration)
(require Bazaar/Lib/parse-json)
(require pict)

(module+ pict
  (require (submod ".." examples)))

(module+ test
  (require (submod ".." examples))
  (require rackunit))
  
;                                                          
;       ;                                  ;            ;; 
;       ;           ;                      ;           ;   
;       ;           ;                      ;           ;   
;    ;;;;  ;;;;   ;;;;;  ;;;;           ;;;;   ;;;   ;;;;; 
;   ;; ;;      ;    ;        ;         ;; ;;  ;;  ;    ;   
;   ;   ;      ;    ;        ;         ;   ;  ;   ;;   ;   
;   ;   ;   ;;;;    ;     ;;;;         ;   ;  ;;;;;;   ;   
;   ;   ;  ;   ;    ;    ;   ;         ;   ;  ;        ;   
;   ;; ;;  ;   ;    ;    ;   ;         ;; ;;  ;        ;   
;    ;;;;   ;;;;    ;;;   ;;;;          ;;;;   ;;;;    ;   
;                                                          
;                                                          
;                                                          

(define (size-check cards)
  (unless (<= (length cards) VISIBLE#)
    (error 'jsexpr->card ""))
  cards)

(struct/description
 turn
 [bank   #:to-jsexpr bag->jsexpr    #:from-jsexpr jsexpr->bag #:is-a "*Pebbles"]
 [cards  #:to-jsexpr card*->jsexpr  #:from-jsexpr (compose size-check jsexpr->card*) #:is-a "*Cards"]
 [active #:to-jsexpr player->jsexpr #:from-jsexpr jsexpr->player #:is-a "Player"]
 [scores #:to-jsexpr score*->jsexpr #:from-jsexpr jsexpr->score* #:is-a "*Naturals"]
 #:transparent
 #:methods gen:equal+hash
 [(define equal-proc
    (λ (x y recursive-equal?)
      (match-define [turn x-bank x-cards x-active x-scores] x)
      (match-define [turn y-bank y-cards y-active y-scores] y)
      (and (equal? x-scores y-scores)
           (b:bag-equal? x-bank y-bank)
           (equal? x-active y-active)
           (= (length x-cards) (length y-cards))
           (b:bag-equal? (apply b:bag x-cards) (apply b:bag y-cards)))))
  (define (hash-proc x re-hash)
    (+ (* 1000 (re-hash (turn-bank x)))
       (* 10 (re-hash (turn-bank x)))))
  (define (hash2-proc x re-hash)
    (+ (* 1000 (re-hash (turn-bank x)))
       (* 10 (re-hash (turn-bank x)))))])

(define turn-wallet (compose p:player-wallet turn-active))

(module+ test ;; Ben's failure 21 Aug
  (define expected
    (jsexpr->turn
     '#hasheq((active
                .
                #hasheq((score . 4)
                        (wallet
                         .
                         ("blue" "blue" "blue" "blue" "green" "green" "green"))))
               (bank . ("red"))
               (cards
                .
                (#hasheq((face? . #f)
                         (pebbles . ("green" "green" "green" "green" "green")))))
               (scores . (5 6 7 8 9)))))

  (define received
    (jsexpr->turn 
     '#hasheq((active
                .
                #hasheq((score . 4)
                        (wallet
                         .
                         ("green" "green" "green" "blue" "blue" "blue" "blue"))))
               (bank . ("red"))
               (cards
                .
                (#hasheq((face? . #f)
                         (pebbles . ("green" "green" "green" "green" "green")))))
               (scores . (5 6 7 8 9)))))

  (check-equal? expected received "ben's failure"))

;                                                          
;                                                          
;                                      ;;;                 
;                                        ;                 
;    ;;;   ;   ;  ;;;;  ;;;;;;  ;;;;     ;     ;;;    ;;;  
;   ;;  ;   ; ;       ; ;  ;  ; ;; ;;    ;    ;;  ;  ;   ; 
;   ;   ;;  ;;;       ; ;  ;  ; ;   ;    ;    ;   ;; ;     
;   ;;;;;;   ;     ;;;; ;  ;  ; ;   ;    ;    ;;;;;;  ;;;  
;   ;       ;;;   ;   ; ;  ;  ; ;   ;    ;    ;          ; 
;   ;       ; ;   ;   ; ;  ;  ; ;; ;;    ;    ;      ;   ; 
;    ;;;;  ;   ;   ;;;; ;  ;  ; ;;;;      ;;   ;;;;   ;;;  
;                               ;                          
;                               ;                          
;                               ;                          

(module+ examples

  (provide t1 t2 t3 ts-noah)
  (define t1 (turn b-r '[] p-r-9 '[]))
  (define t2 (turn (b:bag) '[] p-r-9 '[]))
  (define t3 (turn (b:bag) '[] p-r-9 '[]))

  (define ts-noah      (turn (b:bag-add b-rrrr b-r) (list) p-rbwgy  '[]))

  (define ts0          (turn b-ggggg (list)                         p-r6 '[]))
  (define ts1          (turn b-ggggg [list c-ggggg c-rrbrr c-rgbrg] p-r6 '[]))
  (define ts-20        (turn b-r     [list c-ggggg]         p-rrbrr-20 '(6)))
  (define ts-20-rotate (turn b-r     [list c-ggggg]         p-r6       '(20)))
  (define ts-6-players (turn b-ggg   (list c-ggggg)         p-ggb8     '(5 6 7 9 4)))
  (define ts-3-zeros   (turn b-rrbrr (list c-wyrbb c-ggggg) p-ggggg     '[0 0]))

  ;; for equality testing
  (provide ts1-card-swap ts1-wallet)
  (define ts1-card-swap (turn b-ggggg [list c-rrbrr c-ggggg c-rgbrg] p-r6 '[]))
  (define ts1-wallet    (turn (b:bag-add b-g b-gggg) [list c-rrbrr c-ggggg c-rgbrg] p-r6 '[])))

(module+ examples
  (provide strat-t1 strat-t2 strat-t3 strat-t4 strat-t5 bank0 cards0 cards1)
  
  (define bank0  (b:bag-add b-bbbbb b-ggggg b-rrbrr b-rg b-rg))
  (define cards0 (list c-rrbrr* c-ggggg))
  (define cards1 (list c-rbbbb c-yyrwg* c-ggggg))
 
  (define strat-t1 (turn b-rg  cards0 p-4xb-3xg4 '[]))
  (define strat-t2 (turn bank0 cards0 p-6g-3r-4b '[]))
  (define strat-t5 (turn bank0 cards1 p-2r-2y-1w '[]))
  (define strat-t3 (turn bank0 cards1 p-3r-2y-1w '[]))
  (define strat-t4 (turn bank0 cards1 p-4r-2y-1w '[])))

(module+ examples
  (provide xstrat-1 xstrat-2 xstrat-3 xben-4)

  (require (submod Bazaar/Common/pebbles examples))
  
  (define xstrat-1
    (let* ([c (list c-ggggg* c-ggggg c-wgwgw* c-wgwgw)]
           [b (b:bag-add b-gg (b:bag YELLOW)(b:bag-minus b-yyw (b:bag WHITE)))])
      (turn b c px-1 '[])))

  (define xstrat-2 (turn (b:bag-add b-ggggg b-rbbbb) (list c-bbbbb c-ggggg) px-2 '[]))
  (define xstrat-3 (turn bank0 (list c-yyrwg* c-ggggg) p-4r-2y-1w '[]))

  (define xben-4
    (let* ([b (b:bag-add b-ggggg b-bbbbb)])
      (turn b (list c-rgggg c-rbbbb) p-rr-9 '[]))))

(module+ test
  (check-equal? ts1 ts1-wallet "construct wallet")
  (check-equal? ts1 ts1-card-swap "cards swapped")
  (check-false (equal? ts0 ts1)))
                                   


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

(define (render ts #:name (name ""))
  (match-define [turn bank visibles active scores] ts)
  (define p-bank     (b:render bank))
  (define p-active   (p:render active #:name name))
  (define p-visibles (c:render* visibles))
  (define p-scores   (p:render-scores scores))
  (combine p-bank p-visibles p-active p-scores))

(define (combine . x)
  (define WIDTH 12)
  (let* ([s (map (λ (p) (inset p (quotient WIDTH 2))) x)]
         [h (apply max 0 (map pict-height s))]
         [f (λ (p) (cb-superimpose p (rectangle (+ (pict-width p) WIDTH) h)))]
         [s (map f s)]
         [s (apply hb-append 10 s)]
         [s (frame (inset s 12))])
    s))

#; {-> (values [Listof Pebble] Turn)}
(define (random-turn)
  (define cc (list (c:random-card) (c:random-card) (c:random-card) (c:random-card)))
  (define pp (shuffle b-full))
  (define hf (/ (length pp) 2))
  (turn (drop pp hf) cc (p:random-player (take pp hf)) '[]))

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

(module+ pict
  (render ts1 #:name "Ben")
  (render ts1))

(module+ test
  (check-equal? (jsexpr->turn (turn->jsexpr ts0)) ts0)
  (check-equal? (jsexpr->turn (turn->jsexpr ts1)) ts1))