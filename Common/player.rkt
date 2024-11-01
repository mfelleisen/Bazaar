#lang racket

;; a data representation of the ref's knowledge about the active olayer that it shares during turn
;; ---------------------------------------------------------------------------------------------------

(provide
 #; {type Score = Natural}
 #; {type Player}
 player?
 player-score
 player-wallet
 player-cards
 update-player-wallet
 update-player-score

 #; {Bag -> Player}
 random-player 
 
 #; {Player -> Boolean}
 winning-points?

 #; {Player -> N}
 ;; update this player with a bonus if it collected the appropriate RWB cards 
 player-award-red-white-and-blue-bonus
 player-award-none

 #; {Player Natural -> Player}
 update-score

 #; {Player Bag Bag  -> Player}
 update-pebbles

 #; {Player [Listof Card] -> Player}
 update-player-cards 
 
 #; {[Listof Player] -> Pict}
 render*
 
 #; {Player -> Pict}
 render

 #; {[Listof Score] -> Pict}
 render-scores)

;; ---------------------------------------------------------------------------------------------------
(provide ;; for homework
 player-struct->definition)

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  (provide
   p-rg1 p-rg2
   p-bbbb3 p-bbbbb3
   p-4xb-3xg4 p-ggg5 p-r6 p-g7 p-ggb8 p-gw9 p-ggggg p-rgbrg p-wyrbb p-rrbrr-20))

;; ---------------------------------------------------------------------------------------------------
(module+ json
  (provide
   player*->jsexpr
   jsexpr->player*
   
   player->jsexpr
   jsexpr->player

   score*->jsexpr
   jsexpr->score*))

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

(require Bazaar/scribblings/spec)

(require (prefix-in p: (submod Bazaar/Common/pebbles examples)))

(require (submod Bazaar/Common/bags json))
(require (prefix-in b: Bazaar/Common/bags))
(require (prefix-in c: Bazaar/Common/cards))

(require Bazaar/Lib/configuration)
(require Bazaar/Lib/json)
(require pict)


(module+ examples
  (require (submod Bazaar/Common/bags examples)))

(module+ pict
  (require (submod ".." examples)))

(module+ test
  (require (submod ".." examples))
  (require (submod ".." json))
  (require (submod Bazaar/Common/cards examples))
  (require rackunit))

(module+ json
  (require Bazaar/Lib/parse-json))
  
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

(define (size-check b)
  (unless (pebbles#? (b:bag-size b))
    (error 'jsexpr->card "wrong number of pebbles for a player's wallet"))
  b)

(struct/description
 player
 [wallet #:to-jsexpr bag->jsexpr     #:from-jsexpr (compose size-check jsexpr->bag) #:is-a "*Pebbles"]
 [score  #:to-jsexpr natural->jsexpr #:from-jsexpr jsexpr->natural #:is-a "Natural"]
 [cards #:hidden '()]
 #:transparent
 #:methods gen:equal+hash
 [(define equal-proc
    (λ (x y recursive-equal?)
      (match-define [player x-wallet x-score c] x)
      (match-define [player y-wallet y-score d] y)
      (and (= x-score y-score) (b:bag-equal? x-wallet y-wallet))))
  (define (hash-proc x re-hash)
    (+ (* 1000 (re-hash (player-wallet x)))
       (* 10 (re-hash (player-score x)))))
  (define (hash2-proc x re-hash)
    (+ (* 1000 (re-hash (player-wallet x)))
       (* 10 (re-hash (player-score x)))))])

(module+ test
  (define e-wallet (b:bag "blue" "blue" "blue" "blue" "green" "green" "green"))
  (define a-wallet (b:bag "green" "green" "green" "blue" "blue" "blue" "blue"))

  (check-true (b:bag-equal? e-wallet a-wallet) "ben's failure for wallets")

  (define e-active
    (jsexpr->player 
     #hasheq((score . 4) (wallet . ("blue" "blue" "blue" "blue" "green" "green" "green")))))

  (define a-active
    (jsexpr->player
     #hasheq((score . 4) (wallet . ("green" "green" "green" "blue" "blue" "blue" "blue")))))

  (check-equal? e-active a-active "ben's failure for players"))

(define (update-player-cards p cards)
  (struct-copy player p [cards (append cards (player-cards p))]))

(define (update-player-wallet p wallet)
  (struct-copy player p [wallet wallet]))

(define (update-player-score p delta)
  (struct-copy player p [score (+ (player-score p) delta)]))

(module+ examples
  (define p-rg1      (player b-rg 1 '[]))
  (define p-rg2      (player b-rg 2 '[]))
  (define p-bbbb3    (player b-bbbb 3 '[]))
  (define p-bbbbb3   (player b-bbbbb 3 '[]))
  (define p-4xb-3xg4 (player b-4xb-3xg 4 '[]))
  (define p-ggg5     (player b-ggg 5 '[]))
  (define p-r6       (player b-r 6 '[]))
  (define p-g7       (player b-g 7 '[]))
  (define p-ggb8     (player b-ggb 8 '[]))
  (define p-gw9      (player b-gw 9 '[]))
  
  (define p-ggggg    (player b-ggggg 0 '[]))
  (define p-rgbrg    (player b-rgbrg 0 '[]))
  (define p-wyrbb    (player b-wyrbb 0 '[]))
  
  (define p-rrbrr-20 (player b-rrbrr PLAYER-WINS '[])))

(module+ examples ;; from turn 
  (provide p-rg-0 p-ggg-9 p-r-9 p-rg-9 p-rr-9 p-bbbbb-0 p-5b-5g-0)
  
  (define p-5b-5g-0  (player (b:bag-add b-bbbbb b-ggggg) 0 '[]))
  (define p-bbbbb-0  (player b-bbbbb 0 '[]))
  (define p-rr-9     (player (b:bag-add b-r b-r) 9 '[]))
  (define p-rg-9     (player b-rg 9 '[]))
  (define p-r-9      (player b-r 9 '[]))
  (define p-rg-0     (player b-rg 0 '[]))
  (define p-ggg-9    (player b-ggg 9 '[])))

(module+ examples ;; for strategies
  (provide p-6g-3r-4b p-3r-2y-1w p-4r-2y-1w p-2r-2y-1w px-1 px-2)

  (require (submod Bazaar/Common/pebbles examples))

  (define wallet-test (b:bag-add b-rr b-yyw))
  
  (define p-2r-2y-1w (player b-2r-2y-1w 0 '[]))
  (define p-3r-2y-1w (player b-3r-2y-1w 0 '[]))
  (define p-4r-2y-1w (player b-4r-2y-1w 0 '[]))
  (define p-6g-3r-4b (player b-6g-3r-4b 0 '[]))

  (define px-1       (player (b:bag-add (b:bag WHITE GREEN) b-bbbb b-gggg) 0 '[]))
  (define px-2       (player (b:bag-add (b:bag-add b-rr b-b) b-g) 0 '[])))


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

#; {Player Bag Bag -> Player}
(define (update-pebbles p plus minus)
  (match-define [player wallet score c] p)
  (let* ([s wallet]
         [s (b:bag-minus s minus)]
         [s (b:bag-add s plus)])
    (player s score c)))

;; ---------------------------------------------------------------------------------------------------
#; {Player Natural -> Player}
(define (update-score p delta)
  (match-define [player wallet score c] p)
  (player wallet (+ score delta) c))

;; ---------------------------------------------------------------------------------------------------
(define (winning-points? p)
  (match-define [player _wallet score _cards] p)
  (>= score PLAYER-WINS))

;; ---------------------------------------------------------------------------------------------------
#; {Player -> Player}
(define (player-award-none p)
  p)

(define (player-award-red-white-and-blue-bonus p)
  (define cards (player-cards p))
  (if (c:contains-all (list p:RED p:WHITE p:BLUE) cards)
      (update-score p BONUS)
      p))

(define (player-award-rwb-on-distinct-cards p)
  (if (c:contains-on-separate-card (list p:RED p:WHITE p:BLUE) (player-cards p))
      (update-score p BONUS)
      p))

(module+ test
  (define a-bonus-player (player (b:bag) 18 (list c-rrbrr  c-rgbrg  c-wyrbb)))
  (define +-bonus-player (player (b:bag) (+ 18 BONUS) (list c-rrbrr  c-rgbrg  c-wyrbb)))
  (define no-bonus-player (player (b:bag) 18 (list))))

(module+ test 
  (check-equal? (player-award-red-white-and-blue-bonus a-bonus-player) +-bonus-player)
  (check-equal? (player-award-none a-bonus-player) a-bonus-player)

  (check-equal? (player-award-rwb-on-distinct-cards a-bonus-player) +-bonus-player)
  (check-equal? (player-award-rwb-on-distinct-cards no-bonus-player) no-bonus-player)

  (check-equal? (player-award-red-white-and-blue-bonus no-bonus-player) no-bonus-player))

;; ---------------------------------------------------------------------------------------------------
(define (random-player b)
  (player b (random 9) '()))

;; ---------------------------------------------------------------------------------------------------
#; {[Listof Player] -> Pict}
(define (render* players)
  (define p-states (map render players))
  (apply hb-append 5 p-states))

#; {Player -> Pict}
(define (render ps #:name (name ""))
  (match-define [player wallet score _cards] ps)
  (define p-name   (text name "roman" 12))
  (define p-wallet (b:render wallet))
  (define p-cards  (c:render* _cards))
  (define p-score  (render-scores [list score]))
  (frame (inset (vl-append 5 p-name (hc-append 5 p-score p-wallet p-cards)) 3)))

#; {[Listof Natural] -> Pict}
(define (render-scores scores)
  (apply hb-append 10 (map (λ (score) (text (~a score) "roman" 12)) scores)))

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
  (define (player*->jsexpr s)
    (map player->jsexpr s))

  (define (jsexpr->player* j)
    (def/jsexpr-> player* #:array [(list (app jsexpr->player (? player? p)) ...) p])
    (define players (jsexpr->player* j))
    (unless (<= (length players) MAX-PLAYERS)
      (error 'jexpr->player* "*Players contains more players than the game specs allow"))
    players)

  (define (score*->jsexpr s)
    (map natural->jsexpr s))

  (def/jsexpr-> score*
    #:array [(list (app jsexpr->natural (? natural? n)) ...) n]))

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
  (render p-r6 #:name "clown"))

(module+ test
  (check-equal? (jsexpr->player* (player*->jsexpr (list p-r6))) (list p-r6))
  (check-equal? (jsexpr->player (player->jsexpr p-r6)) p-r6)

  (check-equal? (update-score p-rg1 1) p-rg2)

  (check-equal? (update-pebbles p-bbbb3 `[,p:BLUE] '[]) p-bbbbb3)
  (check-equal? (update-pebbles p-bbbbb3 '[] `[,p:BLUE]) p-bbbb3)

  (define lon '[1 2 3])
  (check-equal? (jsexpr->score* (score*->jsexpr lon)) lon)

  (define lon1 '[1 "a" 3])
  (check-false (jsexpr->score* (score*->jsexpr lon1))))