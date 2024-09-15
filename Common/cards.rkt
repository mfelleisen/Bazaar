#lang racket

;; a data representation of cards 
;; ---------------------------------------------------------------------------------------------------

(provide
 #; {type Card}
 ;; X is the "currency" displayed on cards
 card?
 card-pebbles
 card-face?

 #; {[NEListof Card] -> Boolean}
 card*<?
 
 #; {[Listof COLOR ][Listof Card] -> Boolean}
 contains-all
 
 #; {[Listof Card] -> Pict}
 render*

 #; {Card -> Pict}
 render)

(provide
 card-struct->definition)

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  (require (submod ".."))

  (provide ALL-CARDS)
     
  (provide c-rrbrr  c-rgbrg  c-wyrbb  c-ggggg  c-rbbbb c-rbbbb*   c-rgggg c-rgggg* c-bbbbb
           c-ywywy c-ywywy* c-rrbrr* c-rgbrg* c-wyrbb* c-ggggg* c-yyrwg c-yyrwg* c-bbbbb*
           c-wgwgw c-wgwgw*))

;; ---------------------------------------------------------------------------------------------------
(module+ json
  (provide
   FACE PEBBLES ;; for creating ill-formed JSON 

   #; {[Listof Card] -> JSexpr}
   card*->jsexpr

   #; {JSexpr -> (U False [Listof Card])}
   jsexpr->card*

   #;{Card  -> JSExpr}
   card->jsexpr

   #; {[JSExpr -> (U False Card)]}
   jsexpr->card))

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
(require (prefix-in p: Bazaar/Common/pebbles))
(require Bazaar/Common/bags)

(require Bazaar/Lib/parse-json)
(require Bazaar/Lib/configuration)

(require pict)
(require pict/face)

(module+ examples
  (require (submod Bazaar/Common/bags examples)))

(module+ pict
  (require (submod ".." examples)))

(module+ test
  (require (submod ".." examples))
  (require (submod ".." json))
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

(define (size-check b)
  (unless (<= (bag-size b) P-ON-CARD)
    (error 'jsexpr->card "too many pebbles for a card"))
  b)

(struct/description
 card
 [pebbles #:to-jsexpr bag->jsexpr #:from-jsexpr (compose size-check jsexpr->bag) #:is-a "*Pebbles"]
 [face?   #:to-jsexpr boolean->jsexpr #:from-jsexpr jsexpr->boolean #:is-a "Boolean"]
 #:transparent
 #:methods gen:equal+hash
 [(define equal-proc
    (λ (x y recursive-equal?)
      (and
       (bag-equal? (card-pebbles x) (card-pebbles y))
       (equal? (card-face? x) (card-face? y)))))
  (define (hash-proc x re-hash)
    (+ (* 1000 (re-hash (card-pebbles x)))
       (* 10 (re-hash (card-face? x)))))
  (define (hash2-proc x re-hash2)
    (+ (* 891 (re-hash2 (card-pebbles x)))
       (* 999 (re-hash2 (card-face? x)))))])

;; AWKWARD: OTHERWISE JSON DOESN'T WORK PROPERLY
(define PEBBLES 'pebbles)
(define FACE 'face?)

#; {type Card = (card Bag Boolean)}

(module+ examples
  (define c-rbbbb  (card b-rbbbb #false))
  (define c-rbbbb* (card b-rbbbb #true))

  (define c-rgggg  (card b-rgggg #false))
  (define c-rgggg* (card b-rgggg #true))
  
  (define c-yyrwg  (card b-yyrwb #false))
  (define c-yyrwg* (card b-yyrwb #true))
  
  (define c-rrbrr  (card b-rrbrr #false))
  (define c-rrbrr* (card b-rrbrr #true))

  (define c-ggggg  (card b-ggggg #false))
  (define c-ggggg* (card b-ggggg #true))
  (define c-bbbbb  (card b-bbbbb #false))
  (define c-bbbbb* (card b-bbbbb #true))
  (define c-rgbrg  (card b-rgbrg #false))
  (define c-rgbrg* (card b-rgbrg #true))
  (define c-wyrbb  (card b-wyrbb #false))
  (define c-wyrbb* (card b-wyrbb #true))

  (define c-ywywy  (card b-ywywy #false))
  (define c-ywywy* (card b-ywywy #true))
  (define c-wgwgw  (card b-wgwgw #false))
  (define c-wgwgw* (card b-wgwgw #true))
  
  (define ALL-CARDS
    (list
     c-rgggg c-rgggg*
     c-ywywy c-ywywy* c-wgwgw c-wgwgw*
     c-rbbbb c-rbbbb* c-yyrwg c-yyrwg* c-rrbrr
     c-rrbrr* c-ggggg c-ggggg* c-bbbbb c-bbbbb*
     c-rgbrg c-rgbrg* c-wyrbb c-wyrbb*)))

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

;; ---------------------------------------------------------------------------------------------------

#; {[NEListof Card] [NEListof Card] -> Boolean}
(define (card*<? 1loc 2loc)
  (for/and ([c 1loc] [d 2loc]) (card<? c d)))

#; {Card Card -> Boolean}
;; one card is smaller than another if either
;; (1) it has no face or
;; (2) its bag is smaller than the one of the second card
(define (card<? c d)
  (match-define [card c-pebbles c-face] c)
  (match-define [card d-pebbles d-face] d)
  (or (and (not (equal? c-face d-face)) (not c-face))
      (bag<? c-pebbles d-pebbles)))

(module+ test
  (check-true (card<? c-ggggg c-ywywy))
  (check-true (card<? c-ggggg c-ggggg*))
  (check-false (card<? c-ggggg* c-bbbbb*))
  (check-false (card<? c-ywywy* c-ggggg*)))

;; ---------------------------------------------------------------------------------------------------
#; {[Listof Pebble][Listof Card] -> Boolean}
(define (contains-all pebbles loc)
  (define all-pebbles (append-map card-pebbles loc))
  (for/and ([p pebbles])
    (member p all-pebbles)))

;; ---------------------------------------------------------------------------------------------------
(define (render* c*)
  (define how-many 6)
  (define L (length c*))
  (define picts (map render (take c* (min how-many L))))
  (cond
    [(>= L VISIBLE#) (apply hc-append 5 (squared (take picts VISIBLE#)) (drop picts VISIBLE#))]
    [else (apply hc-append 5 picts)]))

#; {[Listof Pict] -> Pict}
(define (squared picts)
  (vl-append
   (apply hb-append (take picts (quotient VISIBLE# 2)))
   (apply hb-append (drop picts (quotient VISIBLE# 2)))))

#; {Pict -> Pict}
(define (render c)
  (let* ([s (render-in0star-shape (card-pebbles c))]
         [s (put-star-shape-bad-on-background s)]
         [s (if (card-face? c) (add-face s) s)])
    s))

#; {Pict -> Pict}
;; should really use ;; P-ON-CARD
(define (put-star-shape-bad-on-background s)
  (let* ([w (+ (pict-width s) 10)]
         [h (+ (pict-height s) 10)]
         [r (filled-rectangle w h #:color "turquoise")]
         [s (cc-superimpose r s)]
         [r (filled-rectangle w (quotient h 3) #:color "orange")]
         [s (vc-append r s r)])
    s))

#; {Pict -> Pict}
(define (add-face s)
  (let* ([s s]
         [f (scale (face* 'normal 'huge #f default-face-color 0 -3) .1)]
         [s (cc-superimpose s (colorize f "silver"))])
    s))

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
  (define (card*->jsexpr lo-cards)
    (map card->jsexpr lo-cards))

  (define (jsexpr->card* j)
    (def/jsexpr-> card* #:array [(list (app jsexpr->card (? card? c)) ...) c])
    (define cards (jsexpr->card* j))
    (unless (<= (length cards) CARDS#)
      (error 'jsexpr->card* "*Cards contained more cards than the game specs allow"))
    cards))
    
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
  (render c-rrbrr)
  (render c-rrbrr*)

  (define c* (list c-rrbrr c-rrbrr* c-rgbrg c-wyrbb c-rgbrg* c-wyrbb* c-ggggg c-ggggg*))
  (render* (take c* VISIBLE#))
  (render* (rest (take c* VISIBLE#)))
  (render* c*))

(module+ test
  (check-equal? (jsexpr->card (card->jsexpr c-rrbrr)) c-rrbrr)
  (check-equal? (jsexpr->card* (card*->jsexpr (list c-rrbrr))) (list c-rrbrr)))
  
