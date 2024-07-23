#lang racket

;; a generic data representation of cards 

(provide
 #; {type [Card X]}
 ;; X is the "currency" displayed on cards 

 ;; examples 
 
 #; {Card N -> N}
 calculate-points

 #; {Card -> Pict}
 render)

(module+ examples
  (provide CARD1 CARD2))

(module+ json
  (provide
   #;{[Card X] [X -> JSexpr] -> JSExpr}
   card->jsexpr

   #; {[JSExpr [Y -> Boolean] [JSExpr -> X] -> (U False [Bag X])]}
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

(require (prefix-in p: "pebbles.rkt"))
(require Bazaar/Common/bags)
(require pict)
(require pict/face)

(module+ json
  (require (submod Bazaar/Common/bags json))
  (require Bazaar/Common/bags)
  (require Bazaar/Lib/parse-json))

(module+ pict
  (require (submod ".." examples)))

(module+ test
  (require (submod ".." examples))
  (require (submod ".." json))
  (require (submod Bazaar/Common/pebbles json))
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

(struct card [pebbles face?] #:transparent
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

#; {type Card = (card [Bad X] Boolean)}

(module+ examples
  (define CARD1 (card [list p:RED p:RED p:BLUE p:RED p:RED] #false))
  (define CARD2 (card [list p:RED p:RED p:BLUE p:RED p:RED] #true)))

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

(define (calculate-points card pebbles#)
    (for/first ([p POINTS] #:when (>= pebbles# (first p)))
      (if (card-face? card) (third p) (second p))))

(define (render c)
  (define pebbles (map p:render (card-pebbles c)))
  (define angle   (/ (* 2 pi) 5))
  (let* ([pebbles pebbles]
         [s (first pebbles)]
         [pebbles (rest pebbles)]
         [s (vc-append 20 s (apply hc-append 40 (take pebbles 2)))]
         [pebbles (drop pebbles 2)]
         [s (vc-append 20 s (apply hc-append 20 (take pebbles 2)))]
         [w (+ (pict-width s) 10)]
         [h (+ (pict-height s) 10)]
         [r (filled-rectangle w h #:color "turquoise")]
         [s (cc-superimpose r s)]
         [r (filled-rectangle w (quotient h 3) #:color "orange")]
         [s (vc-append r s r)]
         [s (if (card-face? c) (add-face s) s)])
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

  (define PEBBLES 'pebbles)
  (define FACE 'with-face)

  (define (card->jsexpr c e->jsexpr)
    (match-define [card pebbles face?] c)
    (define j-pebbles (bag->jsexpr pebbles (compose string->symbol e->jsexpr)))
    (define j-face?   (boolean->jsexpr face?))
    (hasheq PEBBLES j-pebbles FACE j-face?))
  
  (define (jsexpr->card j domain? jsexpr->e)
    (define (jsexpr->cbag j)
      (jsexpr->bag j (λ (j) (domain? (~a j))) (λ (j) (jsexpr->e (~a j)))))
    
    (def/jsexpr-> card
      #:object {[PEBBLES cbag (? bag? b)] [FACE boolean f]}
      (card b f))
    
    (jsexpr->card j)))

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
  (render CARD1)
  (render CARD2))

(module+ test
  (check-equal? (calculate-points CARD1 0) (second (last POINTS)))
  (check-equal? (calculate-points CARD2 0) (third (last POINTS)))
  
  (check-equal?
   (jsexpr->card (card->jsexpr CARD1 pebble->jsexpr) p:pebble-color? jsexpr->pebble) CARD1))
  