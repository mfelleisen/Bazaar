#lang racket

;; a data representation of the information that the referee sends to the active player
;; ---------------------------------------------------------------------------------------------------

(provide
 #; {type TurnState}
 turn?
 turn

 render

 combine)

(provide ;; for milestone definitions 
 turn-struct->definition)

(module+ json
  (provide
   turn->jsexpr
   jsexpr->turn))

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

(struct/description
 turn
 [bank   #:to-jsexpr bag->jsexpr #:from-jsexpr jsexpr->bag #:is-a "*Pebbles"]
 [cards  #:to-jsexpr card*->jsexpr  #:from-jsexpr jsexpr->card* #:is-a "*Cards"]
 [active #:to-jsexpr player->jsexpr #:from-jsexpr jsexpr->player #:is-a "Player"]
 [scores #:to-jsexpr score*->jsexpr #:from-jsexpr jsexpr->score* #:is-a "*Naturals"])

(module+ examples
  (define ts0          (turn b-ggggg (list)                         p-r6 '[]))
  (define ts1          (turn b-ggggg [list c-ggggg c-rrbrr c-rgbrg] p-r6 '[]))
  (define ts-20        (turn b-r     [list c-ggggg]         p-rrbrr-20 '(6)))
  (define ts-20-rotate (turn b-r     [list c-ggggg]         p-r6       '(20)))
  (define ts-6-players (turn b-r     (list c-ggggg)         p-4xb-3xg4 '(5 6 7 8 9)))
  (define ts-3-zeros   (turn b-rrbrr (list c-wyrbb c-ggggg) p-ggggg     '[0 0])))
                                   


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