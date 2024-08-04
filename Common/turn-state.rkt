#lang racket

;; a data representation of the information that the referee sends to the active player
;; ---------------------------------------------------------------------------------------------------

(provide
 #; {type TurnState}
 turn?
 turn

 render)

(provide ;; for milestone definitions 
 turn)

(module+ json
  (provide
   turn
   turn))

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
 [bank   #:to-jsexpr boolean->jsexpr #:from-jsexpr jsexpr->boolean #:is-a "Boolean"]
 [cards  #:to-jsexpr card*->jsexpr  #:from-jsexpr jsexpr->card* #:is-a "*Cards"]
 [active #:to-jsexpr player->jsexpr #:from-jsexpr jsexpr->player #:is-a "Player"]
 [scores #:to-jsexpr score*->jsexpr #:from-jsexpr jsexpr->score* #:is-a "Natural"])

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
  (define p-bank   (b:render bank))
  (define p-active (p:render active #:name name))
  (define p-visibles (c:render* visibles))
  (define p-scores (frame (inset (p:render-scores scores) 2)))
  (frame (inset (hb-append 10 p-bank p-visibles p-active p-scores) 5)))

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
  (check-equal? (turn (turn ts0)) ts0)
  (check-equal? (turn (turn ts1)) ts1))