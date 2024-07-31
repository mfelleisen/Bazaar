#lang racket

;; a data representation of the information that the referee sends to the active player 

(provide
 #; {type TurnState}
 turn-state?
 turn-state)

(provide ;; for milestone definitions 
 turn-state-struct->definition)

(module+ json
  (provide
   turn-state->jsexpr
   jsexpr->turn-state))

(module+ examples
  (provide ts1))
  
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
(require (prefix-in p: Bazaar/Common/player))

(require Bazaar/Lib/configuration)
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
 turn-state
 [bank   #:to-jsexpr bag->jsexpr #:from-jsexpr jsexpr->bag #:is-a "*Pebbles"]
 [cards  #:to-jsexpr card*->jsexpr #:from-jsexpr jsexpr->card* #:is-a "*Cards"]
 [active #:to-jsexpr player->jsexpr #:from-jsexpr jsexpr->player #:is-a "Player"]
 [scores #:to-jsexpr score*->jsexpr #:from-jsexpr jsexpr->score* #:is-a "Natural"])

(module+ examples
  (define ts1 (turn-state b-ggggg [list c-ggggg c-rrbrr c-rgbrg] p-r6 '[])))

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
  (match-define [turn-state bank cards active scores] ts)
  (define p-bank   (b:render bank))
  (define p-active (p:render active #:name name))
  (define p-scores (frame (inset (p:render-scores scores) 2)))
  (frame (inset (hc-append 10 p-bank p-active p-scores) 5)))

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
  (render ts1 #:name "Ben"))

(module+ test
  (check-equal? (jsexpr->turn-state (turn-state->jsexpr ts1)) ts1))