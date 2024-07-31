#lang racket

;; a data representation of the ref's knowledge about the active olayer that it shares during turn

(provide
 #; {type Player}
 player?
 player-score

 #; {type Score = Natural}

 #; {[Listof Player] -> Pict}
 render*
 
 #; {Player -> Pict}
 render

 #; {[Listof Score] -> Pict}
 render-scores)

(provide ;; for homework
 player-struct->definition)

(module+ examples
  (provide
   p-rg1 p-rg2 p-bbbb3 p-4xb-3xg4 p-ggg5 p-r6 p-g7 p-ggb8 p-gw9 p-ggggg p-rgbrg p-wyrbb p-rrbrr))

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
;                 ;                                                                    

(require (submod Bazaar/Common/bags json))
(require (prefix-in b: Bazaar/Common/bags))
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

(struct/description
 player
 [wallet #:to-jsexpr bag->jsexpr     #:from-jsexpr jsexpr->bag     #:is-a "*Pebblaes"]
 [score  #:to-jsexpr natural->jsexpr #:from-jsexpr jsexpr->natural #:is-a "Natural"])

(module+ examples
  (define p-rg1 (player b-rg 1))
  (define p-rg2 (player b-rg 2))
  (define p-bbbb3 (player b-bbbb 3))
  (define p-4xb-3xg4 (player b-4xb-3xg 4))
  (define p-ggg5 (player b-ggg 5))
  (define p-r6 (player b-r 6))
  (define p-g7 (player b-g 7))
  (define p-ggb8 (player b-ggb 8))
  (define p-gw9 (player b-gw 9))
  (define p-ggggg (player b-ggggg 0))
  (define p-rgbrg (player b-rgbrg 0))
  (define p-wyrbb (player b-wyrbb 0))
  (define p-rrbrr (player b-rrbrr 0)))

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

#; {[Listof Player] -> Pict}
(define (render* players)
  (define p-states (map render players))
  (apply hb-append 5 p-states))

#; {Player -> Pict}
(define (render ps #:name (name ""))
  (match-define [player wallet score] ps)
  (define p-name   (text name "roman" 12))
  (define p-wallet (b:render wallet))
  (define p-score  (render-scores [list score]))
  (frame (inset (vl-append 5 p-name (hc-append 5 p-score p-wallet)) 3)))

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

  (def/jsexpr-> player*
    #:array [(list (app jsexpr->player (? player? p)) ...) p])

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

  (define lon '[1 2 3])
  (check-equal? (jsexpr->score* (score*->jsexpr lon)) lon)

  (define lon1 '[1 "a" 3])
  (check-false (jsexpr->score* (score*->jsexpr lon1))))