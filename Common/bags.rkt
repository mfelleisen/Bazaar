#lang racket

;; a generic bad (multi-set) representation

;; ---------------------------------------------------------------------------------------------------
(provide
 #; {type [Bag X] = [Listof X]}
 ;; X is what the bag contains 

 bag?
 bag
 bag-empty?
 bag-size
 subbag?
 bag-minus
 bag-intersect
 bag-equal?
 render)

(module+ json
  (provide
   #;{[Bag X] [X -> JSexpr] -> JSExpr}
   bag->jsexpr

   #; {[JSExpr [Y -> Boolean : X] [JSExpr -> X] -> (U False [Bag X])]}
   jsexpr->bag))

(module+ examples
  (provide b-rg b-bbbb b-4xb-3xg b-ggg b-r b-g b-ggb b-gw
           b-ggggg b-rgbrg b-wyrbb b-rrbrr))

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

(require (prefix-in p: Bazaar/Common/pebbles))
(require (prefix-in lib: (only-in Bazaar/Lib/bags render)))
(require Bazaar/Lib/bags)
(require)

(module+ test
  (require (submod ".." examples))
  (require (submod ".." json))
  (require json)
  (require rackunit))

(module+ json
  (require (submod Bazaar/Common/pebbles json))
  (require (prefix-in lib: (submod Bazaar/Lib/bags json)))
  (require Bazaar/Lib/parse-json))

(module+ examples
  (require (prefix-in p: (submod Bazaar/Common/pebbles examples))))


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

;; a bag for this project is
#;   [Bag Pebble]

(module+ examples
  ;; usable for cards 
  (define b-rrbrr [bag p:RED p:RED p:BLUE p:RED p:RED])
  (define b-ggggg (bag p:GREEN p:GREEN p:GREEN p:GREEN p:GREEN))
  (define b-rgbrg (bag p:RED p:GREEN p:BLUE p:RED p:GREEN))
  (define b-wyrbb (bag p:WHITE p:YELLOW p:RED p:BLUE p:BLUE))
  
  (define b-ggg [bag p:GREEN p:GREEN p:GREEN])
  (define b-r   [bag p:RED])
  (define b-g   [bag p:GREEN])
  (define b-ggb [bag p:GREEN p:GREEN p:BLUE])
  (define b-gw  [bag p:RED p:WHITE])
  (define b-4xb-3xg [bag p:BLUE p:BLUE p:BLUE p:BLUE p:GREEN p:GREEN p:GREEN])
  (define b-rg      [bag p:RED p:GREEN])
  (define b-bbbb    [bag p:BLUE p:BLUE p:BLUE p:BLUE])

)

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

(define (render b) (lib:render b p:render))

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
  (define (bag->jsexpr b) (lib:bag->jsexpr b pebble->jsexpr))

  (define (jsexpr->bag j) (lib:jsexpr->bag j p:pebble-color? jsexpr->pebble)))

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
  (check-true (jsexpr? (bag->jsexpr b-rrbrr)))
  (check bag-equal? (jsexpr->bag (bag->jsexpr b-rrbrr)) b-rrbrr "basic bag test"))