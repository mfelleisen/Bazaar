#lang racket

;; bags filled with pebbles 
;; ---------------------------------------------------------------------------------------------------

(provide
 #; {type [Bag X] = [Listof X]}
 ;; X is what the bag contains 

 bag?
 bag
 
 bag-empty?
 bag-member?
 bag-size
 subbag?
 bag-add ;; bag ... bag -> bag 
 bag-minus
 bag-intersect
 bag-equal?

 bag<= ;; if bag1 contains the pebble colors in order and bag2 fails
 
 (contract-out
  [bag-transfer
   #; (bag-transfer one to from-one-to-two from-two-to-one)
   (-> bag? bag? bag? bag? (values bag? bag?))])
 
 render)

;; ---------------------------------------------------------------------------------------------------
(module+ json
  (provide
   #;{[Bag X] [X -> JSexpr] -> JSExpr}
   bag->jsexpr

   #; {[JSExpr [Y -> Boolean : X] [JSExpr -> X] -> (U False [Bag X])]}
   jsexpr->bag))

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  (provide b-rg b-bbbb b-4xb-3xg b-ggg b-r b-g b-ggb b-gw b-rr b-gg b-yyw
           ;; for cards 
           b-bbbbb b-ggggg b-rgbrg b-wyrbb b-rrbrr b-rbbbb b-yyrwg))


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

(require (prefix-in p: (submod Bazaar/Common/pebbles examples)))
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

(module+ examples)


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
  
  (define b-bbbb    [bag p:BLUE p:BLUE p:BLUE p:BLUE])

  ;; usable for cards 
  (define b-rrbrr [bag p:RED p:RED p:BLUE p:RED p:RED])
  (define b-ggggg (bag p:GREEN p:GREEN p:GREEN p:GREEN p:GREEN))
  (define b-bbbbb [bag p:BLUE p:BLUE p:BLUE p:BLUE p:BLUE])
  (define b-rgbrg (bag p:RED p:GREEN p:BLUE p:RED p:GREEN))
  (define b-wyrbb (bag p:WHITE p:YELLOW p:RED p:BLUE p:BLUE))
  (define b-rbbbb (bag-add b-bbbb (list p:RED)))
  (define b-yyrwg (bag p:YELLOW p:YELLOW p:RED p:WHITE p:BLUE))

  (define b-yyw [bag p:YELLOW p:YELLOW p:WHITE])
  (define b-ggg [bag p:GREEN p:GREEN p:GREEN])
  (define b-gg  [bag p:GREEN p:GREEN])
  (define b-r   [bag p:RED])
  (define b-rr  [bag p:RED p:RED])
  (define b-g   [bag p:GREEN])
  (define b-ggb [bag p:GREEN p:GREEN p:BLUE])
  (define b-gw  [bag p:RED p:WHITE])
  (define b-4xb-3xg [bag p:BLUE p:BLUE p:BLUE p:BLUE p:GREEN p:GREEN p:GREEN])
  (define b-rg      [bag p:RED p:GREEN])

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

(define (bag<= 1bag 2bag)
  (for/first ([p p:PEBBLES] #:when (and (bag-member? 1bag p) (not (bag-member? 2bag p))))
    #true))

(define (bag-transfer wallet bank left right)
  (values (bag-add (bag-minus wallet left) right)
          (bag-add (bag-minus bank right) left)))

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
  (require SwDev/Testing/check-values)

  (check-true (jsexpr? (bag->jsexpr b-rrbrr)))
  (check bag-equal? (jsexpr->bag (bag->jsexpr b-rrbrr)) b-rrbrr "basic bag test")

  (check-values (bag-transfer b-rg b-rg b-r b-g) b-gg b-rr "transfer"))