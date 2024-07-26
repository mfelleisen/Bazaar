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

(require pict)

(module+ test
  (require (submod ".." json))
  (require json)
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

;; a bag is just a list that may contain repeated elements 

(define bag list)

(define bag? list?)
 
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

(define bag-empty? empty?)

(define bag-size length)

(define (subbag? b c)
  (and (<= (length b) (length c)) (subset? b c)))

(define (bag-minus b c)
  (for/fold ([b b]) ([x c])
    (remove x b)))

(define (bag-remove b x)
  (remove x b))

(define (bag-equal? b c)
  (and (subbag? b c) (subbag? c b)))

(define (bag-intersect b c)
  (for/list ([x b] #:when (member x c)) x))

(define (render b render-element)
  (apply hc-append 2 (map render-element b)))

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
  (define (bag->jsexpr b e->jsexpr)
    (map e->jsexpr b))

  (define (jsexpr->bag j domain? jsexpr->e)
    (def/jsexpr-> bag #:array [(list (? domain? x) ...) (map jsexpr->e x)])
    (jsexpr->bag j)))

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
  (check-false (subbag? '[1 1 1 2] '[1 2]))
  (check-true (subbag? '[1 2]  '[1 1 1 2]))

  (check-equal? (bag-minus '[1 1 2] '[1 2]) '[1])
  (check-equal? (bag-minus '[1 2] '[1 1 2]) '[])
  (check-equal? (bag-minus '[1 1 2] '[1]) '[1 2])

  (define b1 '[1 1 2 3])
  (check-true (jsexpr? (bag->jsexpr b1 values)))
  (check bag-equal? (jsexpr->bag (bag->jsexpr b1 values) natural? values) b1 "basic bag test"))