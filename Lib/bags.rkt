#lang racket

;; a generic bad (multi-set) representation

;; ---------------------------------------------------------------------------------------------------
(provide
 #; {type [Bag X] = [Listof X]}
 ;; X is what the bag contains 

 bag?
 bag
 bag-empty?
 bag-member?
 bag->list
 bag-size
 (contract-out
  [subbag? (-> bag? bag? boolean?)])
 bag-add
 bag-minus
 bag-intersect
 bag-pick-random
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

(define bag->list identity)

(define bag-size length)

(define (subbag? b c)
  (let/ec return
    (for/fold ([c c]) ([x b])
      (if (member x c) (remove x c) (return #false)))
    #true))

(define (bag-minus b c)
  (for/fold ([b b]) ([x c])
    (remove x b)))

(define bag-add append)

(define (bag-remove b x)
  (remove x b))

(define (bag-equal? b c)
  (and (subbag? b c) (subbag? c b)))

(define (bag-intersect b c)
  (for/list ([x b] #:when (member x c)) x))

(define (bag-member? b x)
  (cons? (member x b)))

(define (bag-pick-random b)
  (list-ref b (random (length b))))

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
  (check bag-equal? (jsexpr->bag (bag->jsexpr b1 values) natural? values) b1 "basic bag test")
  
  (check-true (<= 1 (bag-pick-random '[1 2 3 4]) 4)))