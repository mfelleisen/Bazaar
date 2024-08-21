#lang racket

;; the player mechanism

(provide
 (contract-out
  [player% player%/c]))

;; ---------------------------------------------------------------------------------------------------

(module+ examples
  (provide sample))

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

(require Bazaar/Player/strategies)
(require Bazaar/Common/actions)
(require Bazaar/Common/player-interface)
(require Bazaar/Common/turn-state)
(require (prefix-in p: Bazaar/Common/player))

(require (submod Bazaar/Player/strategies json))

(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod Bazaar/Common/equations examples))
  (require (submod Bazaar/Common/turn-state examples))
  (require SwDev/Lib/should-be-racket)
  (require rackunit))

;                                                   
;                                                   
;          ;;;                                      
;            ;                               ;;;    
;   ;;;;     ;    ;;;;   ;   ;   ;;;    ;;;; ;  ;   
;   ;; ;;    ;        ;  ;   ;  ;;  ;   ;;  ;;;;   ;
;   ;   ;    ;        ;   ; ;   ;   ;;  ;       ;;; 
;   ;   ;    ;     ;;;;   ; ;   ;;;;;;  ;     ;; ;; 
;   ;   ;    ;    ;   ;   ; ;   ;       ;       ;  ;
;   ;; ;;    ;    ;   ;   ;;    ;       ;       ;  ;
;   ;;;;      ;;   ;;;;    ;     ;;;;   ;        ;; 
;   ;                      ;                        
;   ;                     ;                         
;   ;                    ;;                         

;; this player serves as the base (like a common abstract class) for implementing several variants 
(define player%
  (class object% 
    (init-field
     [my-name  #; String "Adam"]
     [which    #; {Purchase -> Natural} purchase-size])

    (field [strategy (new strategy% [equations '()] [which which])])
    (field [equations '()])
    
    (super-new)

    (define/public (name)
      my-name)
    
    (define/public (description)
      `[,my-name ,(policy->jsexpr which)])

    (define/public (reset)
      (void))

    (define/public (setup equations0)
      (reset)
      (set! strategy (new strategy% [equations equations0] [which which]))
      (set! equations equations0))
    
    #; {Turn -> [Option Equation*]}
    ;; #false denotes a request for a random bebble from bank
    #; (list 1eq ...) ; denotes a sequence of left-to-right exchanges 
    (define/public (request-pebble-or-trades t)
      (define bank   (turn-bank t))
      (define wallet (turn-wallet t))
      (cond
        [(send strategy should-the-player-request-a-random-pebble t)
         want-pebble]
        [else
         (define visibles (turn-cards t))
         (define trades&buys (send strategy trade-then-purchase t))
         (set! *to-be-bought (exchange-cards trades&buys))
         (exchange-trades trades&buys)]))

    (define *to-be-bought #false) #; (U False [Listof Card])

    #; {Turn -> [Listof Card]}
    ;; the catds that the player wishes to buy, in order 
    (define/public (request-cards t)
      (when (boolean? *to-be-bought)
        (set! *to-be-bought [purchase-cards (send strategy buy-cards t)]))
      (begin0
        *to-be-bought
        (set! *to-be-bought #false)))

    (define/public (win b)
      (eprintf "~a ~a\n" my-name (if b "won" "lost"))
      (void))))

;                                                          
;                                                          
;                                      ;;;                 
;                                        ;                 
;    ;;;   ;   ;  ;;;;  ;;;;;;  ;;;;     ;     ;;;    ;;;  
;   ;;  ;   ; ;       ; ;  ;  ; ;; ;;    ;    ;;  ;  ;   ; 
;   ;   ;;  ;;;       ; ;  ;  ; ;   ;    ;    ;   ;; ;     
;   ;;;;;;   ;     ;;;; ;  ;  ; ;   ;    ;    ;;;;;;  ;;;  
;   ;       ;;;   ;   ; ;  ;  ; ;   ;    ;    ;          ; 
;   ;       ; ;   ;   ; ;  ;  ; ;; ;;    ;    ;      ;   ; 
;    ;;;;  ;   ;   ;;;; ;  ;  ; ;;;;      ;;   ;;;;   ;;;  
;                               ;                          
;                               ;                          
;                               ;                          

(module+ examples
  (define sample (new player%)))

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
  (check-true (is-a? sample player%) "test coverage")
  (check-equal? (send sample name) "Adam" "coverage")
  (check-true (cons? (send sample description)) "coverage")

  (begin 
    (check-true (void? (send sample setup '[])) "coverage")
    (check-equal? (send sample request-pebble-or-trades ts0) #false))
  
  (check-true (void? (send sample reset)) "coverage")
  (check-true (void? (dev/null (send sample win #true))) "coverage")
  (check-true (void? (dev/null (send sample win #false))) "coverage")

  (check-equal? (send sample request-cards ts0) '())

  (begin
    (send sample setup (list rg=bbbb ggg=r ggb=rw))
    (render ts0)
    (check-equal? (send sample request-pebble-or-trades ts0) '[] "a trade is possible, but useless")
    (check-equal? (send sample request-cards ts0) '[])) "the player can't buy anything")
