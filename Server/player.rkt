#lang racket

;; this remote player implements the same interface as the player but conveys its arguments
;; to the given TCP out stream and receives the results on the TCP in stream
                                 
(provide
 (contract-out
  (create-remote (-> string? input-port? output-port? player/c))))

;                                                          
;                                                          
;                                  ;                       
;                                                          
;    ;;;;   ;;;    ;;;;  ;   ;   ;;;    ;;;;   ;;;    ;;;  
;    ;;  ; ;;  ;  ;; ;;  ;   ;     ;    ;;  ; ;;  ;  ;   ; 
;    ;     ;   ;; ;   ;  ;   ;     ;    ;     ;   ;; ;     
;    ;     ;;;;;; ;   ;  ;   ;     ;    ;     ;;;;;;  ;;;  
;    ;     ;      ;   ;  ;   ;     ;    ;     ;          ; 
;    ;     ;      ;; ;;  ;   ;     ;    ;     ;      ;   ; 
;    ;      ;;;;   ;;;;   ;;;;   ;;;;;  ;      ;;;;   ;;;  
;                     ;                                    
;                     ;                                    
;                     ;                                    
    
(require (submod Bazaar/Common/player-interface json))
(require Bazaar/Common/player-interface)
(require Bazaar/Remote/define-remote)
(require (except-in Bazaar/Lib/json string->jsexpr))

(module+ test
  (require (submod ".."))
  (require (submod Bazaar/Common/player-interface examples))
  (require (except-in (submod Bazaar/Referee/game-state examples) ForStudents/ Tests/))
  (require Bazaar/Referee/game-state)
  (require Bazaar/Remote/remote-testing)
  (require rackunit))

;                                            
;                                            
;          ;;;                               
;            ;                               
;   ;;;;     ;    ;;;;   ;   ;   ;;;    ;;;; 
;   ;; ;;    ;        ;  ;   ;  ;;  ;   ;;  ;
;   ;   ;    ;        ;   ; ;   ;   ;;  ;    
;   ;   ;    ;     ;;;;   ; ;   ;;;;;;  ;    
;   ;   ;    ;    ;   ;   ; ;   ;       ;    
;   ;; ;;    ;    ;   ;   ;;    ;       ;    
;   ;;;;      ;;   ;;;;    ;     ;;;;   ;    
;   ;                      ;                 
;   ;                     ;                  
;   ;                    ;;                  


(define (create-remote name in out)
  (new remote-player% [n name] [in in] [out out]))

(define remote-player%
  (class object% [init-field in out [n (gensym 'remote-player)]]
    (super-new)

    (define-define/remote define/remote in out)
    
    (define/public (name) n) ;; it is safe to call this locally 
    ;; -----------------------------------------------------------------------------------------------
    (define/remote (setup equations)               void)
    (define/remote (request-pebble-or-trades turn) action #:#f-okay #true)
    (define/remote (request-cards turn)            action)
    (define/remote (win boolean)                   void)))

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

(module+ test ;; most basic tests 
  (check-true (is-a? (create-remote "A" (current-input-port) (current-output-port)) remote-player%))
  (check-equal? (send (create-remote "N" (current-input-port) (current-output-port)) name) "N"))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (define eq1* (list r=gggg))
  (define jeq1 (equations->jsexpr eq1*))

  (define jts0 (turn->jsexpr ts0))

  (test-remote create-remote b
               (setup eq1*)
               #:remote (void->jsexpr #f)
               #:exp (void)
               #:msg `["setup" [,jeq1]])

  (test-remote create-remote d
               (request-pebble-or-trades ts0)
               #:remote '[]
               #:exp '[]
               #:msg `["request-pebble-or-trades" [,jts0]])

  (test-remote create-remote d
              (request-cards ts0)
               #:remote '[]
               #:exp '[]
               #:msg `["request-cards" [,jts0]])

  (test-remote create-remote g
               (win #f)
               #:remote (void->jsexpr #f)
               #:exp (void)
               #:msg '["win" [#f]]))
