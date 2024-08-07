#lang racket

;; a data representationn for pebbles 

;; -----------------------------------------------------------------------------
(provide
 #; {type Pebble}

 pebble-color?

 #; {Pebble -> Pict}
 render )

(module+ examples
  (provide PEBBLES RED WHITE GREEN YELLOW BLUE))

(module+ json
  (provide
   pebble*->jsexpr
   jsexpr->pebble*
   pebble->jsexpr
   jsexpr->pebble))

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

(require Bazaar/scribblings/spec)
(require pict)

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

(struct pebble [color] #:prefab)

(define RADIUS 15)

(module+ examples
  (define PEBBLES (map pebble COLORS))

  (define RED    (pebble "red"))
  (define GREEN  (pebble "green"))
  (define YELLOW (pebble "yellow"))
  (define WHITE  (pebble "white"))
  (define BLUE   (pebble "blue")))

(define (pebble-color? x)
  (cons? (member x COLORS)))

(define (render p)
  (filled-ellipse RADIUS RADIUS #:color (pebble-color p)))

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

  (define (pebble*->jsexpr lo-cards)
    (map pebble->jsexpr lo-cards))

  (def/jsexpr-> pebble*
    #:array [(list (app jsexpr->pebble (? pebble? c)) ...) c])

  (define (pebble->jsexpr p)
    (pebble-color p))

  (define (jsexpr->pebble j)
    (match j
      [(? pebble-color?) (pebble j)]
      [_  (eprintf "jsexpr->pebble: pebble JSExpr expected, given ~a\n" j) #false])))

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
  (render (pebble "red")))

;; -----------------------------------------------------------------------------
(module+ test
  (check-equal? (jsexpr->pebble (pebble->jsexpr RED)) RED)
  (check-equal? (jsexpr->pebble* (pebble*->jsexpr (list RED))) (list RED)))