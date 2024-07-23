#lang racket

;; a data representation of (generic) equations

;; ---------------------------------------------------------------------------------------------------
(provide
 #; {type [Equation* X] = [Listof [1Equation X]]}
 #; {type [1Equation X]}


 #; {[Equations X] [Bag X] [Bag X] -> [Equations X]}
 #; (useful left-to-right my-wallet bank)
 ;; return those equations `e` in `left-to-right` for which `my-wallet` has
 ;; enough Xs to swap one side and `bank`has enough Xs for the other;
 ;; orient the resulting equations so that `my-wallet` covers the left
 useful
 
 #; {[1Equation X] [X -> Pict] -> Pict}
 #; (render e render-x)
 ;; returns an image of `e` where `render-x` is used to render individual Xs
 render

 #; {[Bag X] [Bag X] -> 1Equation}
 1eq)

(module+ examples
  (provide EQ1 EQ1-rev)
  (provide EQ1* EQ1-rev*))

(module+ json
  (provide 
   1eq->jsexpr
   jsexpr->1eq))

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

(require Bazaar/Common/bags)
(require pict)

(module+ pict
  (require (prefix-in p: Bazaar/Common/pebbles)))

(module+ examples
  (require (prefix-in p: (submod Bazaar/Common/pebbles examples))))

(module+ json
  (require (submod Bazaar/Common/bags json))
  (require (submod Bazaar/Common/pebbles json))
  (require Bazaar/Common/pebbles)
  (require Bazaar/Lib/parse-json))
  
(module+ test
  (require (submod ".." examples))
  (require (submod ".." json))
  (require Bazaar/Lib/parse-json)
  (require rackunit))

(module+ pict
  (require (submod ".." examples)))

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

(struct 1eq [left right] #:transparent

  #:methods gen:equal+hash
  [(define equal-proc
     (λ (x y recursive-equal?)
       (and
        (bag-equal? (1eq-left x) (1eq-left y))
        (bag-equal? (1eq-right x) (1eq-right y)))))
   (define (hash-proc x re-hash)
     (+ (* 1000 (re-hash (1eq-left x)))
        (* 10 (re-hash (1eq-right x)))))
   (define (hash2-proc x re-hash)
     (+ (* 1000 (re-hash (1eq-left x)))
        (* 10 (re-hash (1eq-right x)))))])

#; {type Equation = (1eq Side Side)}
#; {type Side     = b:Bag || (<= 1 (bag-size b) 4)}

(module+ examples 
  #; 1Equation 
  (define EQ1 [1eq '[a a b] '[c]])
  (define EQ1-rev [1eq '[c] '[a a b]])

  #; [Listof 1Equatin]
  (define EQ1* [list EQ1])
  (define EQ1-rev*  [list EQ1-rev])

  (provide W-R-G W-4x-b r-g=4xb) 

  (define W-R-G   `[,p:RED ,p:GREEN])
  (define W-4x-b  `[,p:BLUE ,p:BLUE ,p:BLUE ,p:BLUE])
  (define r-g=4xb (1eq W-R-G W-4x-b)))

(module+ pict
  (render r-g=4xb p:render))

(module+ examples
  #; {[Listof [List ActualArguments ExpectedResult Message]]}
  (provide ForStudents/ Tests/)

  (define-syntax-rule (scenario+ kind actual expected msg)
    (set! kind (append kind (list [list actual expected msg]))))
  
  (define ForStudents/ '[])
  (scenario+ ForStudents/ `[,(list r-g=4xb) ,W-R-G ,W-4x-b] (list r-g=4xb) "left to right, not vv")
  
  (define Tests/ '[]))

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

(define (useful left-to-right my-wallet bank)
  (define right-to-left (map 1eq-flip left-to-right))
  (for/fold ([result '()]) ([e (append left-to-right right-to-left)]
                            #:when (can-swap? e my-wallet bank))
    (cons e result)))

#; {1Equation Bag Bad -> Boolean}
;; can `my-wallet` swap with `bank` according to `e`? 
(define (can-swap? e my-wallet bank)
  (and (subbag? (1eq-left e) my-wallet) (subbag? (1eq-right e) bank)))

#; {1Equation -> 1Equation}
(define (1eq-flip e)
  (1eq (1eq-right e) (1eq-left e)))

;; ---------------------------------------------------------------------------------------------------
;; graphical representation 

(define (render 1eq render-element)
  (define left (apply hc-append 2 (map render-element (1eq-left 1eq))))
  (define right (apply hc-append 2 (map render-element (1eq-right 1eq))))
  (hc-append 5 left (text "=") right))

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
  (define (1eq->jsexpr eq e->jsexpr)
    (match-define [1eq left right] eq)
    (list (bag->jsexpr left e->jsexpr) (bag->jsexpr right e->jsexpr)))

  (define (jsexpr->1eq j domain? jsexpr->e)
    (define (jsexpr->cbag j)
      (jsexpr->bag j (λ (j) (domain? j)) (λ (j) (jsexpr->e j))))

    (def/jsexpr-> 1eq
      #:array [[list (app jsexpr->cbag (? bag? left)) (app jsexpr->cbag (? bag? right))]
               (1eq left right)])
    (jsexpr->1eq j)))


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
  (render EQ1 (λ (x) (text (~a x)))))

(module+ test
  (define my-wallet '[a a b c])
  (define bank      '[a a b c])
  (define low-bank  '[c])

  (check-equal? (useful [list [1eq '[a a b] '[c]]] my-wallet bank) (append EQ1-rev* EQ1*))
  
  (check-true (can-swap? EQ1 my-wallet low-bank))
  (check-equal? (useful [list [1eq '[a a b] '[c]]] my-wallet low-bank) EQ1*))

;; scenario testing 
(module+ test
  (define (equation-tests scenario*)
    (for ([s scenario*])
      (match-define `[,actual ,expected ,msg] s)
      (check-equal? (apply useful actual) expected msg)))

  (equation-tests ForStudents/))

;; json testing 
(module+ test
  (check-equal? (jsexpr->1eq (1eq->jsexpr EQ1 ~a) string? string->symbol) EQ1))
  