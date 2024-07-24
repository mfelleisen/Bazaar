#lang racket

;; a data representation of (generic) equations

;; ---------------------------------------------------------------------------------------------------

(define (distinct-equations eq*)
  (distinct? (append eq* (map 1eq-flip eq*))))

(define (good-equations eq*)
  (for/and ([1eq eq*]) (b:bag-empty? (b:bag-intersect (1eq-left 1eq) (1eq-right 1eq)))))

(provide
 #; {type [Equation* X] = [Setof [1Equation X]]}
 
 #; {type [1Equation X] = [List [Setof X] [Setof X]]}
 ;; the intersection of thetwo sets must be empty 
 
 #; {[Equations X] [Equations X] -> Boolean}
 ;; are the two sets of equations equal? -- each side is considered a set, too
 equations-equal?

 #; {[Equations X] [Bag X] [Bag X] -> [Equations X]}
 #; (useful left-to-right my-wallet bank)
 ;; return those equations `e` in `left-to-right` for which `my-wallet` has
 ;; enough Xs to swap one side and `bank`has enough Xs for the other;
 ;; orient the resulting equations so that `my-wallet` covers the left
 (contract-out 
  [useful (-> (and/c (listof 1eq?) distinct-equations good-equations) b:bag? b:bag?
              (and/c (listof 1eq?) distinct?))])
 
 #; {[1Equation X] [X -> Pict] -> Pict}
 #; (render e render-x)
 ;; returns an image of `e` where `render-x` is used to render individual Xs
 render

 #; {[Bag X] [Bag X] -> 1Equation}
 1eq)

(module+ examples
  (provide
   #; {type UsefulScenarios =
            [Listof [List [List [Listof [1Equation Pebble]] [Bag Pebble] [Bag Pebble]]
                          [Listof [1Equation Pebble]]
                          String]]}

   #; UsefulScenarios
   ForStudents/
   Tests/
   Exns/))

(module+ json
  (provide
   
   #;{[1Equation X] [X -> JSexpr] -> JSExpr}
   equations->jsexpr

   #; {[JSExpr [Y -> Boolean : X] [JSExpr -> X] -> (U False [1Equation X])]}
   jsexpr->equations))

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

(require (prefix-in b: Bazaar/Common/bags))
(require SwDev/Contracts/unique)
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
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod ".." json))
  (require (prefix-in p: Bazaar/Common/pebbles))
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
        (b:bag-equal? (1eq-left x) (1eq-left y))
        (b:bag-equal? (1eq-right x) (1eq-right y)))))
   (define (hash-proc x re-hash)
     (+ (* 1000 (re-hash (1eq-left x)))
        (* 10 (re-hash (1eq-right x)))))
   (define (hash2-proc x re-hash)
     (+ (* 1000 (re-hash (1eq-left x)))
        (* 10 (re-hash (1eq-right x)))))])

#; {type Equation = (1eq Side Side)}
#; {type Side     = b:Bag || (<= 1 (bag-size b) 4)}

(module+ examples
  ;; local only
  (provide W-R-G W-4x-b r-g=4xb)
  (provide EQ1 EQ1-rev EQ1* EQ1-rev*)

  #; 1Equation 
  (define EQ1 [1eq '[a a b] '[c]])
  (define EQ1-rev [1eq '[c] '[a a b]])

  #; [Listof 1Equation]
  (define EQ1* [list EQ1])
  (define EQ1-rev*  [list EQ1-rev])
  
  (define W-R-G   `[,p:RED ,p:GREEN])
  (define W-4x-b  `[,p:BLUE ,p:BLUE ,p:BLUE ,p:BLUE])
  (define r-g=4xb (1eq W-R-G W-4x-b))
  (define g-r=4xb (1eq (reverse W-R-G) W-4x-b))
  (define 4xb=r-g (1eq W-4x-b W-R-G))

  (define W-4xb-3xg  `[,p:BLUE ,p:BLUE ,p:BLUE ,p:BLUE ,p:GREEN ,p:GREEN ,p:GREEN])
  (define 3xg=r    (1eq `[,p:GREEN ,p:GREEN ,p:GREEN] `[,p:RED]))
  (define 3xg=r- (1eq-flip 3xg=r))
  (define 3xg=g    (1eq `[,p:GREEN ,p:GREEN ,p:GREEN] `[,p:GREEN])) ;; bad equation
  (define ggb=rw    (1eq `[,p:GREEN ,p:GREEN ,p:BLUE] `[,p:RED ,p:WHITE])))

(module+ examples ;; make scenarios 
  (define-syntax-rule (scenario+ kind actual expected msg)
    (set! kind (append kind (list [list actual expected msg]))))
  
  (define ForStudents/ '[])
  (scenario+ ForStudents/ `[,(list r-g=4xb) ,W-R-G ,W-4x-b] (list r-g=4xb) "left2right, not vv")
  (scenario+ ForStudents/ `[,(list r-g=4xb) ,W-R-G ,W-4x-b] (list g-r=4xb) "left2right, permute")
  (scenario+ ForStudents/ `[,(list r-g=4xb 3xg=r) ,W-R-G ,W-4x-b] (list g-r=4xb) "left2right/permute")
  
  (define Tests/ '[])
  (scenario+ Tests/ `[,(list r-g=4xb) [] ,W-4x-b] (list) "emoty wallet")
  (scenario+ Tests/ `[,(list r-g=4xb) ,W-4x-b []] (list) "emoty bank")
  (scenario+ Tests/ `[,(list r-g=4xb ggb=rw 3xg=r) ,W-R-G ,W-4xb-3xg] (list 3xg=r- r-g=4xb) "3 -> 2")

  (define Exns/ '())
  (scenario+ Exns/ `[,(list r-g=4xb 4xb=r-g) ,W-R-G ,W-4x-b] #px"distinct-equations" "repeated eq")
  (scenario+ Exns/ `[,(list 3xg=g 4xb=r-g) ,W-R-G ,W-4x-b]   #px"good-equations" "1 bad eq"))

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

(define (equations-equal? eq* fq*)
  (and (subset? eq* fq*) (subset? fq* eq*)))
  
;; ---------------------------------------------------------------------------------------------------
(define (useful left-to-right my-wallet bank)
  (define right-to-left (map 1eq-flip left-to-right))
  (for/fold ([result '()]) ([e (append left-to-right right-to-left)]
                            #:when (can-swap? e my-wallet bank))
    (cons e result)))

#; {1Equation Bag Bad -> Boolean}
;; can `my-wallet` swap with `bank` according to `e`? 
(define (can-swap? e my-wallet bank)
  (and (b:subbag? (1eq-left e) my-wallet) (b:subbag? (1eq-right e) bank)))

#; {1Equation -> 1Equation}
(define (1eq-flip e)
  (1eq (1eq-right e) (1eq-left e)))

;; ---------------------------------------------------------------------------------------------------
;; graphical representation 

(define (render 1eq render-element)
  (define left  (b:render (1eq-left 1eq) render-element))
  (define right (b:render (1eq-right 1eq) render-element))
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
  (define (equations->jsexpr eq* e->jsexpr)
    (map (λ (1eq) (1eq->jsexpr 1eq e->jsexpr)) eq*))

  (define (jsexpr->equations j domain? jsexpr->e)
    (def/jsexpr-> equations
      #:array [(list (app (λ (j) (jsexpr->1eq j domain? jsexpr->e)) (? 1eq? 1eq)) ...) 1eq])
    (define eq* (jsexpr->equations j))

    (cond
      [(and eq* (distinct? eq*)) eq*]
      [else (eprintf "distinct set of equations expected, given ~a" j) #false]))
  
  (define (1eq->jsexpr eq e->jsexpr)
    (match-define [1eq left right] eq)
    (list (bag->jsexpr left e->jsexpr) (bag->jsexpr right e->jsexpr)))

  (define (jsexpr->1eq j domain? jsexpr->e)
    (define (jsexpr->cbag j)
      (jsexpr->bag j (λ (j) (domain? j)) (λ (j) (jsexpr->e j))))

    (def/jsexpr-> 1eq
      #:array [[list (app jsexpr->cbag (? b:bag? left)) (app jsexpr->cbag (? b:bag? right))]
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
  (render EQ1 (λ (x) (text (~a x))))
  (render r-g=4xb p:render))

(module+ test ;; basic testing 
  (define my-wallet '[a a b c])
  (define bank      '[a a b c])
  (define low-bank  '[c])
  (define lo-1-equation [list [1eq '[a a b] '[c]]])
  
  (check-true (can-swap? EQ1 my-wallet low-bank))

  (check-equal? (useful lo-1-equation my-wallet bank) (append EQ1-rev* EQ1*))
  (check-equal? (useful lo-1-equation my-wallet low-bank) EQ1*)

  (check-true (equations-equal? lo-1-equation lo-1-equation))

  (define lo-1-equation& [list [1eq '[a b a] '[c]]])
  (check-true (equations-equal? lo-1-equation& lo-1-equation))

  (define lo-1-equation-swapped [list [1eq '[c] '[a b a]]])
  (check-false (equations-equal? lo-1-equation& lo-1-equation-swapped)))
  

(module+ test ;; json testing 
  (check-equal? (jsexpr->equations (equations->jsexpr EQ1* ~a) string? string->symbol) EQ1*))

(module+ test ;; scenario testing
  #; {Symbol UsefulScenarios {#:check [Equality Thunk Any String -> Void]} -> Void}
  (define (run-scenario* t scenario* #:check (C (λ (equal? act exp m) (check equal? [act] exp m))))
    (eprintf "--------------- ~a\n" t)
    (for ([s scenario*] [i (in-naturals)])
      (match-define (list actual expected msg) s)
      (match-define (list equations wallet bank) actual)
      (show i equations wallet bank expected)
      (C equations-equal? (λ () (apply useful actual)) expected msg)))

  #; {Natural Equations Bag Bag (U Regexp Equations) -> Void}
  (define (show i equations wallet bank expected)
    (eprintf "scenario ~a\n" i)
    (define purple   (colorize (rectangle 10 10) "purple"))
    (define p-given  (show/aux equations))
    (define p-wallet (if (b:bag-empty? wallet) purple (b:render wallet p:render)))
    (define p-bank   (if (b:bag-empty? bank) purple (b:render bank p:render)))
    (define p-expect (if (list? expected) (show/aux expected) (text (~a expected) "roman" 12)))
    (define pl (text "+" "roman" 12))
    (define is (text "---->" "roman" 12))
    (define all (ht-append 5 p-given pl p-wallet pl p-bank is p-expect))
    (pretty-print (frame (inset all 10) #:line-width 4) (current-error-port)))

  #; {Equations -> Pict}
  (define (show/aux equations)
    (for/fold ([r (blank 1 1)]) ([1eq equations])
      (vl-append r (render 1eq p:render))))
    
  (run-scenario* 'for-students ForStudents/)
  (run-scenario* 'tests Tests/)
  (run-scenario* 'exns Exns/ #:check (λ (_ act exp msg) (check-exn exp act msg))))
