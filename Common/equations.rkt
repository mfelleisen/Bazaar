#lang racket

;; a data representation of (generic) equations

;; ---------------------------------------------------------------------------------------------------

(define (distinct-equations eq*)
  (distinct? (append eq* (map 1eq-flip eq*))))

(define (good-equations eq*)
  (for/and ([e eq*])
    (match-define [1eq left right] e)
    (and (good-size left) (good-size right) (no-overlap left right))))

(define (no-overlap left right)
  (b:bag-empty? (b:bag-intersect left right)))

(define (good-size b)
  (<= 1 (b:bag-size b) [MAX-EQ-SIDE]))

;; ---------------------------------------------------------------------------------------------------
(provide
 #; {type Equation* = [Setof 1Equation]}
 
 #; {type 1Equation = [List Bag Bag]}
 ;; the intersection of thetwo sets must be empty
 1eq?
 #; {Bag Bag -> 1Equation}
 1eq
 1eq-left
 1eq-right
 
 #; {Equations Equations -> Boolean}
 ;; are the two sets of equations equal? -- each side is considered a set, too
 equations-equal?

 #; {Equations Bag Bag -> Equations}
 #; (useful left-to-right my-wallet bank)
 ;; return those equations `e` in `left-to-right` for which `my-wallet` has
 ;; enough Xs to swap one side and `bank`has enough Xs for the other;
 ;; orient the resulting equations so that `my-wallet` covers the left
 (contract-out 
  [useful (-> (and/c (listof 1eq?) distinct-equations good-equations) b:bag? b:bag?
              (and/c (listof 1eq?) distinct?))])
 
 #; {1Equation-> Pict}
 #; (render e)
 ;; returns an image of `e` where `render-x` is used to render individual Xs
 render

 #; {[Listof 1Equation] -> Pict}
 render*)

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  (provide
    r-g=4xb r-g=4xb-
    3xg=r   3xg=r-
    ggb=rw  ggb=rw-)
  
  (provide
   #; {type UsefulScenarios = [Listof 1Scenario]}
   #; {type 1Scenario       =  [List [List [Listof 1Equation] Bag Bag] [Listof 1Equation] String]}
  
   #; UsefulScenarios
   ForStudents/
   Tests/
   Exns/))

;; ---------------------------------------------------------------------------------------------------
(module+ json
  (provide
   #;{1Equation -> JSExpr}
   equations->jsexpr

   #; {[JSExpr -> (U False 1Equation)]}
   jsexpr->equations
   ;; this second one allows the use of the same equation more than once 
   jsexpr->trades))

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
(require (prefix-in b: Bazaar/Common/bags))
(require SwDev/Contracts/unique)
(require pict)

(module+ pict
  (require (prefix-in p: Bazaar/Common/pebbles)))

(module+ examples
  (require (submod Bazaar/Common/bags examples))
  (require (prefix-in p: (submod Bazaar/Common/pebbles examples)))
  (require SwDev/Testing/scenarios))

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
  (define r-g=4xb (1eq b-rg b-bbbb))
  (define r-g=4xb- (1eq-flip r-g=4xb))
  (define g-r=4xb (1eq (reverse b-rg) b-bbbb))
  (define 4xb=r-g (1eq b-bbbb b-rg))

  (define 3xg=r    (1eq b-ggg b-r))
  (define 3xg=r-   (1eq-flip 3xg=r))
  (define 3xg=g    (1eq b-ggg b-g)) ;; bad equation
  (define ggb=rw   (1eq b-ggb b-gw))
  (define ggb=rw-  (1eq-flip ggb=rw)))


(module+ examples ;; make scenarios
  (setup-scenarios scenario+ Tests/ ForStudents/ Exns/)
  
  (scenario+ ForStudents/ `[,(list r-g=4xb) ,b-rg ,b-bbbb] (list r-g=4xb) "left2right, not vv")
  (scenario+ ForStudents/ `[,(list r-g=4xb) ,b-rg ,b-bbbb] (list g-r=4xb) "left2right, permute")
  (scenario+ ForStudents/ `[,(list r-g=4xb 3xg=r) ,b-rg ,b-bbbb] (list g-r=4xb) "left2right/permute")
  
  (scenario+ Tests/ `[,(list r-g=4xb) [] ,b-bbbb] (list) "emoty wallet")
  (scenario+ Tests/ `[,(list r-g=4xb) ,b-bbbb []] (list) "emoty bank")
  (scenario+ Tests/ `[,(list r-g=4xb ggb=rw 3xg=r) ,b-rg ,b-4xb-3xg] (list 3xg=r- r-g=4xb) "3 -> 2")
  
  (scenario+ Exns/ `[,(list r-g=4xb 4xb=r-g) ,b-rg ,b-bbbb] #px"distinct-equations" "repeated eq")
  (scenario+ Exns/ `[,(list 3xg=g 4xb=r-g) ,b-rg ,b-bbbb]   #px"good-equations" "1 bad eq"))

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

(define (render 1eq)
  (define left  (b:render (1eq-left 1eq)))
  (define right (b:render (1eq-right 1eq)))
  (hc-append 5 left (text "=") right))

(define (render* e*)
  (define p-e* (map render e*))
  (apply vl-append 5 p-e*))

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
  (define (equations->jsexpr eq*)
    (map 1eq->jsexpr eq*))
  
  (define (jsexpr->equations j)
    (def/jsexpr-> equations #:array [(list (app jsexpr->1eq (? 1eq? 1eq)) ...) 1eq])
    (define eq* (jsexpr->equations j))
    
    (cond
      [(and eq* (distinct? eq*)) eq*]
      [else (eprintf "distinct set of equations expected, given ~a\n" j) #false]))

  (define (jsexpr->trades j)
    (def/jsexpr-> equations #:array [(list (app jsexpr->1eq (? 1eq? 1eq)) ...) 1eq])
    (define eq* (jsexpr->equations j))
    eq*)
  
  (define (1eq->jsexpr eq)
    (match-define [1eq left right] eq)
    (list (bag->jsexpr left) (bag->jsexpr right)))

  (define (jsexpr->1eq j)
    (def/jsexpr-> 1eq
      #:array [[list (app jsexpr->bag (? b:bag? left)) (app jsexpr->bag (? b:bag? right))]
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
  (render r-g=4xb))

(module+ test ;; basic testing 
  (define my-wallet '[a a b c])
  (define bank      '[a a b c])
  (define low-bank  '[c])
  (define lo-1-equation [list [1eq '[a a b] '[c]]])

  (check-true (equations-equal? lo-1-equation lo-1-equation))

  (define lo-1-equation& [list [1eq '[a b a] '[c]]])
  (check-true (equations-equal? lo-1-equation& lo-1-equation))

  (define lo-1-equation-swapped [list [1eq '[c] '[a b a]]])
  (check-false (equations-equal? lo-1-equation& lo-1-equation-swapped)))
  

(module+ test ;; json testing 
  (check-equal? (jsexpr->equations (equations->jsexpr (list r-g=4xb))) (list r-g=4xb)))

(module+ test ;; scenario testing
  #; {Symbol UsefulScenarios {#:check [Equality Thunk Any String -> Void]} -> Void}
  (define (run-scenario* t scenario* #:check (C (λ (equal? act exp m) (check equal? [act] exp m))))
    (eprintf "--------------- ~a\n" t)
    (for ([s scenario*] [i (in-naturals)])
      (match-define (list args expected msg) s)
      (match-define (list equations wallet bank) args)
      (show i equations wallet bank expected)
      (C equations-equal? (λ () (apply useful args)) expected msg)))

  #; {Natural Equations Bag Bag (U Regexp Equations) -> Void}
  (define (show i equations wallet bank expected)
    (eprintf "scenario ~a\n" i)
    (define purple   (colorize (rectangle 10 10) "purple"))
    (define p-given  (show/aux equations))
    (define p-wallet (if (b:bag-empty? wallet) purple (b:render wallet)))
    (define p-bank   (if (b:bag-empty? bank) purple (b:render bank)))
    (define p-expect (if (list? expected) (show/aux expected) (text (~a expected) "roman" 12)))
    (define pl (text "+" "roman" 12))
    (define is (text "---->" "roman" 12))
    (define all (ht-append 5 p-given pl p-wallet pl p-bank is p-expect))
    (pretty-print (frame (inset all 10) #:line-width 4) (current-error-port)))

  #; {Equations -> Pict}
  (define (show/aux equations)
    (for/fold ([r (blank 1 1)]) ([1eq equations])
      (vl-append r (render 1eq))))
    
  (run-scenario* 'for-students ForStudents/)
  (run-scenario* 'tests Tests/)
  (run-scenario* 'exns Exns/ #:check (λ (_ act exp msg) (check-exn exp act msg))))
