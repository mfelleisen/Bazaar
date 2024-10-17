#lang racket

;; bags filled with pebbles 
;; ---------------------------------------------------------------------------------------------------

(provide
 #; {type [Bag X] = [Listof X]}
 ;; X is what the bag contains 

 bag?
 bag
 
 bag-empty?
 bag<?
 
 bag-member?
 bag-size
 subbag?
 bag-add ;; bag ... bag -> bag 
 bag-minus
 bag-intersect
 ;; deterministically pick a pebble from a bag, using the ordering of the colors
 bag-pick
 ;; randomly pick a pebble from a bag 
 bag-pick-random 
 bag-equal?

 random-bag
 
 (contract-out
  [bag-replace-first
   (-> (and/c bag? (compose not bag-empty?)) p:pebble? bag?)]
   
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
  (provide b-rg b-bbbb b-4xb-3xg b-ggg b-r b-g b-ggb b-gw b-rr b-gg b-yyw b-gggg b-b
           ;; for strategies
           b-6g-3r-4b b-2r-2y-1w b-3r-2y-1w b-4r-2y-1w
           ;; for cards 
           b-bbbbb b-ggggg b-rgbrg b-wyrbb b-rrbrr b-rbbbb b-rgggg b-yyrwb b-ywywy b-wgwgw
           b-full))


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

(require (prefix-in p: (submod Bazaar/Common/pebbles examples)))
(require (prefix-in p: Bazaar/Common/pebbles))
(require (prefix-in lib: (only-in Bazaar/Lib/bags render)))
(require Bazaar/Lib/bags)
(require pict)

(module+ test
  (require (submod ".." examples))
  (require (submod ".." json))
  (require SwDev/Testing/check-values)
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
  (define b-gggg [bag p:GREEN p:GREEN p:GREEN p:GREEN])

  ;; usable for cards 
  (define b-rrbrr [bag p:RED p:RED p:BLUE p:RED p:RED])
  (define b-ggggg (bag p:GREEN p:GREEN p:GREEN p:GREEN p:GREEN))
  (define b-wgwgw (bag p:WHITE p:GREEN p:WHITE p:GREEN p:WHITE))
  (define b-ywywy (bag p:YELLOW p:WHITE p:YELLOW p:WHITE p:YELLOW))

  (define b-bbbbb [bag p:BLUE p:BLUE p:BLUE p:BLUE p:BLUE])
  (define b-rgbrg (bag p:RED p:GREEN p:BLUE p:RED p:GREEN))
  (define b-wyrbb (bag p:WHITE p:YELLOW p:RED p:BLUE p:BLUE))
  (define b-rbbbb (bag-add b-bbbb (list p:RED)))
  (define b-rgggg (bag-add b-gggg (list p:RED)))
  (define b-yyrwb (bag p:YELLOW p:YELLOW p:RED p:WHITE p:BLUE))
  
  (define b-yyw [bag p:YELLOW p:YELLOW p:WHITE])
  (define b-ggg [bag p:GREEN p:GREEN p:GREEN])
  (define b-gg  [bag p:GREEN p:GREEN])
  (define b-r   [bag p:RED])
  (define b-rr  [bag p:RED p:RED])
  (define b-g   [bag p:GREEN])
  (define b-b   [bag p:BLUE])
  (define b-ggb [bag p:GREEN p:GREEN p:BLUE])
  (define b-gw  [bag p:RED p:WHITE])
  (define b-4xb-3xg [bag p:BLUE p:BLUE p:BLUE p:BLUE p:GREEN p:GREEN p:GREEN])
  (define b-rg      [bag p:RED p:GREEN])

  (define b-6g-3r-4b (bag-add b-rg b-rg b-rg b-4xb-3xg))
  (define b-2r-2y-1w (bag-add b-rr b-yyw))
  (define b-3r-2y-1w (bag-add b-rr b-r b-yyw))
  (define b-4r-2y-1w (bag-add b-rr b-rr b-yyw))

  (define per (/ PEBBLES# COLOR#))
  (define b-full (apply append (map (λ (c) (apply bag (make-list per c))) p:PEBBLES))))

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

#; {Bag Bag -> Boolean}
;; one bag is below another if the words formed from the sorted first chars are string<=?
(define (bag<? 1bag 2bag)
  (if (= (bag-size 1bag) (bag-size 2bag))
      (string<? (bag->string 1bag) (bag->string 2bag))
      (< (bag-size 1bag) (bag-size 2bag))))

#; {Bag -> String}
;; bag -> list of colors, take first letter, sort, create string
(define (bag->string 1bag)
  (let* ([s 1bag]
         [s (bag->list s)]
         [s (map p:pebble-id s)]
         [s (sort s char<=?)]
         [s (apply string s)])
    s))

(module+ test
  (check-false (bag<? b-ggg b-ggg) "ggg <= ggg")
  (check-true  (bag<? b-ggg b-yyw) "ggg <= yyw")
  (check-false (bag<? b-yyw b-ggg) "yyw !<= ggg"))

;; ---------------------------------------------------------------------------------------------------
#; {Bag Bag Bag Bag -> (vaues Bag Bag)}
;; transfer the content of `left` into `bank` from `wallet` and `right` from `bank` to `wallet` 
(define (bag-transfer wallet bank left right)
  (values (bag-add (bag-minus wallet left) right)
          (bag-add (bag-minus bank right) left)))

(module+ test
  (check-values (bag-transfer b-rg b-rg b-r b-g) b-gg b-rr "transfer"))

;; ---------------------------------------------------------------------------------------------------
#; {Bag -> Pebble}
(define (bag-pick bank)
  (for*/first ([c COLORS] [p bank] #:when (equal? (p:pebble-color p) c))
    p))

(module+ test
  (check-equal? (bag-pick b-yyrwb) p:RED "random pick"))

;; ---------------------------------------------------------------------------------------------------
#; {NEBag -> Bag}
(define (bag-replace-first lhs p)
  (cons p (rest lhs)))

(module+ test
  (check-equal? (bag-replace-first b-rg p:GREEN) b-gg))

;; ---------------------------------------------------------------------------------------------------
(define (random-bag size)
  (build-list size (λ _ (p:random-pebble))))

;; ---------------------------------------------------------------------------------------------------
(define (render b) (lib:render b p:render))

;; ---------------------------------------------------------------------------------------------------

(provide render-in0star-shape)

#; {[List Pebble Pebble Pebble Pebble Pebble] -> Pict}
(define (render-in0star-shape bag-of-5-pebbles)
  (match-define [list p1 p2 p3 p4 p5] (map p:render bag-of-5-pebbles))
  (let* ([pebbles (map p:render bag-of-5-pebbles)]
         [s p1]
         [s (vc-append 20 s (apply hc-append 40 (list p2 p3)))]
         [s (vc-append 20 s (apply hc-append 20 (list p4 p5)))])
    s))
  
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

  (define (jsexpr->bag j)
    (define b (lib:jsexpr->bag j p:pebble-color? jsexpr->pebble))
    (define s (jsexpr->string/ j))
    (cond
      [(false? b)
       #false]
      [(not (pebbles#? (bag-size b)))
       (eprintf "~a : bad contains an more pebbels than allowed: ~a\n" 'jsexpr->bag s)
       #false]
      [else b])))

(module+ test
  (check-true (jsexpr? (bag->jsexpr b-rrbrr)))
  (check bag-equal? (jsexpr->bag (bag->jsexpr b-rrbrr)) b-rrbrr "basic bag test"))
