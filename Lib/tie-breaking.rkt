#lang racket

;; collect candidates and realize tie-breaking functionality

(provide
 (contract-out
  [collector%
   ;; allow the collection of candidates with maximal scores 
   {class/c
    [init          {e0 #;(X) any/c}]
    [init-field    {score (-> #;(X) any/c real?)}
                   (break   (-> (listof #;{X} any/c) any/c))]
    [done          (->m #;(X) any/c)]
    [add-if-better (->m #;(X) any/c void?)]}]
  
  [pick-smallest
   ;; sort `l` with `<` with key `s` 
   ;; take all elements from the front whose `s` elements are `equal?`
   ;; ensure that the remainder is "above" 
   (->i ([l (and/c cons? (listof #;{X} any/c))]
         [< (-> #;{Y} any/c #;{Y} any/c boolean?)]
         [s (-> #;{X} any/c #;{Y} any/c)]
         [? any/c])
        (r (and/c cons? (listof any/c))))]
  
  [tie-breaker
   #; (tie-breaker (f1 ... fN) lox)
   ;; apply the fj-s to lox until 
   #; (fi ... (f1 lox) ...)
   ;; yields singleton list;
   ;; a singleton list or a failure is passed on to the fail continuation `fk` if it exists
   ;; otherwise error 
   (->i ([metrics (listof (-> any/c any))]
         [lox     (listof any/c)])
        (#:continue (continue any/c #; (or/c false? (-> (listof any/c) any))))
        #:pre (lox) (cons? lox)
        (r any/c))]))

;                                                                               
;                               ;                           ;                   
;     ;       ;                 ;                           ;                   
;     ;                         ;                           ;                   
;   ;;;;;   ;;;    ;;;          ;;;;    ;;;;   ;;;   ;;;;   ;  ;    ;;;    ;;;; 
;     ;       ;   ;;  ;         ;; ;;   ;;  ; ;;  ;      ;  ;  ;   ;;  ;   ;;  ;
;     ;       ;   ;   ;;        ;   ;   ;     ;   ;;     ;  ; ;    ;   ;;  ;    
;     ;       ;   ;;;;;;        ;   ;   ;     ;;;;;;  ;;;;  ;;;    ;;;;;;  ;    
;     ;       ;   ;             ;   ;   ;     ;      ;   ;  ; ;    ;       ;    
;     ;       ;   ;             ;; ;;   ;     ;      ;   ;  ;  ;   ;       ;    
;     ;;;   ;;;;;  ;;;;         ;;;;    ;      ;;;;   ;;;;  ;   ;   ;;;;   ;    
;                                                                               
;                                                                                
;                                                                               

#; {[Listof ([NEList X] -> [NEList X])] [NEListof X] [#:continue Boolean] -> (U X [NEList X])}
;; if fk, returns a list of X
;; if not fk, returns an X 
(define (tie-breaker f*0 lox0 #:continue (fk #false))
  (let while ([lox lox0] [f* f*0])
    (match lox
      [(list one) (if fk lox one)]
      [_ (cond
           [(empty? f*) (if fk lox (show-tie-breaking-problem f*0 lox0 lox))]
           [else (while ((first f*) lox) (rest f*))])])))

;                                                                                             
;                        ;                                                                    
;             ;          ;                                  ;;;    ;;;                    ;   
;                        ;                                    ;      ;                    ;   
;   ;;;;    ;;;    ;;;   ;  ;           ;;;  ;;;;;;  ;;;;     ;      ;     ;;;    ;;;   ;;;;; 
;   ;; ;;     ;   ;;  ;  ;  ;          ;   ; ;  ;  ;     ;    ;      ;    ;;  ;  ;   ;    ;   
;   ;   ;     ;   ;      ; ;           ;     ;  ;  ;     ;    ;      ;    ;   ;; ;        ;   
;   ;   ;     ;   ;      ;;;            ;;;  ;  ;  ;  ;;;;    ;      ;    ;;;;;;  ;;;     ;   
;   ;   ;     ;   ;      ; ;               ; ;  ;  ; ;   ;    ;      ;    ;          ;    ;   
;   ;; ;;     ;   ;;     ;  ;          ;   ; ;  ;  ; ;   ;    ;      ;    ;      ;   ;    ;   
;   ;;;;    ;;;;;  ;;;;  ;   ;          ;;;  ;  ;  ;  ;;;;     ;;     ;;   ;;;;   ;;;     ;;; 
;   ;                                                                                         
;   ;                                                                                         
;   ;                                                                                         

(define (pick-smallest lox < selector fk)
  (define sorted     (sort lox < #:key selector))
  (define one        (selector (first sorted)))
  (define all-equals (takef sorted (λ (x) (equal? (selector x) one))))
  (define remainder  (drop sorted (length all-equals)))
  (cond
    [(or fk (empty? remainder) (< one (selector (first remainder)))) all-equals]
    [else (show-tie-breaking-problem '() lox [list 'all: all-equals 'rem: remainder])]))

;                                                                 
;                                                                 
;                 ;;;    ;;;                    ;                 
;                   ;      ;                    ;                 
;    ;;;    ;;;     ;      ;     ;;;    ;;;   ;;;;;   ;;;    ;;;; 
;   ;;  ;  ;; ;;    ;      ;    ;;  ;  ;;  ;    ;    ;; ;;   ;;  ;
;   ;      ;   ;    ;      ;    ;   ;; ;        ;    ;   ;   ;    
;   ;      ;   ;    ;      ;    ;;;;;; ;        ;    ;   ;   ;    
;   ;      ;   ;    ;      ;    ;      ;        ;    ;   ;   ;    
;   ;;     ;; ;;    ;      ;    ;      ;;       ;    ;; ;;   ;    
;    ;;;;   ;;;      ;;     ;;   ;;;;   ;;;;    ;;;   ;;;    ;    
;                                                                 
;                                                                 
;                                                                 

#; {class (X)
     [init {e0 X}]
     [init-field {score (X -> Real)}]
     [done (->m [Listof X])]
     [add-if-better (->m X Void)]}
;; collect the best elements according to `score` 
(define collector%
  (class object% (init e0) (init-field score) (init-field break)
    (field [*best-score (score e0)])
    (field [*possibles [set e0]])
    (super-new)
    
    (define/public (done)
      (define l (set->list *possibles))
      (match l
        [(list ex) ex]
        [_ (break l)]))

    (define/public (add-if-better e1)
      (define v1 (score e1))
      (cond
        [(> v1 *best-score)
         (set!-values (*best-score *possibles) (values v1 `[,e1]))]
        [(= v1 *best-score)
         (set! *possibles (set-add *possibles e1))]
        [else (void)]))))

;                                     
;                                     
;                                     
;                                     
;    ;;;    ;;;;   ;;;;   ;;;    ;;;; 
;   ;;  ;   ;;  ;  ;;  ; ;; ;;   ;;  ;
;   ;   ;;  ;      ;     ;   ;   ;    
;   ;;;;;;  ;      ;     ;   ;   ;    
;   ;       ;      ;     ;   ;   ;    
;   ;       ;      ;     ;; ;;   ;    
;    ;;;;   ;      ;      ;;;    ;    
;                                     
;                                     
;                                     

#; {[Listof ([NEList X] -> [NEList X])] [NEListof X] [NEListof X] -> Void}
;; explain an error when sorting or tie breaking goes wrong 
(define (show-tie-breaking-problem f*0 lox0 lox)
  (define p-f*0  (map object-name f*0))
  (define p-lox0 (with-output-to-string (λ () (pretty-print lox0))))
  (define p-lox  (with-output-to-string (λ () (pretty-print lox))))
  (define N      (length lox))
  [error 'tie-breaker "~a left over:\n ~a\n given ~a and ~a\n" N p-lox p-f*0 p-lox0])