#lang racket

;; collect candidates and realize tie-breaking functionality

(provide
 (contract-out
  [collector%
   {class/c
     [init          {e0 any/c}]
     [init-field    {score (-> any/c real?)}]
     [done          (->m [listof any/c])]
     [add-if-better (->m any/c void?)]}]
  
  [tie-breaker
   #; (tie-breaker (f1 ... fN) lox)
   ;; apply the fj-s to lox until 
   #; (fi ... (f1 lox) ...)
   ;; yields singleton list;
   ;; a singleton list or a failure is passed on to the fail continuation `fk` if it exists
   ;; otherwise error 
   (->i ([metrics (listof (-> any/c any))]
         [lox     (listof any/c)])
        (#:continue (continue (or/c false? (-> (listof any/c) any))))
        #:pre (lox) (cons? lox)
        (r any/c))]
  
  [take-all-equals
   ;; sort `l` according to `<` & take all elements from the front whose `sel` elements are `equal?`
   (->i ([l (and/c cons? (listof any/c))]
         [< (-> any/c any/c boolean?)]
         [s (-> any/c any/c)]) (r (and/c cons? (listof any/c))))]))

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

#; {[nEListof (X -> Real)] [NEListof X] -> X}
(define (tie-breaker f*0 lox0 #:continue (fk #false))
   (let while ([lox lox0] [f* f*0])
     (match lox
       [(list one) (if fk (fk lox) one)]
       [_ (cond
            [(empty? f*) (if fk (fk lox) (show-tie-breaking-problem f*0 lox0 lox))]
            [else (while ((first f*) lox) (rest f*))])])))

(define (show-tie-breaking-problem f*0 lox0 lox)
  (define p-f*0  (map object-name f*0))
  (define p-lox0 (with-output-to-string (λ () (pretty-print lox0))))
  (define p-lox  (with-output-to-string (λ () (pretty-print lox))))
  (define N      (length lox))
  [error 'tie-breaker "~a left over:\n ~a\n given ~a and ~a\n" N p-lox p-f*0 p-lox0])

#; {[NEListof X] [X -> Y] -> [NEListof X]}
(define (take-all-equals lox0 < selector)
  (define lox (sort lox0 < #:key selector))
  (define one (selector (first lox)))
  (takef lox (λ (x) (equal? (selector x) one))))


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
  (class object% (init e0) (init-field score)
    (field [*best-score (score e0)])
    (field [*possibles [set e0]])
    (super-new)
    
    (define/public (done)
      (set->list *possibles))

    (define/public (add-if-better e1)
      (define v1 (score e1))
      (cond
        [(> v1 *best-score)
         (set!-values (*best-score *possibles) (values v1 `[,e1]))]
        [(= v1 *best-score)
         (set! *possibles (set-add *possibles e1))]
        [else (void)]))))