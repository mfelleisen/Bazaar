#lang racket

;; .. allow the referee to `send state` messages and `send end` messages to signal that a game is over

;; WARNING: The referee calls methods via `xsend` so any bug is camouflaged (see log)
;;          Also, when the call is done, `xsend` tears down the thread and the call to the observer.

(provide
 void-observer%

 ;; visual observer:
 ;; collect game states for viewing and inspection after the game is complete 
 ;; <--    previous state
 ;; -->    next state
 ;; blank  save current state as JSON via file dialog
 ;; "s"    save current state as image via file dialog
 ;; "x"    exit

 ;; from the referee perspective
 #; {class/c [state (-> Any GameState Void)] [end (-> Void)]}
 ;; from the connector's perspective, also 
 #; (class/c [save (-> Void)] [show (-> Natural)])
 observer%)

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

(require Bazaar/Referee/game-state)
(require (submod Bazaar/Referee/game-state json))
(require 2htdp/universe)
(require (only-in 2htdp/image save-image))
(require (except-in pict rotate circle))
(require json)
(require (only-in racket/gui make-eventspace current-eventspace get-file get-text-from-user))

;                              
;                            ; 
;                    ;       ; 
;                            ; 
;   ;   ;   ;;;    ;;;    ;;;; 
;   ;   ;  ;; ;;     ;   ;; ;; 
;    ; ;   ;   ;     ;   ;   ; 
;    ; ;   ;   ;     ;   ;   ; 
;    ; ;   ;   ;     ;   ;   ; 
;     ;    ;; ;;     ;   ;; ;; 
;     ;     ;;;    ;;;;;  ;;;; 
;                              
;                              
;                              

(define void-observer%
  (class object%
    (super-new)
    (define/public (state msg gs) (eprintf "state ~a\n" msg))
    (define/public (end winners drop-outs) (eprintf "the end:\n ~a\n ~a\n" winners drop-outs))))

;                                            
;                                            
;             ;                        ;;;   
;                                        ;   
;   ;   ;   ;;;    ;;;   ;   ;  ;;;;     ;   
;   ;   ;     ;   ;   ;  ;   ;      ;    ;   
;    ; ;      ;   ;      ;   ;      ;    ;   
;    ; ;      ;    ;;;   ;   ;   ;;;;    ;   
;    ; ;      ;       ;  ;   ;  ;   ;    ;   
;     ;       ;   ;   ;  ;   ;  ;   ;    ;   
;     ;     ;;;;;  ;;;    ;;;;   ;;;;     ;; 
;                                            
;                                            
;                                            

;; an observer that can deal with a single game:
;; -- the referee uses `end` and `state` to send the sequence of states during a game 
;; -- after it sends `end` the observer can be called with `show`,
;;    which displays the collected states in a separate window
;; -- ... the observer can be called with `save`,
;;    which saves all states as bitmaps in Tmp/ .. as pngs 

(define observer%
  (class object%
    (super-new)
   
    ;; while the game is on-going, this field contains the states in reverse order
    #;{[Listof GameState]}
    (field [*live-list '()])

    ;; when the observer has compeleted its task, this field contains [List GameState Bitmap] in order
    #; {[Option [Vector (U [List GameState Pict] [List GameState Pict BitMap])]]}
    (field [*cache      #false])
    
    #; {Any GameState -> Void}
    (define/public (state msg gs)
      (unless *cache (set! *live-list (cons (list msg gs) *live-list))))

    #; {-> Natural}
    (define/public (end winners drop-outs)
      (when (cons? *live-list)
        (define cached-states (reverse *live-list))
        (define tallest 0)
        (set! *c-size (length cached-states))
        (set! *cache (make-vector (add1 *c-size)))
        (for ([s cached-states] [i (in-naturals)])
          (match-define [list msg gs] s)
          (define gs-as-pict  (render gs))
          (define msg-as-pict (tt (~a msg)))
          (define the-pict (vl-append 22 gs-as-pict msg-as-pict))
          (define height (pict-height the-pict))
          (when (> height tallest) (set!-values [tallest *t-index] (values height i)))
          (vector-set! *cache i (list gs the-pict)))
        (define last (vl-append 22 (names 'winners winners) (names "drop outs" drop-outs)))
        (vector-set! *cache *c-size `[,(first (vector-ref *cache (- *c-size 2))) ,last])))

    (define/private (tt msg) (text msg "roman" 22))
    (define/private (names tag los) (tt (~a "the final " tag " are: " (string-join los ", "))))

    (field [*t-index 0])
    (field [*c-size  -1])
    ;; -----------------------------------------------------------------------------------------------
    #; {-> Natural}
    (define/public (show)
      (cond
        [(false? *cache) -999]
        [else 
         (big-bang *t-index
           [close-on-stop #true]
           [to-draw       (λ (x) (display x))]
           [stop-when     negative?]
           [on-key        (λ (x ke) (back-or-forth x ke))])]))

    #; {-> Void}
    ;; saves all states as bitmaps in Tmp/..png files 
    (define/public (save)
      (define tmp Tmp/)
      (when (directory-exists? tmp) (delete-directory/files tmp))
      (make-directory tmp)
      (parameterize ([current-directory tmp])
        (for ([x (in-vector *cache)] [i (in-naturals)])
          (define bm (display i))
          (save-image bm (~a i ".png")))))
        
    #; {Natural -> Bitmap}
    (define/private (display i)
      (match (vector-ref *cache i)
        [(list s p b) b]
        [(list (? game? s) (? pict? p))
         (define b (pict->bitmap (vl-append 22 (tt documentation) p)))
         (vector-set! *cache i (list s p b))
         b]))
    
    (define commands
      `(["x"     ,(λ (i) -999) "exit"]
        ["s"     ,(λ (i) (save-state third i save-image) i) "save PNG"]
        [" "     ,(λ (i) (save-state (compose game->jsexpr first) i save-json)  i) "save GAME STATE"]
        ["left"  ,(λ (i) (sub1/nat i)) "←"]
        ["right" ,(λ (i) (add1/nat i)) "→"]))
    (define documentation
      (for/fold ([r '()] #:result (~a "valid commands: " (string-join r ", "))) ([c commands])
        (match c
          [(list x _) (cons x r)]
          [(list x _ y) (cons (~a y " (" (~s x) ")") r)])))

    #; {Natural -> Natural}
    ;; EFFECT for some keystrokes 
    (define/private (back-or-forth i key-event)
      (match (assoc key-event commands)
        [#false i]
        [(list* ke f extra) (f i)]))

    #; {Natural -> Natural}
    (define/private (add1/nat i)
      (min *c-size (+ i 1)))

    #; {Natural -> Natural}
    (define/private (sub1/nat i)
      (max 0 (sub1 i)))
    
    #; {[RefState -> X] [X -> Void] Natural -> Void}
    ;; EFFECT use 'state->` to render the `i`th state; then ask user to select file & save with write
    (define/private (save-state state-> i write)
      (define state0 (vector-ref *cache i))
      (define output (state-> state0)) 
      (define file   (or (get-file) (get-text-from-user "new json file" "specify a file")))
      (when file (write output file)))))

#; {JSexpr PathString -> Void}
(define (save-json output file)
  (with-output-to-file file #:exists 'replace (λ () (write-json output))))

;                                                                                      
;                                                           ;                          
;          ;;;                                   ;     ;    ;                ;     ;   
;            ;                                         ;    ;                      ;   
;   ;;;;     ;    ;;;;   ;   ;        ;     ;  ;;;   ;;;;;  ; ;;           ;;;   ;;;;; 
;   ;; ;;    ;        ;  ;   ;        ;     ;    ;     ;    ;;  ;            ;     ;   
;   ;   ;    ;        ;   ; ;          ; ; ;     ;     ;    ;   ;            ;     ;   
;   ;   ;    ;     ;;;;   ; ;          ; ; ;     ;     ;    ;   ;            ;     ;   
;   ;   ;    ;    ;   ;   ; ;          ;; ;;     ;     ;    ;   ;            ;     ;   
;   ;; ;;    ;    ;   ;   ;;           ;; ;;     ;     ;    ;   ;            ;     ;   
;   ;;;;      ;;   ;;;;    ;            ; ;    ;;;;;   ;;;  ;   ;          ;;;;;   ;;; 
;   ;                      ;                                                           
;   ;                     ;                                                            
;   ;                    ;;                                                            

(module+ pict
  (require (submod "referee.rkt" examples))
  (require "referee.rkt")

  (define failing-observer%
    (class object% [init-field name]
      (super-new)
      (define/public (state msg gs) (eprintf "calling state ~a\n" name) (/ 1 0))
      (define/public (end winners drop-outs) (eprintf "calling end ~a\n" name) (/ 1 0))))
  
  (define o (new observer%))
  (define p (new void-observer%))
  (define q (new failing-observer% [name 'q]))
  (define r (new failing-observer% [name 'r]))

  (apply referee/state (append (first (second 8Simple/)) (list (list q o p q r))))

  '----
  (send o show))
