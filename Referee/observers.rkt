#lang racket

;; .. allow the referee to `send state` messages and `send end` messages to signal that a game is over

;; WARNING: The referee calls methods via `xsend` so any bug is camouflaged (see log)
;;          Also, when the call is done, `xsend` tears down the thread and the call to the observer.

(provide
 ;; visual observer:
 ;; collect game states for viewing and inspection after the game is complete 
 ;; <--    previous state
 ;; -->    next state
 ;; blank  save current state as JSON via file dialog
 ;; "s"    save current state as image via file dialog
 ;; "x"    exit

 ;; from the referee perspective
 #; {class/c
     [state (-> Any Equations Action GameState Void)]
     [end (-> Void)]}
                     
 ;; from the connector's perspective, also 
 #; (class/c
     [save (-> Void)]
     [show (-> Natural)])
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
(require Bazaar/Common/player-interface)
(require (submod Bazaar/Common/equations json))
(require (submod Bazaar/Referee/game-state json))
(require (prefix-in gs: Bazaar/Referee/game-state))

(require 2htdp/universe)
(require (only-in 2htdp/image save-image))
(require (except-in pict rotate circle))
(require json)
(require (only-in racket/gui get-file get-text-from-user))

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

    ;; what the GUI supports 
    (field
     [*commands
      `(["x"     ,(λ (i) -999) "exit"]
        ["s"     ,(λ (i) (save-state third i save-image) i)
                 "save PNG"]
        [" "     ,(λ (i) (save-state
                          (λ (gs) `[,(game->jsexpr (first gs)) ,(equations->jsexpr olde)])
                          i save-json)  i)
                 "save GAME STATE"]
        ["left"  ,(λ (i) (sub1/nat i)) "←"]
        ["right" ,(λ (i) (add1/nat i)) "→"])])
   
    ;; while the game is on-going, this field contains the states in reverse order
    #;{[Listof GameState]}
    (field [*live-list '()])

    ;; when the observer has compeleted its task, this field contains [List GameState Bitmap] in order
    #; {[Option [Vector (U [List GameState Pict] [List GameState Pict BitMap])]]}
    (field [*cache #false])
    (field [*size  -1])
    (field [*eqs  '()])
    (field [olde  '()])
           

    ;; -----------------------------------------------------------------------------------------------
    #; {Any GameState -> Void}
    (define/public (state msg equations action gs)
      (set! *eqs equations)
      (unless *cache (set! *live-list (cons (list msg action gs) *live-list))))

    ;; -----------------------------------------------------------------------------------------------
    #; {-> Natural}
    (define/public (end winners drop-outs)
      (when (cons? *live-list)
        (set! olde *eqs)
        (set! *eqs (e:render* *eqs))
        (define cached-states (reverse *live-list))
        (populate-cache-with-picts cached-states)
        (add-last winners drop-outs)
        (frame-all-to-keep-steady)))

    #; {[Listof GameState] -> Void}
    ;; EFFECT set up *cache with (list GameState ItsPict) except for last slot
    (define/private (populate-cache-with-picts cached-states)
      (set! *size  (add1 (length cached-states)))
      (set! *cache (make-vector *size))
      (for ([s cached-states] [i (in-naturals)])
        (match-define [list msg action gs] s)
        (define the-pict (vl-append 22 [documentation] *eqs (gs:render gs) (explanation msg action)))
        (vector-set! *cache i (list gs the-pict))))

    #; {[Listof String] [Listof String] -> Void}
    ;; EFFECT set up last slot in *cache with (list Result PictOfFinalState) 
    (define (add-last winners drop-outs)
      (define last (vl-append 22 (names 'winners winners) (names "drop outs" drop-outs)))
      (vector-set! *cache (- *size 1) `[,(first (vector-ref *cache (- *size 2))) ,last]))

    #; {Any (U False [Listof Card] [Listof Equation] Any) -> Pict}
    (define/private (explanation msg action)
      (match action
        [#false
         (ht-append 5 (tt (~a msg)) (tt ":") (tt "requesting a pebble"))]
        [(list (? c:card?) ...)
         (ht-append 5 (tt (~a msg)) (tt ":") (tt "[") (c:render* action) (tt "]"))]
        [(list (? e:1eq?) ...)
         (ht-append 5 (tt (~a msg)) (tt ":") (tt "[") (e:render* action) (tt "]"))]
        [_
         (ht-append 5 (tt (~a msg)) (tt ":") (tt (~a action)))]))

    (define/private (tt msg) (text msg "roman" 22))
    (define/private (names tag los) (tt (~a "the final " tag " are: " (string-join los ", "))))

    #; {-> Pict}
    (define/private [documentation]
      (for/fold ([r '()] #:result (tt (~a "valid commands: " (string-join r ", ")))) ([c *commands])
        (match c
          [(list x _) (cons x r)]
          [(list x _ y) (cons (~a y " (" (~s x) ")") r)])))
    
    #; {-> Void}
    ;; EFFECT set up *t-index
    (define/private (frame-all-to-keep-steady)
      (define cache  (vector->list *cache))
      (define width  (min (pict-width (second (argmax (compose pict-width second) cache))) 1900))
      (define height (min (pict-height (second (argmax (compose pict-height second) cache))) 1900))
      (define area   (rectangle (+ width 8) (+  height 8) #:border-color "salmon" #:border-width 2))
      (for ([x (in-vector *cache)] [i (in-naturals)])
        (match-define [list gs pict] x)
        (when (>= (pict-height pict) 2000)
          (pretty-print pict (current-error-port))
          (set! pict (scale pict .2)))
        (define framed (lt-superimpose area (ht-append (blank 4 1) (vl-append (blank 1 4) pict))))
        (vector-set! *cache i (list gs framed))))
      
    ;; -----------------------------------------------------------------------------------------------
    #; {-> Natural}
    (define/public (show)
      (cond
        [(false? *cache) -999]
        [else 
         (big-bang 0
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
    ;; EFFECT cache bitmap once made
    (define/private (display i)
      (match (vector-ref *cache i)
        [(list s p b) b]
        [(list (? gs:game? s) (? pict? p))
         (define b (pict->bitmap p))
         (vector-set! *cache i (list s p b))
         b]))
    
    #; {Natural -> Natural}
    ;; EFFECT for some keystrokes 
    (define/private (back-or-forth i key-event)
      (match (assoc key-event *commands)
        [#false i]
        [(list* ke f extra) (f i)]))

    #; {Natural -> Natural}
    (define/private (add1/nat i)
      (min (sub1 *size) (+ i 1)))

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
      (define/public (state msg equations action gs) (eprintf "calling state ~a\n" name) (/ 1 0))
      (define/public (end winners drop-outs) (eprintf "calling end ~a\n" name) (/ 1 0))))

  (define o (new observer%))
  (define p (new void-observer%))
  (define q (new failing-observer% [name 'q]))
  (define r (new failing-observer% [name 'r])))

#;
(module+ pict
  (apply referee/state (append (first (second 8Simple/)) (list (list q o p q r))))

  ;; uncomment for interactive tests 
  #;
  (send o show))

(module+ pict

  (run-scenario-with-observer 9Simple/ 3 o)

  #;
  (run-scenario-with-observer sey 1 o p:player-award-seychelles-bonus)

  #;
  (run-scenario-with-observer rwb 5 o p:player-award-red-white-and-blue-bonus)
  (send o show))

