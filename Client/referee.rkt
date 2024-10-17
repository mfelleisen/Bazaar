#lang racket

;; the proxy referee runs a player in the same context as a referee proper

(require (prefix-in json: (only-in json jsexpr->string jsexpr?)))

(define create-reply/c   (-> (or/c eof-object? json:jsexpr?) (or/c json:jsexpr? broken?)))
(define receiver/c       (-> create-reply/c any))
(define proxy-referee/c  (-> receiver/c custodian? (-> player/c any)))

;; the `proxy-referee` for a player is a function that
;; -- repeatedly receives JSexpr and turns them into arguments so that it can 
;; -- call the appropriate method in the given player and then
;; -- turn the result into a JSexpr that can be sent back 
;; 
;; the `receiver` is supposed to be a function that handles the side of a remote-call interaction 
;; -- its argument is called on the received JSON or EOF turned into JSxpr or EOF
;;    and its result is what the `receiver` turns back into a remote reply
;;    [I have developed a library that sets up both a sender and a receiver.]
;;    
;; `create-reply` is the best name I could come up with for the argument of the `receiver`

(provide
 proxy-referee/c
 
 (contract-out
  
  [create-proxy-referee
   #; (create-proxy-referee name port ip quiet? rm)
   #; (create-proxy-referee name port ip quiet? rm sender-routine)
   ;; the resulting proxy referee gets connected to the server with `name`
   ;; using the given proxy-referee; output to error is managed
   ;; the optional sender routine allows sending the name in a weird maner 
   (->* (string? port/c string? boolean? proxy-referee/c) (any/c) [-> any/c])]
  
  [connect-and-run
   #; {[-> ProxyReferee] Player OutputPort -> Void}
   ;; connect the Proxy to the server and the player, then start it up
   (-> (-> any/c) any/c output-port? any)]
  
  [default-proxy-ref-maker proxy-referee/c]
  
  (pick-referee
   ;; pick a proxy-referee given the string (usually the name of a player)
   (-> string? (or/c #false proxy-referee/c)))
  
  ;; for xtranslate only 
  [*referee-list (listof (list/c string? proxy-referee/c))]))

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

(require Bazaar/Remote/define-dispatcher)

(require (submod Bazaar/Common/actions json))
(require Bazaar/Common/player-interface)

(require Bazaar/Player/mechanism)

(require SwDev/Testing/make-client)
(require SwDev/Testing/communication)

;; ---------------------------------------------------------------------------------------------------
;; imports for the macro definition

(require (submod Bazaar/Common/cards json))
(require (submod Bazaar/Common/equations json))
(require (submod Bazaar/Common/turn-state json))

(require Bazaar/Common/actions)
(require Bazaar/Common/equations)
(require (except-in Bazaar/Common/turn-state render))
(require (except-in Bazaar/Common/cards render render*))

(require Bazaar/Lib/json)

(require (for-syntax syntax/parse))
(require (for-syntax racket/format racket/string))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))

  (require (submod Bazaar/Common/equations examples))
  (require (submod Bazaar/Common/turn-state examples))
  (require (except-in (submod Bazaar/Referee/game-state examples) ForStudents/ Tests/))
  
  (require Bazaar/Player/strategies)
  
  (require SwDev/Lib/should-be-racket)
  (require rackunit))

;                                                                               
;                                                        ;                      
;                                                        ;            ;         
;                                                        ;                      
;   ;;;;    ;;;;   ;;;   ;   ;  ;   ;         ;;;;    ;;;; ;;;;;;   ;;;   ; ;;  
;   ;; ;;   ;;  ; ;; ;;   ; ;   ;   ;             ;  ;; ;; ;  ;  ;    ;   ;;  ; 
;   ;   ;   ;     ;   ;   ;;;    ; ;              ;  ;   ; ;  ;  ;    ;   ;   ; 
;   ;   ;   ;     ;   ;    ;     ; ;           ;;;;  ;   ; ;  ;  ;    ;   ;   ; 
;   ;   ;   ;     ;   ;   ;;;    ; ;          ;   ;  ;   ; ;  ;  ;    ;   ;   ; 
;   ;; ;;   ;     ;; ;;   ; ;    ;;           ;   ;  ;; ;; ;  ;  ;    ;   ;   ; 
;   ;;;;    ;      ;;;   ;   ;    ;            ;;;;   ;;;; ;  ;  ;  ;;;;; ;   ; 
;   ;                             ;                                             
;   ;                            ;                                              
;   ;                           ;;

(define *sender (λ (name ip) (send-message name ip)))
(define [(create-proxy-referee name port# ip quiet remote-referee (sender *sender))]
  (with-handlers ([exn:fail:network? (λ (xn) (eprintf "fail! ~a\n" name) (λ (_) 'failed-connection))])
    (define-values (receiver custodian)
      (connect-to-server-as-receiver ip port# #:init (λ (ip) (sender name ip))))
    (remote-referee receiver custodian)))

(define (connect-and-run proxy-referee 1player error-port)
  (parameterize ([prefix-with-spaces 5000]
                 [current-error-port error-port]
                 [trickle-output?    #true])
    ([proxy-referee] 1player)))

;; ---------------------------------------------------------------------------------------------------
(define *referee-list '())

(define (pick-referee x)
  (define r (assoc x *referee-list))
  (and r (second r)))

(define-syntax (def-and-add-rm stx)
  (syntax-parse stx
    [(_ name:id add?
        (~optional (~seq #:setup     ret-setup:id) #:defaults ([ret-setup #'void]))
        ;; both request-189-trades and request cards are turns:
        (~optional (~seq #:take-turn ret-tt:id)    #:defaults ([ret-tt    #'action]))
        (~optional (~seq #:win       ret-win:id)   #:defaults ([ret-win   #'void])))
     #:do [[define name-as-string (string-replace (~a (syntax-e #'name)) "make-" "")]]
     #`(begin
         (define-remote-proxy-context name #;for player%

           ;; eventually this macro needs to check that these IDs are method names
           ;; supply the class name and check list membership? 
           #; (interface->method-names (class->interface player%))
           
           [[setup equations]               ret-setup]
           [[request-pebble-or-trades turn] ret-tt]
           [[request-cards turn]            ret-tt]
           ;; when the last clause matches,
           ;; the dispatcher signals the end of the cycle by setting done? to #true
           [[win boolean]                   ret-win])
         (when add?
           (set! *referee-list (cons [list #,name-as-string name] *referee-list))))]))

(define RPT "request-pebble-or-trades")

(def-and-add-rm default-proxy-ref-maker #false) ;  #:setup normal)
(def-and-add-rm make-nonSetup       #true  #:setup     non-json-void)
(def-and-add-rm make-illTT          #true  #:take-turn ill-formed-action)
(def-and-add-rm make-invTTBadString #true  #:take-turn invalidate-action)
(def-and-add-rm make-nonWin         #true  #:win       non-json-void)

(define (normal->jsexpr a) (eprintf "noremal ~a\n" a) (void->jsexpr a))

(define (ill-formed-action->jsexpr x)
  (define y (json:jsexpr->string (action->jsexpr x)))
  [broken (~a (make-string 4095 #\space) (substring y 0 (sub1 (string-length y))))])

(require SwDev/Debugging/spy)

#; {Action -> JSExpr}
(define (invalidate-action->jsexpr a)
  (match a
    [(? false?) #true]
    [(cons (? 1eq? one) others)
     (cons (spy (ill-formed-1eq one)) (equations->jsexpr others))]
    [(cons (? card? one) others)
     (cons (spy (ill-formed-card one)) (card*->jsexpr others))]
    [_ (spy [list [list [list "re"] [list "red"]]])]))

#; {1Eq -> JESexpr}
;; duplicate LHS and add at end 
(define (ill-formed-1eq eq)
  (match (1eq->jsexpr eq)
    [(list lhs rhs) `[,lhs ,rhs ,lhs]]))

#; {Card -> JSExpr}
;; deconstruct JSExpr and misspell field name 
(define (ill-formed-card one)
  (match (card->jsexpr one)
    [(hash-table [(== PEBBLES) p] [(== FACE) f])
     (define pebbles-misspelled (string->symbol (substring (~a PEBBLES) 1 2)))
     (hash pebbles-misspelled p FACE f)]))

(define (non-json-void->jsexpr a)
  [broken "void"])


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


(module+ test ;; test individual remote calls
  #; {JSexpr -> JSexpr}
  (define (run j [rm0 #false] #:the-player (p [plr1]))
    (dev/null
     (let/cc escape
       (define rm (if rm0 [rm0 escape j] [[mk-rm default-proxy-ref-maker] escape j]))
       (rm p))))
  (define [(mk-rm f) escape j] (f [mk-escape-dispatcher escape j] (make-custodian)))
  (define [[mk-escape-dispatcher escape j] dispatcher] (escape (dispatcher j)))

  (define [plr1] (create-player "bye"))
  (define jeq1 (equations->jsexpr (list r=gggg)))
  (define jts0 (turn->jsexpr ts0))
  (define jts1 (turn->jsexpr ts-3-zeros)))

;; ---------------------------------------------------------------------------------------------------
(module+ test  ;; test individual remote calls
  (check-false [run eof])
  (check-equal? (run `["setup" [,jeq1]]) "void" "setup")
  (check-equal? (run `["request-pebble-or-trades" [,jts0]]) #false "pebble or trade")
  (check-equal? (run `["request-cards" [,jts0]]) '[] "pebble or trade")
  (check-equal? (run `["win" [#t]]) "void" "win tt")
  (check-equal? (run `["win" [#f]]) "void" "win ff"))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; winning call? 
  (define rm (default-proxy-ref-maker (λ (f) (f `["win" [#true]])) (make-custodian)))
  (check-equal? (dev/null (rm [plr1])) #true))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; check an entire sequence of calls, ending in a winning call 
  (define b*
    `[["setup" [,jeq1]]
      ["request-pebble-or-trades" [,jts0]]
      ["request-cards" [,jts0]]
      ["win" [#true]]])
  (define chop! (λ (f) (begin0 (f (first b*)) (set! b* (rest b*)))))
  (define rm+   (default-proxy-ref-maker chop! (make-custodian)))
  (check-equal? (dev/null (rm+ [plr1])) #t))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; checks that the proxy-referee shuts down the given custodian if the JSON is bad
  (check-true
   (let ([cust (make-custodian)])
     (parameterize ([current-custodian cust])
       (define port (open-input-file "referee.rkt"))
       (dev/null ((default-proxy-ref-maker (λ (f) (f `[0 [#t]])) cust) [plr1]))
       (port-closed? port)))))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; test all the mostly broken proxy-referees
  (define nonSetup (pick-referee "nonSetup"))
  (check-pred broken? [run `["setup" [,jeq1]] [mk-rm nonSetup]])

  (let ([illTT (pick-referee "illTT")])
    (check-pred broken? (run `["request-pebble-or-trades" [,jts0]] [mk-rm illTT])))

  (let ([r (pick-referee "invTTBadString")])
    ;; would request a pebble so it return #true instead 
    (check-false (action? (run `["request-pebble-or-trades" [,jts1]] [mk-rm r])))
    ;; would request a card but it breaks the card format 
    (check-false (action? (run `["request-cards" [,jts1]] [mk-rm r]))))

  (let ([nonWin (pick-referee "nonWin")])
    (check-pred broken? (run `["win" [#true]] [mk-rm nonWin]))))
