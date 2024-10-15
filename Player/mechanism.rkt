#lang racket

;; a class representation of the player mechanism, including subclasses whose methods
;; -- raise exceptions when a method is called
;; -- cheat during a turn after being called n times 
;; -- go into infinite loops after being called n times
;; and behave normally otherwise 

(define player-factory (-> string? any/c player/c))
(define factory-table  (listof (list/c string? player-factory)))

(provide
 player% ;; for reflection 

 (contract-out
  [create-player
   #; (create-player name which)
   ;; create a player object from a name and an evaluation function
   ;; it uses the player factory, if provided, to create baddies
   (->* (string?) ((-> purchase? natural?) #:bad player-factory) player/c)]

  [retrieve-factory
   (-> string? factory-table player-factory)]

  [good-for-6
   ;; plain old creator -- essentially create-player
   factory-table]

  [exn-raising-table-for-7
   ;; raise exns for specific methods 
   factory-table]

  [cheater-table-for-8
    ;; cheat! 
    factory-table]
  [ACHEAT string?]

  [infinite-loop-table-for-9
   ;; go into infinite loops after n calls 
   factory-table]
  [Count# natural?])

 all-cheater-classes)

;; ---------------------------------------------------------------------------------------------------

(module+ examples
  (provide sample))

(module+ json
  (provide actor->jsexpr jsexpr->actor actor*->jsexpr jsexpr->actor* lax-names-okay?))

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

(require (only-in (submod Bazaar/Common/cards examples) c-ggggg ALL-CARDS))
(require Bazaar/Player/strategies)
(require Bazaar/Common/actions)
(require (prefix-in b: Bazaar/Common/bags))
(require (prefix-in c: Bazaar/Common/cards))
(require (prefix-in e: Bazaar/Common/equations))
(require (prefix-in p: Bazaar/Common/player))
(require (prefix-in t: Bazaar/Common/turn-state))
(require Bazaar/Common/player-interface)
(require Bazaar/Common/turn-state)

(require (submod Bazaar/Player/strategies json))

(require (for-syntax syntax/parse))

(module+ json
  (require (submod ".."))
  (require Bazaar/Lib/parse-json))

(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod ".." json))
  (require (submod Bazaar/Common/equations examples))
  (require (submod Bazaar/Common/turn-state examples))
  (require Bazaar/Lib/check-message)
  (require SwDev/Lib/should-be-racket)
  (require racket/sandbox)
  (require rackunit))

;                                                   
;                                                   
;          ;;;                                      
;            ;                               ;;;    
;   ;;;;     ;    ;;;;   ;   ;   ;;;    ;;;; ;  ;   
;   ;; ;;    ;        ;  ;   ;  ;;  ;   ;;  ;;;;   ;
;   ;   ;    ;        ;   ; ;   ;   ;;  ;       ;;; 
;   ;   ;    ;     ;;;;   ; ;   ;;;;;;  ;     ;; ;; 
;   ;   ;    ;    ;   ;   ; ;   ;       ;       ;  ;
;   ;; ;;    ;    ;   ;   ;;    ;       ;       ;  ;
;   ;;;;      ;;   ;;;;    ;     ;;;;   ;        ;; 
;   ;                      ;                        
;   ;                     ;                         
;   ;                    ;;                         

;; this player serves as the base (like a common abstract class) for implementing several variants 
(define player%
  (class object% 
    (init-field
     [my-name  #; String "Adam"]
     [which    #; {Purchase -> Natural} purchase-size])

    (field [strategy (new strategy% [equations '()] [which which])])
    (field [equations '()])
    
    (super-new)

    (define/public (name)
      my-name)
    
    (define/public (description)
      `[,my-name ,(policy->jsexpr which)])

    (define/public (reset)
      (set! *to-be-bought #false)
      (void))

    (define/public (setup equations0)
      (reset)
      (set! strategy (new strategy% [equations equations0] [which which]))
      (set! equations equations0))
    
    #; {Turn -> [Option Equation*]}
    ;; #false denotes a request for a random bebble from bank
    #; (list 1eq ...) ; denotes a sequence of left-to-right exchanges 
    (define/public (request-pebble-or-trades t)
      (cond
        [(send strategy should-the-player-request-a-random-pebble t)
         want-pebble]
        [else
         (define trades&buys (send strategy trade-then-purchase t))
         (set! *to-be-bought (exchange-cards trades&buys))
         (exchange-trades trades&buys)]))
    
    (define *to-be-bought #false) #; (U False [Listof Card])

    #; {Turn -> [Listof Card]}
    ;; the catds that the player wishes to buy, in order 
    (define/public (request-cards t)
      (when (boolean? *to-be-bought)
        (set! *to-be-bought (send strategy buy-cards t)))
      (begin0
        *to-be-bought
        (set! *to-be-bought #false)))

    (define/public (win b)
      (eprintf "~a ~a\n" my-name (if b "won" "lost"))
      (void))))

;                                                          
;                                                          
;                                      ;;;                 
;                                        ;                 
;    ;;;   ;   ;  ;;;;  ;;;;;;  ;;;;     ;     ;;;    ;;;  
;   ;;  ;   ; ;       ; ;  ;  ; ;; ;;    ;    ;;  ;  ;   ; 
;   ;   ;;  ;;;       ; ;  ;  ; ;   ;    ;    ;   ;; ;     
;   ;;;;;;   ;     ;;;; ;  ;  ; ;   ;    ;    ;;;;;;  ;;;  
;   ;       ;;;   ;   ; ;  ;  ; ;   ;    ;    ;          ; 
;   ;       ; ;   ;   ; ;  ;  ; ;; ;;    ;    ;      ;   ; 
;    ;;;;  ;   ;   ;;;; ;  ;  ; ;;;;      ;;   ;;;;   ;;;  
;                               ;                          
;                               ;                          
;                               ;                          

(module+ examples
  (define sample (new player%)))

;                                                                        
;                            ;                                           
;                            ;           ;                    ;          
;                            ;           ;                    ;          
;    ;;;;   ;;;    ;;;    ;;;;         ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;   ;;  ;  ;; ;;  ;; ;;  ;; ;;           ;    ;;  ;  ;   ;    ;    ;   ; 
;   ;   ;  ;   ;  ;   ;  ;   ;           ;    ;   ;; ;        ;    ;     
;   ;   ;  ;   ;  ;   ;  ;   ;           ;    ;;;;;;  ;;;     ;     ;;;  
;   ;   ;  ;   ;  ;   ;  ;   ;           ;    ;          ;    ;        ; 
;   ;; ;;  ;; ;;  ;; ;;  ;; ;;           ;    ;      ;   ;    ;    ;   ; 
;    ;;;;   ;;;    ;;;    ;;;;           ;;;   ;;;;   ;;;     ;;;   ;;;  
;       ;                                                                
;    ;  ;                                                                
;     ;;                                                                 

(module+ test
  (check-true (is-a? sample player%) "test coverage")
  (check-equal? (send sample name) "Adam" "coverage")
  (check-true (cons? (send sample description)) "coverage")

  (begin 
    (check-true (void? (send sample setup '[])) "coverage")
    (check-equal? (send sample request-pebble-or-trades ts0) #false))
  
  (check-true (void? (send sample reset)) "coverage")
  (check-true (void? (dev/null (send sample win #true))) "coverage")
  (check-true (void? (dev/null (send sample win #false))) "coverage")

  (check-equal? (send sample request-cards ts0) '())

  (begin
    (send sample setup (list rg=bbbb ggg=r ggb=rw))
    (render ts0)
    (check-equal? (send sample request-pebble-or-trades ts0) '[] "a trade is possible, but useless")
    (check-equal? (send sample request-cards ts0) '[] "the player can't buy anything")))
;                                                                          
;                                                                          
;     ;                                                ;                   
;     ;                       ;                                            
;     ;                       ;                                            
;   ;;;;;     ;;;     ;;;   ;;;;;;   ;;;;    ;;;;    ;;;     ;;;;    ;;;;  
;     ;      ;   ;   ;   ;    ;     ;;  ;;   ;;  ;     ;    ;    ;  ;    ; 
;     ;          ;  ;         ;     ;    ;   ;         ;    ;;;;;;  ;      
;     ;      ;;;;;  ;         ;     ;    ;   ;         ;    ;        ;;;;  
;     ;     ;    ;  ;         ;     ;    ;   ;         ;    ;            ; 
;     ;     ;   ;;   ;   ;    ;     ;;  ;;   ;         ;    ;;   ;  ;    ; 
;     ;      ;;; ;    ;;;      ;;;   ;;;;    ;       ;;;;;   ;;;;;   ;;;;  
;                                                                          
;                                                                          
;                                                                          
;                                                                          

(define (create-player name [which purchase-size] #:bad (F (retrieve-factory "good" good-for-6)))
  (F name which))

(define (retrieve-factory name table+)
  (define maker (assoc name table+))
  (unless maker
    (error 'retrieve-factory "cannot retrieve ~s in ~v\n" name table+))
  (and maker (second maker)))

(define good-for-6 `[["good" ,(λ (n s) (new player% [my-name n] [which s]))]])

(module+ test
  (check-true (is-a? (create-player "hello" purchase-points) player%))

  (define bsetup-factory (retrieve-factory "setup" exn-raising-table-for-7))
  (define bsetup (create-player "hello" purchase-points #:bad bsetup-factory))
  (check-true (is-a? bsetup player%))
  (check-equal? (object-name bsetup) 'object:setup%)

  (check-exn #px"cannot" (λ () (retrieve-factory "boo" factory-all))))



;                                                                        
;                                                ;;                      
;          ;;;                              ;   ;              ;   ;;;   
;            ;                             ;    ;                    ;   
;    ;;;     ;    ;;;;    ;;;    ;;;       ;  ;;;;;  ;;;;    ;;;     ;   
;   ;;  ;    ;        ;  ;   ;  ;   ;     ;     ;        ;     ;     ;   
;   ;        ;        ;  ;      ;         ;     ;        ;     ;     ;   
;   ;        ;     ;;;;   ;;;    ;;;     ;      ;     ;;;;     ;     ;   
;   ;        ;    ;   ;      ;      ;    ;      ;    ;   ;     ;     ;   
;   ;;       ;    ;   ;  ;   ;  ;   ;   ;       ;    ;   ;     ;     ;   
;    ;;;;     ;;   ;;;;   ;;;    ;;;    ;       ;     ;;;;   ;;;;;    ;; 
;                                      ;                                 
;                                                                        
;                                                                        

;; allow the creation of player% subclasses that have one method go bad after "n" calls
;; call the `reset` method between test cases to get the state of count ("n") right 

(define-syntax (class/fail stx)
  (syntax-parse stx
    [(class/fail go-bad-after-this-many-times [(method-that-goes-bad args) body ...])
     #:with equations (datum->syntax stx 'equations #'class/fail #'class/fail)
     #'(class* player% ()
         (init-field #; [String {Nat}]  badfm) ;; descriptor for use in integration tests
         (inherit-field my-name equations)
         (super-new)

         (define/override (description)
           (append (super description) badfm))

         ;; `reset` is called at the very beginning of the game and only once. 
         ;; If method-that-goes-bad is setup, then new count is 1 because `setup` is running.
         ;; Otherwise the new count is 0, because no other method has been called yet. 
         (field [count 0])
         (define/override (reset)
           (super reset)
           (set! count (if (eq? 'method-that-goes-bad 'setup) 1 0)))
       
         (define/override (method-that-goes-bad . args)
           (set! count (+ count 1))
           (cond
             [(< count go-bad-after-this-many-times)
              (super method-that-goes-bad . args)]
             [(>= count go-bad-after-this-many-times)
              (let* ([s (super method-that-goes-bad . args)]
                     [x (let () body ...)])
                (or x s))])))]))

;                                                                                                    
;                                                                                                    
;                                                                   ;;;;                             
;                                                                   ;  ;                             
;                 ;              ;;;   ;   ;  ; ;;                 ;    ;;;;;;;   ;;;    ;;;;  ;;;;  
;                  ;;;          ;;  ;   ; ;   ;;  ;                ;    ;;  ;  ; ;;  ;  ;;  ;      ; 
;                     ;;        ;   ;;  ;;;   ;   ;                ;    ;;  ;  ; ;   ;; ;   ;      ; 
;   ;;;;;; ;;;;;;     ;;        ;;;;;;   ;    ;   ;                ;    ;;  ;  ; ;;;;;; ;   ;   ;;;; 
;                  ;;;          ;       ;;;   ;   ;                ;    ;;  ;  ; ;      ;   ;  ;   ; 
;                 ;             ;       ; ;   ;   ;    ;;           ;  ; ;  ;  ; ;      ;; ;;  ;   ; 
;                                ;;;;  ;   ;  ;   ;    ;;           ;;;; ;  ;  ;  ;;;;   ;;;;   ;;;; 
;                                                      ;                                    ;        
;                                                     ;;                                 ;  ;        
;                                                                                         ;;         

#; {[-> Any] {Natural} -> [Class <: Player%]}
;; functions that generate player% subclasses with exn-raising or diverging methods

(define (setup% th [failure-timing 1])
  (class/fail failure-timing [(setup _) [th]]))

(define (request-pebble-or-trades% th [failure-timing 1])
  (class/fail failure-timing [(request-pebble-or-trades _) [th]]))

(define (request-cards% th [failure-timing 1])
  (class/fail failure-timing [(request-cards _) [th]]))

(define (win% th [failure-timing 1])
  (class/fail failure-timing [(win _) [th]]))

(define baddies% (list setup% request-pebble-or-trades% request-cards% win%))

#; {Class -> String}
;; retrieve the non-% part of a class's name, which is assumed to be alphabetic plus a hyphen 
(define (class-name c)
  (second (regexp-match #px"([a-z\\-A-Z]*)%" (~a (object-name c)))))

;; ---------------------------------------------------------------------------------------------------
;; a factory table for methods that raise exceptions immeditaley

(define [exn] (/ 1 0))

(define exn-raising-table-for-7
  (for*/list ([f baddies%])
    (define class% (f exn))
    (define name-% (class-name f))
    (list (format "~a" name-%) (λ (n s) (new class% [badfm `(,name-%)] [my-name n] [which s])))))

;; ---------------------------------------------------------------------------------------------------
;; a factory table for methods that go into infinite loops when called the n-th time 

(define [loop] [loop])
(define Count# 7)

(define infinite-loop-table-for-9
  (for*/list ([f baddies%] [k (in-range 1 (+ Count# 1) 1)])
    (define class% (f loop k))
    (define name-% (class-name f))
    (list (format "~a-~a" name-% k)
          (λ (n s) (new class% [badfm `(,name-% ,k)] [my-name n] [which s])))))

(module+ test ;; players that raise exns 
  (define cep current-error-port)

  (let* ([new-exn-setup    (retrieve-factory "setup" exn-raising-table-for-7)]
         [name             "bad"]
         [exn-setup-player (create-player "bad" purchase-points #:bad new-exn-setup)])
    (check-equal? (check-message "div" cep (~a name " won") (send exn-setup-player win #t)) (void))
    (check-exn #px"division" (λ () (send exn-setup-player setup '[]))))

  (let* ([new-exn-rc    (retrieve-factory "request-pebble-or-trades" exn-raising-table-for-7)]
         [exn-nt-player (create-player "bad" purchase-size #:bad new-exn-rc)])
    (check-exn #px"division" (λ () (send exn-nt-player request-pebble-or-trades ts0))))

  (let* ([new-exn-rc    (retrieve-factory "request-cards" exn-raising-table-for-7)]
         [exn-nt-player (create-player "bad" purchase-size #:bad new-exn-rc)])
    (check-exn #px"division" (λ () (send exn-nt-player request-cards ts0))))

  (let* ([new-exn-win    (retrieve-factory "win" exn-raising-table-for-7)]
         [exn-win-player (create-player "bad" #:bad new-exn-win)])
    (check-exn #px"div" (λ () (check-message "div2" cep #px"bad" (send exn-win-player win #f))))))

;                                                          
;          ;                                               
;          ;                      ;                        
;          ;                      ;                        
;    ;;;   ; ;;    ;;;   ;;;;   ;;;;;   ;;;    ;;;;   ;;;  
;   ;;  ;  ;;  ;  ;;  ;      ;    ;    ;;  ;   ;;  ; ;   ; 
;   ;      ;   ;  ;   ;;     ;    ;    ;   ;;  ;     ;     
;   ;      ;   ;  ;;;;;;  ;;;;    ;    ;;;;;;  ;      ;;;  
;   ;      ;   ;  ;      ;   ;    ;    ;       ;         ; 
;   ;;     ;   ;  ;      ;   ;    ;    ;       ;     ;   ; 
;    ;;;;  ;   ;   ;;;;   ;;;;    ;;;   ;;;;   ;      ;;;  
;                                                          
;                                                          
;                                                          

;; ---------------------------------------------------------------------------------------------------
;; a cheating method that returns #false falls back on the `super` method

;; TODO: check which of the methods will always succceed or may fail 

(define wallet-cannot-trade%
  (class/fail
   1
   [(request-pebble-or-trades t*)
    (define t (first t*)) ;; because class/fail uses . args
    (make-trade-w/o-support (p:player-wallet (t:turn-active t)) equations)]))

(define bank-cannot-trade%
  (class/fail
   1
   [(request-pebble-or-trades t*)
    (define t (first t*)) ;; because class/fail uses . args
    (make-trade-w/o-support (t:turn-bank t) equations)]))

(define (make-trade-w/o-support pebbles equations)
  (or
   (for/first ([e equations] #:unless (b:subbag? (e:1eq-left e) pebbles))  (list e))
   (for/first ([e equations] #:unless (b:subbag? (e:1eq-right e) pebbles)) (list (e:1eq-flip e)))))

(module+ test
  (let* ([f (retrieve-factory "wallet-cannot-trade" cheater-table-for-8)]
         [a (create-player "ouch" #:bad f)])
    (send a setup `[,r=bbbb])
    (check-equal? (send a request-pebble-or-trades ts0) (list r=bbbb)))

  (let* ([f (retrieve-factory "bank-cannot-trade" cheater-table-for-8)]
         [a (create-player "ouch" #:bad f)])
    (send a setup `[,r=bbbb])
    (check-equal? (send a request-pebble-or-trades ts0) (list r=bbbb))))

;; ---------------------------------------------------------------------------------------------------
(define use-non-existent-equation%
  (class/fail
   1
   [(request-pebble-or-trades t)
    (e:make-non-existent-equation equations)]))

(module+ test
  (let* ([f (retrieve-factory "use-non-existent-equation" cheater-table-for-8)]
         [a (create-player "ouch" #:bad f)])
    (send a setup `[,r=bbbb])
    (check-equal? (send a request-pebble-or-trades ts0) (list w=bbbb))))

;; ---------------------------------------------------------------------------------------------------
(define buy-unavailable-card%
  (class/fail
   1
   [(request-cards t*)
    (define t (first t*)) ;; because class/fail uses . args
    (define visibles (turn-cards t))
    (for/first ([c ALL-CARDS] #:unless (member c visibles))
      (list c))]))

(define wallet-cannot-buy-card%
  (class/fail
   1
   [(request-cards t*)
    (define t (first t*)) ;; because class/fail uses . args
    (define visibles (turn-cards t))
    (define wallet   (p:player-wallet (t:turn-active t)))
    (for/first ([c visibles] #:unless (b:subbag? (c:card-pebbles c) wallet))
      (list c))]))

(module+ test
  ts1
  (let* ([f (retrieve-factory "wallet-cannot-buy-card" cheater-table-for-8)]
         [a (create-player "ouch" #:bad f)])
    (send a setup `[,r=bbbb])
    (check-equal? (send a request-cards ts1) (list c-ggggg))))

;; ---------------------------------------------------------------------------------------------------
(define all-cheater-classes
  `[ [,use-non-existent-equation%
      "an exchange that is not according to any given equation"]
     [,bank-cannot-trade%
      "an exchange according to some given equation that the bank cannot currently support"]
     [,wallet-cannot-trade%
      "an exchange according to some given equation that its wallet does not currently support"]
     [,buy-unavailable-card%
      "the purchase of a card that is not among the currently visible ones"]
     [,wallet-cannot-buy-card%
      "the purchase of a card that its wallet cannot currently afford"] ])

;; ALSO CHECK THAT THE RETURNS ARE ILLEGAL! 

(define ACHEAT "a cheat")

(define cheater-table-for-8
  (for*/list ([c% (map first all-cheater-classes)])
    (define name (class-name c%))
    (list (format "~a" name) (λ (n s) (new c% [badfm `(,ACHEAT ,name)] [my-name n] [which s])))))

;                       
;                       
;          ;;;    ;;;   
;            ;      ;   
;   ;;;;     ;      ;   
;       ;    ;      ;   
;       ;    ;      ;   
;    ;;;;    ;      ;   
;   ;   ;    ;      ;   
;   ;   ;    ;      ;   
;    ;;;;     ;;     ;; 
;                       
;                       
;                       

(define factory-all (append exn-raising-table-for-7 cheater-table-for-8 infinite-loop-table-for-9))

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
  (define lax-names-okay? (make-parameter #false))

  (define (actor*->jsexpr p*) (map actor->jsexpr p*))

  (define (actor->jsexpr p) (send p description))

  (define (jsexpr->actor* j #:loops [loops #false] #:cheating [cheaters #false])
    (match j
      [(list (app (λ (j) (jsexpr->actor j #:loops loops #:cheating cheaters))
                  (? (λ (p) (is-a? p player%)) players)) ...)
       (unless (<= (length players) MAX-PLAYERS)
         (error 'jexpr->actor* "*Actors contains more players than the game specs allow"))
       players]
      [_ (eprintf "value does not match *Actors schema:\n~a\n" (jsexpr->string j))
         #false]))

  (define (jsexpr->actor j #:loops [loops #false] #:cheating [cheaters #false])
    (match j
      [(list* (? jname? name) (app jsexpr->policy (? procedure? s)) remainder)
       (define check-then-create [check-then-create/curried j name s])
       (match remainder
         ['()
          (check-then-create #false           "good" 'x good-for-6)]
         [(list (? string? m))
          (check-then-create #false            m "exception" exn-raising-table-for-7)]
         [(list (== ACHEAT) (? string? m))
          (check-then-create (false? cheaters) m "cheating" cheater-table-for-8 )]
         [(list (? string? m) (? natural? n))
          (check-then-create (false? loops)    (~a m "-" n) "looping" infinite-loop-table-for-9)]
         [_ (err "options beyond 'plain' don't match" j)])]
      [_ (err "not an array" j)]))

  #; {JSexpr -> [Option String]}
  (define (jname? j)
    (cond
      [(not (string? j)) (err "name not a string" j)]
      [(lax-names-okay?) j]
      [(not (regexp-match (pregexp PLAYER-NAME) j)) (err "name not alphanumeric or too short" j)]
      [(not (<= (string-length j) MAX-PLAYER-NAME)) (err "name too long" j)]
      [else j]))

  #; {JSexpr String Strategey -> Boolean String Natural FactoryTable -> [Option Player]}
  (define ([check-then-create/curried j name which] b? bad-method n factory-table)
    (cond
      [(or b? (not-in bad-method factory-table)) (err n j)]
      [else (create-player name which #:bad (retrieve-factory bad-method factory-table))]))
  
  #; {String FactoryTable -> Boolean}
  (define (not-in bad-method factory-table)
    (define r (assoc bad-method factory-table))
    (not r))

  #; {JSexpr N -> False}
  (define (err n j)
    (define s (jsexpr->string/ j))
    (eprintf "~a does not match Actor schema [~a] \n ~a\n" 'jsexpr->actor n s)
    #false))

(module+ test
  
  (check-false (check-message "a" cep #px"schema" (jsexpr->actor 1)) "bad JSexpr 1")
  (check-false (check-message "b" cep #px"schema" (jsexpr->actor '["a" "dag" 1])) "bad JSexpr 2")
  (check-false (check-message "c" cep #px"not match" (jsexpr->actor `["a" "dag" "setup" 1])) "BAD")

  (check-equal? (send (jsexpr->actor (actor->jsexpr sample)) setup `[]) (void) "normal player")

  (let* ([new-exn-setup (retrieve-factory "setup" exn-raising-table-for-7)]
         [exn-setup     (create-player "bad" purchase-points #:bad new-exn-setup)])
    (check-exn #px"div" (λ () (send (jsexpr->actor (actor->jsexpr exn-setup)) setup '[])) "j exn"))


  (let* ([new-exn-setup (retrieve-factory "setup-1" infinite-loop-table-for-9)]
         [setup-1       (create-player "bad" purchase-points #:bad new-exn-setup)])
    (check-exn #px"out of time"
               (λ ()
                 (with-deep-time-limit 1
                   (send (jsexpr->actor (actor->jsexpr setup-1) #:loops 'yes) setup '[]))) "j inf"))

  (let* ([buy-invisible-card-factory (retrieve-factory "buy-invisible-card" cheater-table-for-8)]
         [players (list (create-player "A" purchase-points #:bad buy-invisible-card-factory))]
         [j (actor*->jsexpr players)]
         [p (jsexpr->actor* j #:cheating 'yes!) ]) 
    (check-true (andmap (λ (p) (is-a? p player%)) p) "cheating player")))
