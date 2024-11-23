#lang racket

;; a server that signs up players over TCP and runs a game 

(require Bazaar/scribblings/spec)

;; ---------------------------------------------------------------------------------------------------
(provide
 ascending-by-age
 descending-by-age

 ;; for homework 
 server-config->definition
 set-server-config
 QUIET
 PORT 
 
 jsexpr->server-config
 server-config->jsexpr
 
 (contract-out
  [default-server-config server-config/c]
  
  [server
   ;; returns the list of winners and cheaters/failures

   ;; get at least `MIN-PLAYERS while running at most `SERVER-TRIES` wait periods of `SERVER-WAIT`s
   ;; but as soon as `MAX-PLAYERS` have signed up, stop accepting more players and run the game
   ;; if at the end of a wait period `MIN-PLAYERS` are around, also launch the game and stop signups
   ;; otherwise, shut down.
   ;;   (a production server would have a "wait in line" queue for late comers; and it would restart.)
   ;; 
   ;; runs a referee on the players that signed up properly port# plus the house players (if any) 
   (->i ([refc     (list/c e:equations? gs:game? any/c #;bonus-function)]
         [confg    server-config/c])
        ([ordering (-> list? list?)]
         [plyrs    list?]
         #:result (return-results-or-void (-> list? any/c)))
        (result any/c))]))

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

(require Bazaar/Server/player)
(require (prefix-in e: Bazaar/Common/equations))
(require Bazaar/Common/player-interface) ;; type Player 
(require (prefix-in gs: Bazaar/Referee/game-state))
(require Bazaar/Referee/referee)
(require (only-in (submod Bazaar/Referee/referee examples) void-observer%))

(require Bazaar/Lib/configuration)

(require SwDev/Testing/communication)

(module+ examples
  (require (submod ".."))
  (require (only-in (submod Bazaar/Common/equations examples) ggb=rw))
  (require (submod Bazaar/Referee/game-state examples))
  (require (prefix-in p: Bazaar/Common/player)) ;; type Player 
  (require
    (prefix-in
     c: (only-in
         Bazaar/Client/client
         make-client-for-name-sender default-client-config set-client-config QUIET PORT ACTOR*)))
  (require Bazaar/Lib/parse-json)
  (require SwDev/Testing/scenarios))

(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod Bazaar/Referee/game-state examples))
  (require (prefix-in c: Bazaar/Client/client))
  (require rackunit))

;                                                          
;                                                          
;                              ;;;     ;                   
;                             ;        ;                   
;                             ;                            
;     ;;;    ;;;;   ; ;;;   ;;;;;;   ;;;     ;;; ;   ;;;;  
;    ;   ;  ;;  ;;  ;;   ;    ;        ;     ;  ;;  ;    ; 
;   ;       ;    ;  ;    ;    ;        ;    ;    ;  ;      
;   ;       ;    ;  ;    ;    ;        ;    ;    ;  ;;;    
;   ;       ;    ;  ;    ;    ;        ;    ;    ;     ;;; 
;   ;       ;    ;  ;    ;    ;        ;    ;    ;       ; 
;    ;   ;  ;;  ;;  ;    ;    ;        ;     ;  ;;  ;    ; 
;     ;;;    ;;;;   ;    ;    ;     ;;;;;;;  ;;; ;   ;;;;  
;                                                ;         
;                                            ;  ;;         
;                                             ;;;          
;                                                          

(define PORT0 45678)

(define-configuration server
  (PORT            PORT0 #:is-a "Natural" "between 10000 and 60000")
  (SERVER-TRIES    2 #:is-a "Natural" "less than 10")
  (SERVER-WAIT     20 #:is-a "Natural" "less than 30[s]")
  (WAIT-FOR-SIGNUP 2 #:is-a "Natural" "less than 10")
  (QUIET           #true #:is-a "Boolean"))

;                                            
;                                            
;                                            
;    ;;;    ;;;    ;;;;  ;   ;   ;;;    ;;;; 
;   ;   ;  ;;  ;   ;;  ; ;   ;  ;;  ;   ;;  ;
;   ;      ;   ;;  ;      ; ;   ;   ;;  ;    
;    ;;;   ;;;;;;  ;      ; ;   ;;;;;;  ;    
;       ;  ;       ;      ; ;   ;       ;    
;   ;   ;  ;       ;       ;    ;       ;    
;    ;;;    ;;;;   ;       ;     ;;;;   ;    
;                                            
;                                            
;                                            

(define LOCAL     "127.0.0.1")
(define MAX-TCP   30)
(define REOPEN    #true)
(define DEFAULT-RESULT '[[] []])

(define test-run?  (make-parameter #false)) ;; [Channel [Listof N]] when used 

;; IMPLEMENTATION 
;; create separate thread that signs up one player at time
;; this thread sends a list of players to the main thread as specified
;; it also waits for a question from the main thread (at the end of a wait period)
;;   when this question arrives, it stops running if `MIN-PLAYERS` have signed up
;;   otherwise it returns `#false` to indicate it wishes to sign up more players 

(define ascending-by-age values) ;; youngest first 
(define descending-by-age reverse) ;; oldest first 

(define (server refc config [age-ordering ascending-by-age] [house-players '()] #:result (show void))
  ;; set up custodian so `server` can clean up all threads, TCP ports, etc. -- in case it is re-used
  (parameterize ([current-custodian (make-custodian)])
    (define players (wait-for-players house-players config))
    (begin0
      (cond
        [(empty? players)
         (send-message DEFAULT-RESULT) (show DEFAULT-RESULT)]
        [(test-run?)
         => (λ (result) (channel-put result (age-ordering players)) DEFAULT-RESULT)]
        [else
         (configure-and-run-referee (age-ordering players) refc show)])
      (custodian-shutdown-all (current-custodian)))))

#; {[Listof Player] RefereeInfo -> [List [Listof Player] [Listof Player]]}
(define (configure-and-run-referee actor* for-referee optionally-return-result)
  (define result
    (with-handlers ([exn:fail?
                     (λ (n)
                       (eprintf "server reports referee failure\n")
                       (eprintf "~a\n" (exn-message n))
                       DEFAULT-RESULT)])
      ;; IF AN OBSERVER IS DESIRED -------------------- VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
      (apply referee/state actor* (append for-referee #; (list (list (new void-observer%)))))))
  (send-message result)
  (optionally-return-result result))

#; {[Listof Player] ServerConfig -> [Listof Player]}
;; collect list of playaers in reverse order of sign-up [youngest first]
;; it spawns a thread to manage time and number of tries in parallel 
#|  wait-for-players
        |   spawn(channel)
        | -----------------> sign-up-players
        |                         |

 1. 

        |                         |
        |   put(channel, players) | once max # of players 
        | <---------------------- | sign up 
        |                        ---
        | 

 2. 
        |   spawn(channel)
        | -----------------> sign-up-players
        |                         |
        |   put(channel: and?)    | 
        | --------------------->  | if time has run out 
        |                         |
        |   put(channel: players) | if min # of players
        | <---------------------- | have signed up
        |                         |
        |                        ----

  3. 
        |   spawn(channel)
        | -----------------> sign-up-players
        |                         |
        |   put(channel: and?)    | 
        | --------------------->  | if time has run out 
        |                         |
        |   put(channel: #false)  | too few players 
        | <---------------------- | have signed up
        |                         |
        |                         |
      more wait time passes and 1/2/3 repeat up to number of wait periods 
|#

(define (wait-for-players house-players config)
  (define max-time  (dict-ref config SERVER-WAIT))
  (define max-tries (dict-ref config SERVER-TRIES))
  (define communicate-with-sign-up (make-channel))
  (thread (sign-up-players communicate-with-sign-up house-players config))
  (let loop ([n max-tries])
    (cond
      [(zero? n) '()]
      [(sync/timeout max-time communicate-with-sign-up) => identity] 
      [else
       (channel-put communicate-with-sign-up "are there at least then min# of players signed up")
       (cond
         [(channel-get communicate-with-sign-up) => values]
         [else (loop (- n 1))])])))

#; {[Channel String] [Listof Player] ServerConfig -> (- Void)}
;; create a process function for signing up players
;; stops if `MAX-PLAYERS` have signed up _or_
;;       if `wait-for-players` requests players and `MIN-PLAYERS` have signed up
(define [(sign-up-players send-players house-players config)]
  (let/ec return
    (define listener (tcp-listen (dict-ref config PORT) MAX-TCP REOPEN))
    (define *players (box house-players))
    (parameterize ([io-time-out (dict-ref config WAIT-FOR-SIGNUP)])
      (let collect-players ()
        (cond
          [(= (length (unbox *players)) MAX-PLAYERS)
           (return (channel-put send-players (unbox *players)))]
          [(>= (length (unbox *players)) MIN-PLAYERS)
           (sync
            (handle-evt listener (sign-up->add-to *players listener))
            (handle-evt send-players (λ (_) (return (channel-put send-players (unbox *players))))))
           (collect-players)]
          [else
           (sync
            (handle-evt listener (sign-up->add-to *players listener))
            (handle-evt send-players (λ (_) (channel-put send-players #false))))
           (collect-players)])))))

#; ([Box [Listof Player]] TCP-Listener -> Void)
(define [(sign-up->add-to *players listener) _]
  (with-handlers ((exn:fail:network? (lambda (x) (log-error "connect: ~a" (exn-message x)) *players)))
    (define-values (in out) (tcp-accept listener))
    (define name (read-message in)) ;; timed
    (cond
      [(player-name? name)
       (define nxt (if (test-run?) (add1 (length (unbox *players))) (create-remote name in out)))
       (set-box! *players (cons nxt (unbox *players)))]
      [else
       (eprintf "improper sign-up: ~a\n" (bad-name->message name))
       (close-input-port in)
       (close-output-port out)])))

#; {JSexpr -> String}
(define (bad-name->message name)
  [if (string? name)
      (if (regexp-match #px"Timed" name) "timed out" name)
      (~a "not a string: " name)])
;                                                                 
;                                                                 
;                                                ;                
;                                                                 
;    ;;;    ;;;    ;;;   ; ;;   ;;;;    ;;;;   ;;;    ;;;    ;;;  
;   ;   ;  ;;  ;  ;;  ;  ;;  ;      ;   ;;  ;    ;   ;; ;;  ;   ; 
;   ;      ;      ;   ;; ;   ;      ;   ;        ;   ;   ;  ;     
;    ;;;   ;      ;;;;;; ;   ;   ;;;;   ;        ;   ;   ;   ;;;  
;       ;  ;      ;      ;   ;  ;   ;   ;        ;   ;   ;      ; 
;   ;   ;  ;;     ;      ;   ;  ;   ;   ;        ;   ;; ;;  ;   ; 
;    ;;;    ;;;;   ;;;;  ;   ;   ;;;;   ;      ;;;;;  ;;;    ;;;  
;                                                                 
;                                                                 
;                                                                 

;; turn Ref scenarios into Server-Client scenarios so that it is possible to add "evil" actors 
(module+ examples
  (provide scenario*)

  #; {type ServerClientScenario =
           [List RefereeData ServerConfiguration ClientConfiguration [Listof BadClients] Any String]}
  #; {type RefereeData = [List Equations GameState]}

  #; {Natural [Listof RefereeScenario] -> [Listof ServerClientScenario]}
  (define (scenario* milestone# ref-scenario*
                     #:bonus  (aw p:player-award-none)
                     #:quiet  (quiet #true)
                     #:extras (client* '[]))
    (for/list ([r ref-scenario*])
      (match-define [list args expected msg] r)
      (match-define [list actor* equations gs] args)
      (define refc  (append (list equations gs) (list aw)))
      (define label (~a milestone# " " msg))
      (server-client-scenario refc actor* expected label #:quiet quiet #:extras client*)))
  
  #; {RefereeData [Listof ActorObject] Any String -> ServerClientScenario}
  #; { #:quiet Boolean #:extras [Listof Client] }

  ;; set up basics, then: 
  ;; create a server-client scenario from a referee scenario
  (define p# 45674)
  (define (server-client-scenario refc actor* expected label #:quiet (q #true) #:extras (client* '[]))
    (set! p# (+ p# 1))
    (define cc (c:set-client-config c:default-client-config c:ACTOR* actor* c:QUIET q c:PORT p#))
    (define sc (set-server-config default-server-config QUIET q PORT p#))
    (list refc sc cc client* expected label)))

;; ---------------------------------------------------------------------------------------------------
(module+ examples ;; actors with bad names and bad JSON communication 
  (provide Names/ Baddies/)

  (require Bazaar/Client/referee)
  (require Bazaar/Player/mechanism)

  (define playing-with-baddies
    (for/list ([i 5]) (create-player (~a "goodie" i))))

  (define bad-json-players
    (map (λ (n) (create-player (first n))) *referee-list))

  (define badly-named-players
    (list (create-player "ouch ouch")
          (create-player "ouchouchouchouchouchouchouchouch")))

  (setup-scenarios s+ Names/ Baddies/)

  (for ([bjp bad-json-players])
    (s+ Baddies/
        (list (cons bjp playing-with-baddies) `[,ggb=rw] gs-6-players)
        `[["goodie3"] [,(~a (send bjp name))]]
        (~a "1 bad JSON player, with all 5 goodies: " (send bjp name)(send bjp name))))

  (s+ Names/
      (list (append (take playing-with-baddies 3) badly-named-players) `[,ggb=rw] gs-3-zeros)
      '[["goodie0"] []]
      "2 bad names, need at least 2 surviving players, so we take 3 to match up with gs-3-zeros"))

;; ---------------------------------------------------------------------------------------------------
(module+ examples ;; players whose registrtation goes bad
  (provide scenario-special-1 scenario-special-2 special-scenario-client-from-port)
  (provide special->jsexpr jsexpr->special)

  (struct special-scenario [client-from-port type])

  (define refc (list `[,ggb=rw] gs-3-zeros p:player-award-none))

  (define special1 "a client that connects but does not complete the registration")
  (define name1 "AAdivergeBeforeSending")
  (define [diverge-before-sending-name port#]
    (c:make-client-for-name-sender name1 (λ (_ ip) (let L () (L))) port#))
  (define scenario-special-1
    (server-client-scenario refc (take playing-with-baddies 3) `[["goodie0"] []] special1
                            #:extras (list [special-scenario diverge-before-sending-name name1])))
 
  ;; thne next one should be observationally equivalent to a player that goes infinite in setup
  (define special2 "a client that connects but does not complete the registration")
  (define name2  "AAsendnameloop")
  (define [diverge-after-sending-name port#]
    (c:make-client-for-name-sender name2 (λ (n ip) (send-message n ip) (let L () (L))) port#))
  (define scenario-special-2
    (server-client-scenario refc (take playing-with-baddies 2) `[["goodie0"] [,name2]] special2
                            #:extras (list [special-scenario diverge-after-sending-name name2])))
  
  (define (special->jsexpr fc)
    (special-scenario-type fc))

  (define (jsexpr->special j)
    (match j
      [(== name2) diverge-after-sending-name]
      [(== name1) diverge-before-sending-name]
      [_ (eprintf "jsexpr->special : ~a is not a special scenario\n" j)
         (eprintf "jsexpr->special names: ~a\n" (list name1 name2))
         #false])))

(module+ examples
  (provide specials bonus-rwb bonus-sey bonus1 bonus2)
  (provide simple-7 simple-8 simple-9 complex-7 complex-8 complex-9)

  (require (submod Bazaar/Referee/referee examples))

  (define specials (list scenario-special-1 scenario-special-2))
  (define bonus1    (scenario* 10 Names/))
  (define bonus2    (scenario* 10 Baddies/))
  (define bonus-rwb (scenario* 10 rwb #:bonus p:player-award-red-white-and-blue-bonus))
  (define bonus-sey (scenario* 10 sey #:bonus p:player-award-seychelles-bonus))
  (define simple-7  (scenario* 7 Simple/))
  (define complex-7 (scenario* 7 Complex/))
  (define simple-8  (scenario* 8 8Simple/))
  (define complex-8 (scenario* 8 8Complex/))
  (define simple-9  (scenario* 9 9Simple/))
  (define complex-9 (scenario* 9 9Complex/)))
  
;                                                                        
;      ;;                                                                
;     ;           ;;;    ;;;             ;                    ;          
;     ;             ;      ;             ;                    ;          
;   ;;;;;  ;   ;    ;      ;           ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;    ;   ;    ;      ;             ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;    ;   ;    ;      ;             ;    ;   ;; ;        ;    ;     
;     ;    ;   ;    ;      ;             ;    ;;;;;;  ;;;     ;     ;;;  
;     ;    ;   ;    ;      ;             ;    ;          ;    ;        ; 
;     ;    ;   ;    ;      ;             ;    ;      ;   ;    ;    ;   ; 
;     ;     ;;;;     ;;     ;;           ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                                                        
;                                                                        
;                                                                        

(module+ test ;; how to run scenarios as unit tests 
  #; {ServerClientScenario -> Void}
  ;; start a server; start regular clients then bad clients; test
  (define (run-server-client-scenario server-client-scenario)
    (match-define [list refc sc cc bad-clients expected msg] server-client-scenario)
    (define special-clients (map special-scenario-client-from-port bad-clients))
    (check-equal? (run-server-client refc sc cc special-clients) expected msg))
  
  (define (run-server-client refc sconfig cconfig bad-clients)
    (parameterize ([current-custodian (make-custodian)])
      (define client   (launch-clients cconfig bad-clients))
      (define result   (launch-server refc sconfig))
      (begin0
        result
        (sync client)
        (custodian-shutdown-all (current-custodian)))))

  #; {ServerConfiguration -> Result}
  (define (launch-server refc sconfig)
    (define quiet   (dict-ref sconfig QUIET))
    (define err-out (if quiet (open-output-string) (current-error-port)))
    (parameterize ([current-error-port err-out])
      (server refc sconfig descending-by-age #:result values)))

  #; {ServerConfiguration [Listof Player] [Listof Player] -> Thread}
  (define (launch-clients cconfig bad-clients)
    (define port#   (dict-ref cconfig c:PORT))
    (define quiet   (dict-ref cconfig c:QUIET))
    (define err-out (if quiet (open-output-string) (current-error-port)))
    (thread
     (λ ()
       (parameterize ([current-error-port err-out])
         ;; for failuers before the threads are launched
         ;; `quiet` is passed along so that thrreads can quiet the proxy refs
         (define baddies (map (λ (f) (f port#)) bad-clients))
         (c:clients cconfig #:baddies baddies))))))

(module+ test
  '---Specials---
  (for-each run-server-client-scenario specials)

  '---Awards--
  (for-each run-server-client-scenario bonus-rwb)
  (for-each run-server-client-scenario bonus-sey)

  '---Bonus---
  (for-each run-server-client-scenario bonus1)
  (for-each run-server-client-scenario bonus2)

  '---7---
  (for-each run-server-client-scenario simple-7)
  (for-each run-server-client-scenario complex-7)

  '---8---
  (for-each run-server-client-scenario simple-8)
  (for-each run-server-client-scenario complex-8)

  '---9---
  (for-each run-server-client-scenario simple-9)
  (for-each run-server-client-scenario complex-9))


;                                                                                      
;                                                                                      
;     ;       ;             ;                          ;                    ;          
;     ;                                                ;                    ;          
;   ;;;;;   ;;;  ;;;;;;   ;;;   ; ;;    ;;;;         ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;       ;  ;  ;  ;    ;   ;;  ;  ;;  ;           ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;       ;  ;  ;  ;    ;   ;   ;  ;   ;           ;    ;   ;; ;        ;    ;     
;     ;       ;  ;  ;  ;    ;   ;   ;  ;   ;           ;    ;;;;;;  ;;;     ;     ;;;  
;     ;       ;  ;  ;  ;    ;   ;   ;  ;   ;           ;    ;          ;    ;        ; 
;     ;       ;  ;  ;  ;    ;   ;   ;  ;; ;;           ;    ;      ;   ;    ;    ;   ; 
;     ;;;   ;;;;;;  ;  ;  ;;;;; ;   ;   ;;;;           ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                          ;                                           
;                                       ;  ;                                           
;                                        ;;                                            

;; these tests are entirely independent of the game that's run

#;
(module+ test ;; timing
  
  #; {Port-Number (U False n:N) -> (U False [Listof 0]: (=/c (length) n))}
  #; (run-server-test p k)
  ;; runs the server on port p, waitig for m players, but receiving k
  (define (run-server-test port k #:delay (d #false) #:non-string (n #false))
    [define custodian (make-custodian)]
    [define result    (make-channel)]
    [define err-p     (open-output-string)]
    [define sconfig   (set-server-config default-server-config PORT port)]
    (define th        (launch-server-with-clients sconfig custodian result err-p port k d n))
    (begin0
      (cond
        [(or (not k) (< k MIN-PLAYERS))
         (sync th)
         (get-output-string err-p)]
        [else (channel-get result)])
      (custodian-shutdown-all custodian)))

  #; {Config Custodian [Channel list] Output-port Port [Option Natural] [Option Natural] -> Void}
  (define (launch-server-with-clients config2 custodian result err-out port k d n)
    (parameterize ([test-run?          result]
                   [current-custodian  custodian]
                   [current-error-port err-out])
      (define th (thread (λ () (server (list '[] gs0) config2 #:result values))))
      (sleep 1)
      (sign-up-fake-clients k port d n)
      th))

  #;{Thread [Option Natural] Port-Number [Option Natural] [Option Natural] -> Void}
  (define (sign-up-fake-clients how-many port delay-this-one-s-name-submission this-one-sends-number)
    (unless (boolean? how-many)
      (for ([i how-many])
        (define-values (_ out) (tcp-connect LOCAL port))
        (when (and delay-this-one-s-name-submission (= delay-this-one-s-name-submission i))
          (sleep (+ (dict-ref default-server-config WAIT-FOR-SIGNUP) .1)))
        (if (and this-one-sends-number (= this-one-sends-number i))
            (send-message 42 out)
            (send-message "a" out)))))
  
  'timing-tests
  (define the-range (range MAX-PLAYERS 0 -1))
  (check-equal? (run-server-test 45671 #f) "" "no sign ups")
  (check-equal? (run-server-test 45677 10) the-range "sign up enough players")
  (check-equal? (run-server-test 45677 11 #:delay 3) the-range "sign up enough players T")
  (check-equal? (run-server-test 45677 11 #:non-string 3) the-range "sign up enough players NS")
  (check-equal? (run-server-test 45676 1) "" "sign up too few players"))
