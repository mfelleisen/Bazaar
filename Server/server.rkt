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
   (->i ([refc     (list/c e:equations? gs:game?)]
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

(require Bazaar/Lib/configuration)

(require SwDev/Testing/communication)
(require (only-in SwDev/Lib/should-be-racket all-but-last))

(module+ examples
  (require (submod ".."))
  (require
    (prefix-in
     c: (only-in
         Bazaar/Client/client
         make-client-for-name-sender default-client-config set-client-config QUIET PORT ACTOR*)))
  (require (submod Bazaar/Referee/referee examples))
  (require (prefix-in c: Bazaar/Client/referee)))

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
      (apply referee/state actor* for-referee)))
  (send-message (f result))
  (optionally-return-result (f result)))

(define (f result)
  (match-define [list w do] result)
  (list (map (λ (o) (send o name)) w) (map (λ (o) (send o name)) do)))

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
  (define (scenario* milestone# ref-scenario* #:quiet (quiet #true) #:extras (client* '[]))
    (for/list ([r ref-scenario*])
      (match-define [list args expected msg] r)
      (match-define [list actor* equations gs] args)
      (define refc  (list equations gs))
      (define label (~a milestone# " " msg))
      (server-client-scenario refc actor* expected label #:quiet quiet #:extras client*)))
  
  #; {String RefereeData [Listof ActorObject] Any String
             #:quiet    Boolean
             #:extras   [Listof Client]
             #:drop     (-> [Listof Player] [Listof Player])
             #:expected (-> Result Result)
             -> ServerClientScenario}

  ;; set up basics, then: 
  ;; create a server-client secnarior from a referee scenario
  (define p# 45674)
  (define (server-client-scenario refc actor* expected label
           #:expected (result identity)
           #:drop     (drop-a-given-player identity)
           #:quiet    (q #true)
           #:extras   (client* '[])
           ;; the update function injects the actual (bad) players into the game state to
           ;; circumvent the naming conflict 
           #:update-state-players (update #false)) #; (-> [Listof Player] [Listof Player])
           
    (set! p# (+ p# 1))

    (define nu-players (drop-a-given-player actor*))
    (define cc (c:set-client-config c:default-client-config c:ACTOR* nu-players c:QUIET q c:PORT p#))
    (define sc (set-server-config default-server-config QUIET q PORT p#))
    
    (define nu-expected (result expected))
    (list refc sc cc client* nu-expected label)))

;; ---------------------------------------------------------------------------------------------------
;; ---------------------------------------------------------------------------------------------------
(module+ examples  ;; bad name senders
  (provide special-scenario-client-from-port)

  (struct special-scenario [client-from-port type])

  (define divBeforeSend "divergeBeforeSending")
  (define [client-diverge-before-sending-name port#]
    (c:make-client-for-name-sender divBeforeSend (λ (_ ip) (let L () (L))) port#))

  #;
  (define scenario-special-1
    (mixed-all-tiles-rev-inf-exn-dag2-A
     (server-client-scenario
      (~a "a client that connects but does not complete the registration")
      #:extras (list [special-scenario client-diverge-before-sending-name divBeforeSend]))
     "dummy one"
     "dummy two"))
 
  ;; thne next one should be observationally equivalent to a player that goes infinite in setup
  (define divAfterSend  "sendnameloop")
  (define [client-diverge-after-sending-name port#]
    (c:make-client-for-name-sender divAfterSend (λ (n ip) (send-message n ip) (let L () (L))) port#))
  (define remote-player-for-client
    (create-remote divAfterSend (current-input-port) (current-output-port)))

  #;
  (define scenario-special-2
    (mixed-all-tiles-rev-inf-exn-dag ;; four players expected 
     (server-client-scenario 
      (~a "a client that connects but does not complete the registration")
      #:drop all-but-last
      #:update-state-players (λ (given) (append given [list remote-player-for-client]))
      #:expected (λ (x) (list (first x) (cons divAfterSend (reverse (rest (second x))))))
      #:extras (list [special-scenario client-diverge-after-sending-name divAfterSend]))
     "dummy one"
     "dummy two"))

  (provide special->jsexpr jsexpr->special)
  
  (define (special->jsexpr fc)
    (special-scenario-type fc))

  (define (jsexpr->special j)
    (match j
      [(== divAfterSend) client-diverge-after-sending-name]
      [(== divBeforeSend) client-diverge-before-sending-name]
      [_ (eprintf "jsexpr->special : ~a is not a special scenario" j)
         #false])))
  
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
    (define quiet (dict-ref sconfig QUIET))
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

;; ---------------------------------------------------------------------------------------------------

(module+ test
  ; (provide sc1)
  (require (submod Bazaar/Referee/referee examples))
  (define sc1 (scenario* 7 Simple/))
  (for-each run-server-client-scenario (scenario* 7 Simple/))
  '---
  (for-each run-server-client-scenario (scenario* 7 Complex/))
  '---+++)

;; ---------------------------------------------------------------------------------------------------

#;
(module+ test
  'special-1
  (run-server-client-scenario scenario-special-1)
  'special-2
  (run-server-client-scenario scenario-special-2))


#;
(module+ test ;; running scenarios as unit tests
  7
  (for-each run-server-client-scenario scenarios-for-7)
  (for-each run-server-client-scenario scenarios-for-7/s)
  8
  (for-each run-server-client-scenario scenarios-for-8)
  (for-each run-server-client-scenario scenarios-for-8/s)
  9
  (for-each run-server-client-scenario scenarios-for-9)
  (for-each run-server-client-scenario scenarios-for-9/s)
  'A
  (for-each run-server-client-scenario scenarios-for-A)
  'B
  (for-each run-server-client-scenario scenarios-for-B))
