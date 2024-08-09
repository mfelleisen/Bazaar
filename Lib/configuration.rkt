#lang at-exp racket

;; support for defining objects

;; ---------------------------------------------------------------------------------------------------

(provide
 ;; SYNTAX
 #; (define-object name [key value] ...)
 ;; creates objects as dictionaries 
 ;; -- default dict object named
 #;    default-name-object
 ;; -- a list of availble options named
 #;    name-options
 ;; -- a domain-contract named
 #;    name-object/c
 ;;    ensuring that the domain consists of just the keys
 ;; -- a objecture-set function named set-name-object
 #;    (set-name-object c KEY VALUE ...)
 ;; -- to and from JSexpr conversions
 #;    (name-object->jsexpr c)
 #;    (jsexpr->name-object j)
 ;; -- a scribble rendering as a "schema definition" named
 #;    [name->text]
 define-object

 struct/description
 
 is-list-of-key-value-pairs)

;; ---------------------------------------------------------------------------------------------------
(require SwDev/Lib/hash-contract)
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))
(require (for-syntax racket/struct-info))
(require (for-syntax racket/format))
(require json)

;                                            
;                                            
;            ;                           ;   
;            ;                           ;   
;    ;;;   ;;;;;   ;;;;  ;   ;   ;;;   ;;;;; 
;   ;   ;    ;     ;;  ; ;   ;  ;;  ;    ;   
;   ;        ;     ;     ;   ;  ;        ;   
;    ;;;     ;     ;     ;   ;  ;        ;   
;       ;    ;     ;     ;   ;  ;        ;   
;   ;   ;    ;     ;     ;   ;  ;;       ;   
;    ;;;     ;;;   ;      ;;;;   ;;;;    ;;; 
;                                            
;                                            
;                                            

(define-syntax (struct/description stx)
  (syntax-parse stx
    [(_ name
        [key 
         (~optional (~seq #:to-jsexpr to #;function) #:defaults ([to #'identity]))
         (~optional (~seq #:from-jsexpr from #;function) #:defaults ([from #'identity]))
         (~optional (~seq #:is-a is-a ... #;string) #:defaults ([(is-a 1) (list #'"")]))]
        ...
        options ...)
     #:do   [(define n (syntax-e #'name))]
     #:with (keyv ...)   (generate-temporaries #'(key ...))
     #:with name->jsexpr (format-id stx "~a->jsexpr" n #:source #'name #:props stx)
     #:with jsexpr->name (format-id stx "jsexpr->~a" n #:source #'name #:props stx)
     #:with name->def    (format-id stx "~a-struct->definition" n #:source #'name #:props stx)
     #:with name?        (format-id stx "~a?" n #:source #'name #:props stx)

     #:do [(define o* (map syntax-e (syntax->list #'(options ...))))
           (define w/ (member '#:transparent o*))]
     #:with (+option ...) (if w/ #'(options ...) (cons (datum->syntax stx '#:prefab) #'(options ...)))
    
     #`(begin
         (struct name [key ...] +option ...)
         (define key* [list 'key ...])
         
         #; {Struct -> JSexpr}
         (define key+to* (map cons key* `[,to ...]))
         (define [name->jsexpr s-instance]
           (unless (name? s-instance)
             (error 'name->jsexpr "expected ~a instance, given ~a" 'name s-instance))
           (struct->jsexpr s-instance key+to*))

         #; {JSexpr -> Struct}
         (define [jsexpr->name j]
           (match j
             [(hash-table [(? (curry eq? 'key)) (app from keyv)] ...) (name keyv ...)]
             [(? jsexpr?)
              (eprintf "JSON value does not match ~a schema:\n ~a\n" 'name (jsexpr->string j))
              #false]
             [_
              (eprintf "non-JSON value does not match ~a schema:\n ~a\n" 'name j)
              #false]))
         
         #; {Struct -> ScribbleTable}
         (define t* (map (λ (k c) (list (~a k) c)) key*  `((,is-a ...) ...)))
         (define [name->def] (fields->data-def 'name t*)))]))

;                                     
;       ;                             
;       ;     ;            ;          
;       ;                  ;          
;    ;;;;   ;;;    ;;;   ;;;;;   ;;;  
;   ;; ;;     ;   ;;  ;    ;    ;   ; 
;   ;   ;     ;   ;        ;    ;     
;   ;   ;     ;   ;        ;     ;;;  
;   ;   ;     ;   ;        ;        ; 
;   ;; ;;     ;   ;;       ;    ;   ; 
;    ;;;;   ;;;;;  ;;;;    ;;;   ;;;  
;                                     
;                                     
;                                     

(define-syntax (define-object stx)
  (syntax-parse stx
    [(_ name
        [key value0
         (~optional (~seq #:to-jsexpr to #;function) #:defaults ([to #'identity]))
         (~optional (~seq #:from-jsexpr from #;function) #:defaults ([from #'identity]))
         (~optional (~seq #:is-a is-a ... #;string) #:defaults ([(is-a 1) null]))]
        ...)
     #:do   [(define n (syntax-e #'name))]
     #:with (keyv ...)   (generate-temporaries #'(key ...))
     #:with name-options (format-id stx "~a-options" n #:source #'name #:props stx)
     #:with name/c       (format-id stx "~a-object/c" n #:source #'name #:props stx)
     #:with default-name (format-id stx "default-~a-object" n #:source #'name #:props stx)
     #:with set-name     (format-id stx "set-~a-object" n #:source #'name #:props stx)
     #:with name->jsexpr (format-id stx "~a-object->jsexpr" n #:source #'name #:props stx)
     #:with jsexpr->name (format-id stx "jsexpr->~a-object" n #:source #'name #:props stx)
     #:with name->def    (format-id stx "~a-object->definition" n #:source #'name #:props stx)

     #:with state->dict (format-id stx "~a->dict" #'name #:source #'name #:props stx)
     #:with dict->state (format-id stx "dict->~a" #'name #:source #'name #:props stx)
     #`(begin
         (struct name [key ...] #:prefab)
         
         (define key (~a 'key)) ...
         (define name-options [list key ...])
         (define default-name (add-to 'default (hash) [list [list key value0] ...] "" name-options))

         #; {Contract}
         (define name/c [hash-carrier/c name-options])
         
         #; {(set-name c Key1 Value1 ... KeyN ValueN) : Void}
         (define (set-name object . key-value-pairs0)
           (define key-value-pairs (is-list-of-key-value-pairs key-value-pairs0))
           (add-to 'set-name object key-value-pairs key-value-pairs0 name-options))
           
         #; {object -> JSexpr}
         (define key*   `[,(normalize 'key) ...])
         (define g-key* `[,key ...])
         (define to*    `[,to ...])
         (define [name->jsexpr c] (object->jsexpr c key* g-key* to*))

         (define [jsexpr->name j]
           (match j
             [(hash-table
               [(? (curry eq? (normalize 'key))) (app from keyv)] ...)
              (add-to 'jsexpr (hash) [list [list key keyv] ...] "can't happen" name-options)]
             [_ (eprintf "JSON value does not match ~a schema:\n ~a\n" 'name jsexpr->string)
              #false]))
         
         #; {object -> ScribbleTable}
         (define t* 
           (for/list ([k key*] [c `((,is-a ...) ...)])
             (list (~a k) c)))
         (define [name->def] (fields->data-def 'name t*)))]))

#; {Symbol -> Symbol}
(define (normalize key)
  (string->symbol (string-downcase (~a key))))

#; {[Listof Any] -> [Option [Listof [List Symbol Any]]]}
(define (is-list-of-key-value-pairs key-value-pairs)
  (let loop ([key-value-pairs key-value-pairs] [h '()])
    (match key-value-pairs
      [(list) (reverse h)]
      [(list x) #false]
      [(list* k v key-value-pairs) (loop key-value-pairs (cons [list k v] h))])))

#; {Symbol object [Listof [List Symbol Any]] [Listof Any] [Listof Symbol] -> Congiguration}
(define (add-to tag object key-value-pairs key-value-pairs0 name-options)
  (cond
    [(false? key-value-pairs)
     (error tag "key-value pair expected; given ~a" key-value-pairs0)]
    [else 
     (for/fold ([h object]) ([kv-pair key-value-pairs])
       (match-define [list k v] kv-pair)
       (unless (member k name-options)
         (error tag "object key expected, given ~a" k))
       (dict-set h k v))]))

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

(module json racket
  (provide
   #; {object [Listof Symbol] [Listof Symbol] [Listof (Any -> JSexpr)] -> JSexpr}
   object->jsexpr

   struct->jsexpr 

   jsexpr->string)

  (require json)
  
  (define (struct->jsexpr s field-names+to*)
    (define values (rest (vector->list (struct->vector s))))
    (define j
      (for/fold ([h (hasheq)]) ([k+to field-names+to*] [v values])
        (match-define (cons k to) k+to)
        (dict-set h k (to v))))
    j)
  
  (define (object->jsexpr c key* g-key* to*)
    (for/fold ([h (hasheq)]) ([k key*] [g-key g-key*] [to to*])
      (dict-set h k (to (dict-ref c g-key))))))

(require 'json)

;                                                          
;                               ;      ;                   
;                           ;   ;      ;      ;;;          
;                               ;      ;        ;          
;    ;;;    ;;;    ;;;;   ;;;   ;;;;   ;;;;     ;     ;;;  
;   ;   ;  ;;  ;   ;;  ;    ;   ;; ;;  ;; ;;    ;    ;;  ; 
;   ;      ;       ;        ;   ;   ;  ;   ;    ;    ;   ;;
;    ;;;   ;       ;        ;   ;   ;  ;   ;    ;    ;;;;;;
;       ;  ;       ;        ;   ;   ;  ;   ;    ;    ;     
;   ;   ;  ;;      ;        ;   ;; ;;  ;; ;;    ;    ;     
;    ;;;    ;;;;   ;      ;;;;; ;;;;   ;;;;      ;;   ;;;; 
;                                                          
;                                                          
;                                                          

(module scribble racket
  (provide
   #; {Symbol [NEListof [List String [Cons String [Listof String]]]] -> ScribbleTable}
   ;; render the object specification as a scribble verbatim block
   fields->data-def
   
   table? #;"for testing")

  (require Bazaar/scribblings/shared)

  (define (fields->data-def name t*)
    (define the-defined (deftech (case-name name)))
    (define def-name    [list "A " the-defined (~a " is an object with " (length t*) " fields:")])
    (define definition  (splice-fields t* (blanks-needed t*) #true))
    (element-join (list def-name
                        (apply verbatim #:indent 4  (element-join definition ",\n")) "\n")))

  (define (case-name name)
    (define name* (string->list (~a name)))
    (define one (string-titlecase (string (first name*))))
    (~a one (apply string (rest name*))))
  
  (define (blanks-needed t*)
    (define mx (apply max (map string-length (map first t*))))
    (λ (s) (white (make-string (- mx (string-length s)) #\_))))

  (define (splice-fields t* blanks [start #false])
    (match t*
      ['() '()]
      [(list [list field [cons def explanation]])
       (list (1-field field def explanation start (blanks field) #true))]
      [(cons [list field [cons def explanation]] x*)
       (cons (1-field field def explanation start (blanks field) #false) (splice-fields x* blanks))]))

  (define space  @tt|{   { }|)
  (define (1-field field def explanation start blanks end)
    (let* ([s (if (empty? explanation) '() (list " (" explanation ")"))]
           [s [list* (tt (~s field)) blanks " :    " (tech def) s]]
           [s (if start (cons space s) (cons @white[space] s))]
           [s (if end (append s (list "  } ")) s)])
      s))

  (define (white s) @element[(make-style #false (list 'hspace (color-property "white"))) s]))

(require 'scribble)

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

(module+ test
  (provide  server-object->definition default-server-object)

  (require Bazaar/Lib/check-message)
  (require rackunit)
  (require json)

  (define-object server
    (PORT            0 #:is-a "Natural" "between 10000 and 60000")
    (SERVER-TRIES    1 #:is-a "Natural")
    (SERVER-WAIT     2 #:is-a "PositiveReal")
    (WAIT-FOR-SIGNUP 3 #:is-a "Natural")
    (REF-SPEC        4 #:to-jsexpr (λ (x) x) #:from-jsexpr (λ (x) x) #:is-a "RefSpec")
    (QUIET           5 #:is-a "Boolean" ))
  
  (check-equal? (server-PORT (server 1 2 3 4 5 6)) 1)
  
  (check-equal? [length server-options] 6)
  [check-equal? [contract? server-object/c] #true]
  [check-equal? (dict-ref default-server-object REF-SPEC) 4]
  [check-equal? (dict-ref (set-server-object default-server-object PORT 1 REF-SPEC 17) REF-SPEC) 17]

  (check-equal?
   (jsexpr->server-object (server-object->jsexpr default-server-object))
   default-server-object)

  (check-false
   (check-message
    "json" current-error-port #px"schema" 
    (jsexpr->server-object (dict-set (server-object->jsexpr default-server-object) 'A 11))))

  (check-true
   (jsexpr? (server-object->jsexpr (set-server-object default-server-object PORT 1 REF-SPEC 17))))

  [check-exn #px"key-value pair" (λ () (set-server-object default-server-object PORT 1 REF-SPEC))]

  #;
  (server-object->definition))


(module+ test

  (struct/description turn 
                      {cards #:to-jsexpr values}
                      {me #:to-jsexpr values}
                      {scores})

  (define b (turn 1 2 3))

  #;
  [turn-struct->definition]
  
  (check-true (jsexpr? (turn->jsexpr b)))
  (check-equal? (jsexpr->turn (turn->jsexpr b)) b))


