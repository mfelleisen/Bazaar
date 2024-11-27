#lang racket

;; a library for specifying the methods of remote proxies via "types" (define/remote for classes)

;; turns the signature line for a player method into a method that
;; serializes arguments, performs TCP-based exchanges, and deserializes the result 

(provide
 #; (define-define/remote define/remote:id in out)
 ;; given an input port and an output port, the macro defines a macro that creates two new forms

 #; (define/remote (m [->to:id]) <-from:id)
 ;; is a public method `m` that consumes a collection that is converted to JSexpr with `->to`
 ;; and whose result term is converted back from JSexpr to an internal data structure with `<-from`

 #; (define/remote (m ->to ...) <-from)
 ;; is a public method `m` that consumes `(->to ...) arguments, converting each to JSexpr with `->to`
 ;; and whose result term is converted back from JSexpr to an internal data structure with `<-from`

 ;; The remote calls have the following JSexpr shape:
 #; ["method name" [argument ...]]
 ;; Note the required parentheses around `argument`. 

 ;; the `<-from` conversion is supposed to fail with a `misc:match` exception if the JSexpr is
 ;; ill-formed (wrt to the spec; it is JSON)

 

 define-define/remote)

;; ---------------------------------------------------------------------------------------------------
;; dependencies 

(require SwDev/Testing/communication)
(require (for-syntax syntax/parse))


;; ---------------------------------------------------------------------------------------------------
;; the macro 

(define-syntax-rule (define-define/remote define/remote in out)
  (define-syntax (define/remote stx)
    (define ff #'#f)
    ;; false okay, then allow (read-message in) to return #false
    (syntax-parse stx
      [(_ (m [->to:id]) <-from:id  (~optional (~seq #:#f-okay ff-okay?) #:defaults ([ff-okay? ff])))
       #:with ->to-j  (->jsexpr #'->to)
       #:with p<-from (parse- #'<-from) 
       #'(define/public (m x)
           (call-via-json in out 'm `[,(~s 'm) [,(for/list ([y x]) (->to-j y))]] p<-from ff-okay?))]
      
      [(_ (m ->to (... ...)) <-from (~optional (~seq #:#f-okay ff-okay?) #:defaults ([ff-okay? ff])))
       #:with (->to-j (... ...))  (map ->jsexpr (syntax->list #'(->to (... ...))))
       #:with p<-from (parse- #'<-from)
       #:with (x (... ...)) (generate-temporaries #'(->to (... ...)))
       #`(define/public (m x (... ...))
           (call-via-json in out 'm `[,(~s 'm) [,(->to-j x) (... ...)]] p<-from ff-okay?))])))

;; ---------------------------------------------------------------------------------------------------
;; compile-time helpers

#; {ID:Syntax String String -> ID:Syntax}
(define-for-syntax (pre-suf ->to-stx #:prefix [pre ""] #:suffix [suf ""])
  (define sy (syntax-e ->to-stx))
  (define to (string->symbol (format "~a~a~a" pre sy suf)))
  (define rr (datum->syntax ->to-stx to ->to-stx ->to-stx))
  rr)

(define-for-syntax (->jsexpr id:stx) (pre-suf id:stx #:suffix "->jsexpr"))
(define-for-syntax (parse- id:stx) (pre-suf id:stx #:prefix "jsexpr->"))

;; ---------------------------------------------------------------------------------------------------
;; run-time helpers

(define (call-via-json in out tag json <-from false-okay?)
  (send-message json out)
  (define msg (read-message in))
  (cond
    [false-okay? (<-from msg)]
    [(<-from msg) => identity]
    [else
     (define msg
       (with-output-to-string
         (λ ()
           (printf "server received ILL-FORMED/INVALID JSON or the call timed out:\n")
           (pretty-print msg))))
     (raise msg)]))

(module+ test
  (define-define/remote define/remote 'in 'out))
