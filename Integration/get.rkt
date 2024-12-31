#lang racket

;; a function for retrieving a possibly optional JSON value from STDIN 

(provide
 ILL ;; ill-formed JSON 
 INV ;; invalid JSON 
 
 #; {[JSexpr -> X ...] Symbol String -> X ...}
 #; (get validator tag msg #:eof-okay? Boolean #:empty-okay? Boolean #:dont-exit Boolean)
 #; [validator     :: (JSExpr -> (values X ...))]
 #; [tag           :: the function that actually raises the error]
 #; [msg           ;; the msg when the JSON is INValid or ILLformed ]
 #; [#:eof-okay?   :: the desired value is optional]
 #; [#:empty-okay? :: the validator may return '()]
 #; [#:dont-exit   :: just signal an exn that the caller is allowed to catch]
 get)

;; ---------------------------------------------------------------------------------------------------
(require SwDev/Testing/communication)

;; ---------------------------------------------------------------------------------------------------
(define ILL "ill-formed JSON")
(define INV "invalid JSON: ")

;; read a JSON value from current input port, validate and map to internal data ~~ or (error tag msg)
(define (get validator tag msg #:eof-okay? (eof? #f) #:empty-okay? (eok? #f) #:dont-exit (no-exit #f))
  (define x (read-message))
  (cond
    [(eof-object? x)
     (unless eof? (stop tag no-exit "missing JSON"))]
    [(and (string? x) (regexp-match #px"ERROR" x))
     (stop tag no-exit "~a:\n ~a" ILL x)])
  (define pieces (call-with-values (λ () (validator x)) list))
  (unless (or eok? (first pieces))
    (pretty-print x (current-error-port))
    (stop tag no-exit "~a ~a: see above" INV msg))
  (apply values pieces))

#; {Symbol Boolean String Any .. -> Empty}
(define (stop tag no-exit base . x)
  (send-message (string-append (~a tag) ": things went wrong"))
  (apply eprintf (format "~a: ~a\n" tag base) x)
  (if no-exit (apply error tag (format "~a: ~a\n" tag base) x) (exit 1)))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require rackunit)
  (check-equal?
   (with-output-to-string
     (λ ()
       (check-exn exn:fail?
                  (λ ()
                    (with-input-from-string "" (λ () (get values "test" "mt" #:dont-exit #true)))))))
   "\"test: things went wrong\"\n"))
