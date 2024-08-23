#lang racket

;; a primitive addition to rackunit that checks whether an expression prints an error message
;; matching some regular expression

(provide
 ;; SYNTAX
 #; (check-message msg port re body ...)
 ;; returns the result of `body ...`
 ;; captures all output of `body ...` on `port` and regexp-matches against `re`
 check-message)

;; -----------------------------------------------------------------------------
(require rackunit)

;; -----------------------------------------------------------------------------
(define-syntax-rule (check-message msg port rgxp body ...)
  (let ([os  (open-output-string)]
        [err #false]
        [mg  (~a "looking for " rgxp " in " msg)])
    (begin0
      (parameterize ([port os])
        (with-handlers ([exn:fail? (λ (xn) (set! err xn))])
          body ...))
      (close-output-port os)
      (let* ([out (get-output-string os)]
             [ok? (cons? (regexp-match rgxp out))])
        (check-true ok? (~a "pattern matches: " msg))
        (unless ok?
          (eprintf "output string is: \n")
          (fprintf (current-error-port) out)
          (eprintf "instead of: \n")
          (fprintf (current-error-port) rgxp)
          (eprintf "\n------------------\n")))
      (when err
        (unless (regexp-match #px"div" msg)
          (eprintf "WARNING: check-message: exn intercepted in ~a:\n" msg)
          (fprintf (current-error-port) (exn-message err))
          (eprintf "\n------------------\n"))
        (raise err)))))

(module+ test
  (check-message "xxx" current-output-port #px"hello" (printf "hello")))
