#lang racket

;; a mechasnism for importing and re-exporting libraries 

(provide
 #; SYNTAX
 #; (require->provide spec)
 ;; where spec is one of: 
 #; (prefix-in p module-path)
 #; (only-in p module-path)
 #; (except-in p module-path)
 #; module-path
 ;; and the result is a require and provide all 
 require->provide)

(require (for-syntax syntax/parse))

(define-syntax (require->provide stx)
  (syntax-parse stx
    [(_ ((~literal prefix-in) x spec))
     #:with prov #'(all-from-out spec)
     #'(begin
         (require (prefix-in x spec))
         (provide prov))]
    [(_ ((~literal only-in) spec x ...))
     #:with prov #'(all-from-out spec)
     #'(begin
         (require (only-in spec x ...))
         (provide prov))]
    [(_ ((~literal except-in) spec x ...))
     #:with prov #'(all-from-out spec)
     #'(begin
         (require (except-in spec x ...))
         (provide prov))]
    [(_ spec)
     #:with prov #'(all-from-out spec)
     #'(begin
         (require spec)
         (provide prov))]))
