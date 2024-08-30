#lang racket

;; a tool for managing observes via looping over registered observers and `xsend`

(provide
 #;{type MO = (instanceof/c manage-observers%)}

 #; (class/c
     [add*  (->m (listof any/c) any)]
     [end   (->m (listof string?) (listof string?) any)]
     [state (->m (or/c symbol? string?) gs:game? any)])
 manage-observers%)

;; ---------------------------------------------------------------------------------------------------
(require Bazaar/Lib/xsend)

;; ---------------------------------------------------------------------------------------------------
(define manage-observers%
  (class object%
    (super-new)
    (define *observers (list))

    (define/public (add* o*)
      (set! *observers (append o* *observers)))

    (define/public (end winners drop-outs)
      (for ([o *observers])
        (xsend o end winners drop-outs)))

    (define/public (state msg gs)
      (for ([o *observers])
        (xsend o state msg gs)))))