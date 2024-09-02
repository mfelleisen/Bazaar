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

    #; {[[Instanceof Observer%] -> X] -> Void}
    ;; apply `do` to all *observers
    ;; EFFECT eliminate those for which `(xcall do o)` fails 
    (define/private (send-all do)
      (let all ([observers *observers] [survivors '()])
        (match observers 
          [(list)
           (set! *observers survivors)]
          [(cons first others)
           (match (xcall do first)
             [(or (? string?) (? failed?)) (all (remove* `[,first] others) survivors)]
             [_                            (all others (cons first survivors))])])))

    #; {[Instanceof Observer%] -> Void}
    (define/public (add* o*)
      (set! *observers (append o* *observers)))

    #; {[Listof String] [Listof String] -> Void}
    (define/public (end winners drop-outs)
      (send-all (λ (o) (send o end winners drop-outs))))

    #; {Any GameState -> Void}
    (define/public (state msg equstions action gs)
      (send-all (λ (o) (send o state msg equstions action gs))))))