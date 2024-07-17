#lang racket

;; a data representation of (parameteric) equations

;; ---------------------------------------------------------------------------------------------------
(provide
 #; {type [Equation* X] = [Listof [1Equation X]]}
 #; {type [1Equation X]}


 #; {[Equations X] [Bag X] [Bag X] -> [Equations X]}
 #; (useful left-to-right my-wallet bank)
 ;; return those equations `e` in `left-to-right` for which `my-wallet` has
 ;; enough Xs to swap one side and `bank`has enough Xs for the other;
 ;; orient the resulting equations so that `my-wallet` covers the left
 useful
 
 #; {[1Equation X] [X -> Pict] -> Pict}
 #; (render e render-x)
 ;; returns an image of `e` where `render-x` is used to render individual Xs
 render

 #; {[Bag X] [Bag X] -> 1Equation}
 1eq)

(module+ examples
  (provide EQ1 EQ1-rev)
  (provide EQ1* EQ1-rev*))

;; ---------------------------------------------------------------------------------------------------
;; dependencies 

(require Bazaar/Common/bags)
(require pict)

(module+ examples
  (require (prefix-in p: Bazaar/Common/pebbles)))

(module+ test (require rackunit))
(module+ test (require (submod ".." examples)))

;; ---------------------------------------------------------------------------------------------------
;; data definitions and examples 

(struct 1eq [left right] #:prefab)
#; {type Equation = (1eq Side Side)}
#; {type Side     = b:Bag || (<= 1 (bag-size b) 4)}

(module+ examples 
  #; 1Equation 
  (define EQ1 [1eq '[1 1 2] '[3]])
  (define EQ1-rev [1eq '[3] '[1 1 2]])

  #; [Listof 1Equatin]
  (define EQ1* [list EQ1])
  (define EQ1-rev*  [list EQ1-rev])

  (provide WALLET2 BANK2 EQ2) 

  (define WALLET2` [,p:RED ,p:GREEN])
  (define BANK2   `[,p:BLUE ,p:BLUE ,p:BLUE ,p:BLUE])
  (define EQ2      (1eq  WALLET2 BANK2))

  (render EQ2 p:render))

(module+ examples
  #; {[Listof [List ActualArguments ExpectedResult Message]]}
  (provide ForStudents/ Tests/)

  (define-syntax-rule (scenario+ kind actual expected msg)
    (set! kind (append kind (list [list actual expected msg]))))
  
  (define ForStudents/ '[])
  (scenario+ ForStudents/ `[,(list EQ2) ,WALLET2 ,BANK2] (list EQ2) "left to right, not vv")
  
  (define Tests/ '[]))

;; ---------------------------------------------------------------------------------------------------
;; functionality 

(define (useful left-to-right my-wallet bank)
  (define right-to-left (map 1eq-flip left-to-right))
  (for/fold ([result '()]) ([e (append left-to-right right-to-left)]
                            #:when (can-swap? e my-wallet bank))
    (cons e result)))

#; {1Equation Bag Bad -> Boolean}
;; can `my-wallet` swap with `bank` according to `e`? 
(define (can-swap? e my-wallet bank)
  (and (subbag? (1eq-left e) my-wallet) (subbag? (1eq-right e) bank)))

#; {1Equation -> 1Equation}
(define (1eq-flip e)
  (1eq (1eq-right e) (1eq-left e)))

;; ---------------------------------------------------------------------------------------------------
;; graphical representation 

(define (render 1eq render-element)
  (define left (apply hc-append 2 (map render-element (1eq-left 1eq))))
  (define right (apply hc-append 2 (map render-element (1eq-right 1eq))))
  (hc-append 5 left (text "=") right))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (render EQ1 (λ (x) (text (~a x))))

  (define my-wallet '[1 1 2 3])
  (define bank      '[1 1 2 3])
  (define low-bank  '[3])

  (check-equal? (useful [list [1eq '[1 1 2] '[3]]] my-wallet bank) (append EQ1-rev* EQ1*))
  
  (check-true (can-swap? EQ1 my-wallet low-bank))
  (check-equal? (useful [list [1eq '[1 1 2] '[3]]] my-wallet low-bank) EQ1*))

(module+ test
  (define (equation-tests scenario*)
    (for ([s scenario*])
      (match-define `[,actual ,expected ,msg] s)
      (check-equal? (apply useful actual) expected msg)))

  (equation-tests ForStudents/))