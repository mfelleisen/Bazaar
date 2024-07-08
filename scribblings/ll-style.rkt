#lang at-exp racket

;; this would be a beautiful example for Cameron's effort to implement Alexis's protocol 

(provide
 (contract-out
  [ll-style
   (->i ()
        ;; for LaTeX only, the following optional argument splits a table across 2 pages 
        (#:break-into-2 [break (s) (or/c #false natural?)])
        #:rest [s list? #;[listof string?]]
        [r any/c])]))

#; { SYNTAX
 @ll-style{
  Q & A |
  ...
  Q & A |
  }} ;; draws bottom border in latex

#; { SYNTAX
 @ll-style{
  Q & A |
  ...
  Q & A 
  }} ;; does not


;; ---------------------------------------------------------------------------------------------------
(require "shared.rkt")
(require (only-in scribble/manual para emph nested tabular tt t hspace))
(require scribble/html-properties)
(require scribble/core)
(require scribble/decode)

;; ---------------------------------------------------------------------------------------------------
(define (ll-style #:break-into-2 [break #false] . qs-and-as)
  (define-values [no-bottom cells] (seq->cells/any qs-and-as))
  (cond
    [else ;; HTML 
     (define cell-style (attributes '[(valign . "top") (width . "280") (style . "padding: 10px;")]))
     @tabular[#:row-properties '(bottom-border)
              #:cell-properties (for/list ((_ cells)) (list cell-style cell-style))
              #:style 'boxed
              cells]]))

#; {[Listof X] -> [Values Boolean [Listof [List [Listof X] [Listof X]]]]}
(define (seq->cells/any s)
  (define bottom? #false)
  (when (and (string? (last s)) (regexp-match #px"\\|$|\\|\n$" (last s)))
    (set! bottom? #true))
  (let* ([s (map (λ (x) (if (equal? "\n" x) " " x)) s)]
         [s (element-split s "|" #true)]
         [s (map (λ (x) (element-split x "&" #false)) s)]
         [s (filter (match-lambda [(list '("")) #f][(list '[] '[]) #f][(list a b c d ...) (error "can't happen")] [_ #t]) s)]
         [s (map (match-lambda [(and x (list q a)) x] [(list q) `(,q [,(tt " ")])]) s)]
         [s (map (match-lambda [(list q a) (list (decode* q) (decode* a))]) s)])
    (values (not bottom?) s)))

(define (decode* lox)
  (for/fold ([sofar '()]) ([x lox])
    (if (string? x) (append sofar (decode-string x)) (append sofar (list x)))))

#; {[Listof Element] PatternString Boolean -> [Listof [Listof Element]]}
;; find strings that match s, then split the list in two there
(define (element-split l0 s repeat?)
  (let loop ([l l0] [x '()] [afirst #false])
    (cond
      [(empty? l) (if (empty? x) x (list (reverse x)))]
      [else (define one (first l))
       (define rst (rest l))
       (cond
         [(and (string? one) (string-contains? one s))
          (when afirst
            (unless repeat?
              (eprintf "ERROR: repeated separator ~v not allowed in:\n  ~v\n" s (first l))
              (error 'element-split "repeated separator not expected")))
          (match-define [list a b] (string-split one s #:trim? #false))
          (define line (reverse (cons a x)))
          (cons line (loop rst (list b) #true))]
         [else
          (loop rst (cons one x) afirst)])])))

#; {[Listof [List String String]] (U False N) -> Tabular}
(define (render-latex cells no-bottom)
  (define separator (list @element[exact "\\rule{.8\\textwidth}{.2pt}"] 'cont))
  (let* ([s cells]
         [s (map (match-lambda [(list q a) [list (make-cell "llq" q) (make-cell "lla" a)]]) s)]
         [s (for/fold ([t '()]) ([line s]) (list* line separator t))]
         [s (reverse (if no-bottom s (cons separator s)))])
    @nested[#:style 'inset]{@tabular[#:row-properties '(top) s]}))

#; {StyleString String -> Para}
(define (make-cell s x)
  (para #:style (style s '[]) x))

(define exact (style #f '(exact-chars)))
    
@; ---------------------------------------------------------------------------------------------------

(module+ test
  (require SwDev/Testing/check-values)
  (require (only-in scribble/manual tech))

  (define (tst . l) [seq->cells/any l])

  (check-values @tst{a b & c | d e & f}
                #true
                '[[["a b"] ["c"]]
                  [["d e"] ["f"]]]
                "simple"))
