#lang racket

;; this file exists to export names for the milestone specifications of the Sw Dev course 

(require (for-syntax syntax/parse))

(define-syntax (req->prov stx)
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
    [(_ spec)
     #:with prov #'(all-from-out spec)
     #'(begin
         (require spec)
         (provide prov))]))

;; the specs 
(req->prov Bazaar/scribblings/spec)

;; code files
(req->prov (only-in Bazaar/Common/cards card-struct->definition))
(req->prov (only-in Bazaar/Common/player player-struct->definition))
(req->prov (only-in Bazaar/Common/turn-state turn-struct->definition))
(req->prov (only-in Bazaar/Referee/game-state game-struct->definition))
(req->prov Bazaar/Player/strategies)
(req->prov (submod Bazaar/Player/strategies json))
(req->prov (submod Bazaar/Common/player json))
(req->prov Bazaar/Player/mechanism)

