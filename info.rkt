#lang info

(define collection "Bazaar")

(define deps
  (list
    "htdp-lib"
    "SwDev"
    "rackunit-lib"
    "gui-lib"
    "base"))

(define build-deps
  (list
    "at-exp-lib"
    "scribble-abbrevs"
    "scribble-lib"
    "typed-racket-lib"
    "sandbox-lib"
    "racket-doc"
    "rackunit-lib"))

(define scribblings '(("scribblings/bazaar.scrbl" ())))

(define pkg-desc "A Bazaar Game Implementation")

(define version "0.2")

(define pkg-authors '(matthias))

(define license '(Apache-2.0 OR MIT))
