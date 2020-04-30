#lang info
(define collection "rascas")
(define deps '("base"
               "math-lib"
               "parser-tools-lib"
               "rackunit-lib"
               "srfi-lite-lib"
               "plot-gui-lib"
               "plot-lib"
               "sandbox-lib"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "rackunit-lib"
                     "data-lib"))
(define scribblings '(("scribblings/rascas.scrbl" ())))
(define pkg-desc "Computer Algebra System for Racket. Port of dharmatech/mpl.")
(define version "0.0")
(define pkg-authors '(orseau))
