#lang info

(define collection "notes")
(define deps '(
  "at-exp-lib"
  "base"
  "html-parsing"
  "math-lib"
  "pict-lib"
  "plot-lib"
  "scribble-lib"
  "slideshow-lib"
  "sxml"
  "with-cache"
))
(define build-deps '(
  "pict-doc"
  "racket-doc"
  "rackunit-abbrevs"
  "rackunit-lib"
  "scribble-doc"
))
(define pkg-desc "Parse notes")
(define version "0.1")
(define pkg-authors '(Rostislav Svoboda))
;; no documentaion needed right now
#;(define scribblings '(("docs/notes.scrbl" ())))
