#lang at-exp racket/base

(provide

 (all-from-out
  scribble/manual
  scribble/doclang)

 (rename-out
  [manual:#%module-begin #%module-begin]
  )

  tobe
  block
  block-name
  javascript
  lisp
  lnk
)

(require
 scribble/core
 scribble/manual

 (except-in scribble/doclang
            #%module-begin)
 (only-in scribble/manual/lang
          [#%module-begin manual:#%module-begin])
 (for-syntax racket/base syntax/parse)
 )

;; the dot '.' means variable number of arguments
(define (block name . items)
   (make-element
    (make-style #f null)
    (cons name items)))

(define (block-name name)
   (string-append name "\n"))

(define (javascript . items)
  (make-element
   (make-style #f null)
   items))

(define (lisp . items)
  (make-element
   (make-style #f null)
   items))

(define (lnk title new-line url)
  #;(printf "############ title: '~a'; url: '~a'\n" title url)
  (make-element
   (make-style #f null)
   (list title url)))

(define tobe "ToBe") ; this is a constant
