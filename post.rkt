#lang at-exp racket/base
(require scribble/base
         (for-syntax racket/base
                     unstable/syntax)
         (planet ryanc/scriblogify/scribble-util)
         scribble/lp)

(define yaml literal)
(define more (the-jump))

(define-syntax-rule (chunky . e)
  (chunk . e))

(define-syntax-rule (verbatim:codeblock e ...)
  (verbatim e ...))

(define-syntax (download-link stx)
  (syntax/loc stx (void)))

(provide (all-defined-out))
