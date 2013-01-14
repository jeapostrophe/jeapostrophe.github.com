#lang racket/base
(require (for-syntax racket/base
                     racket/match
                     syntax/parse
                     unstable/syntax)
         (prefix-in sb: scribble/base))

(begin-for-syntax
  (define (string-take* s n)
    (list->string
     (for/list ([c (in-string s)]
                [i (in-range n)])
       c))))

(define-syntax (title stx)
  (syntax-parse stx
    [(_ content ...)
     (define fname (path->string (syntax-source-file-name stx)))
     (match-define (regexp #rx"^(....)-(..)-(..)-(.*)\\.(rkt|scrbl)$"
                           (list _ year month day code ext))
                   fname)
     (with-syntax ([tag (format "~a-~a-~a-~a" year month day (string-take* code 8))])
       (quasisyntax/loc stx
         (sb:title
          #:tag tag
          (format "~a-~a-~a: " #,year #,month #,day)
          content ...)))]))
(define (categories . _) (void))
(define (the-jump . _) (void))

(provide (all-defined-out))
