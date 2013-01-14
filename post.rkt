#lang at-exp racket/base
(require (for-syntax racket/base
                     racket/match
                     syntax/parse
                     unstable/syntax)
         (prefix-in sb: 
                    (combine-in scribble/base
                                scribble/manual)))

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
     (with-syntax 
         ([(fname year month day code) (list fname year month day code)]
          [tag (format "~a-~a-~a-~a" year month day (string-take* code 8))])
       (quasisyntax/loc stx
         (begin
           (sb:title
            #:tag tag
            (format "~a-~a-~a: " year month day)
            content ...)
           @sb:margin-note{The source for this post is online at @sb:link[(format "https://github.com/jeapostrophe/jeapostrophe.github.com/tree/source/posts/~a" fname)]{@|fname|}.})))]))

(define (categories . l)
  @sb:t{@sb:bold{Categories:} @(map (λ (c) @sb:elem{@sb:secref[c] }) l)})

(define (the-jump . _) (void))

(provide (all-defined-out))
