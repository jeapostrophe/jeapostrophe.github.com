#lang at-exp racket/base
(require (for-syntax racket/base
                     racket/match
                     syntax/parse
                     unstable/syntax
                     "post-help.rkt")
         racket/file
         racket/list
         "post-help.rkt"
         (prefix-in sb: 
                    (combine-in scribble/base
                                scribble/manual)))

(define-syntax (title stx)
  (syntax-parse stx
    [(_ content ...)
     (define fname (path->string (syntax-source-file-name stx)))
     (match-define (regexp #rx"^(....)-(..)-(..)-(.*)\\.(rkt|scrbl)$"
                           (list _ year month day code ext))
                   fname)
     (with-syntax 
         ([(fname year month day code) (list fname year month day code)]
          [tag (filename->tag fname)])
       (quasisyntax/loc stx
         (begin
           (sb:title
            #:tag tag
            (format "~a-~a-~a: " year month day)
            content ...)
           (current-tag tag)
           @sb:margin-note{The source for this post is online at @sb:link[(format "https://github.com/jeapostrophe/jeapostrophe.github.com/tree/source/posts/~a" fname)]{@|fname|}.})))]))

(define current-tag (make-parameter #f))

(define (categories . l)
  (define (per-cat c)
    (define cat-path (build-path categories-path c))
    (define old
      (if (file-exists? cat-path)
        (file->value cat-path)
        empty))
    (define new
      (sort 
       (remove-duplicates 
        (cons (current-tag) old))
       string-ci>?))
    (write-to-file new cat-path
                   #:exists 'replace)
    @sb:elem{@sb:secref[c] })

  @sb:t{@sb:bold{Categories:} @(map per-cat l)})

(define (the-jump . _) @sb:centered{-})

(provide (all-defined-out))
