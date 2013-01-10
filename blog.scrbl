#lang scribble/base

@(begin
   (require (for-syntax racket/base)
            scribble/lp-include)
   ;; XXX subtitle
   (define (subtitle . a) (void))

   (begin-for-syntax
     (require racket/runtime-path
              syntax/strip-context)
     (define-runtime-path posts-path "posts"))

   (define-syntax (include-posts stx)
     (syntax-case stx ()
       [(_)
        (with-syntax
            ([(post-inc ...)
              (for/list ([p (in-list (directory-list posts-path))]
                         #:when (file-exists? (build-path posts-path p)))
                (with-syntax ([include-it
                               (if (regexp-match #rx"rkt" (path->string p))
                                 #'lp-include
                                 #'include-section)])
                  (replace-context
                   stx
                   (quasisyntax/loc stx
                     (include-it
                      (file
                       #,(path->string (build-path posts-path p))))))))])
          (syntax/loc stx
            (begin post-inc ...)))])))

@title{Jay McCarthy}
@subtitle{'Cowards die many times before their deaths, The valiant never taste of death but once.'}

Something before

@(include-posts)

@(begin-for-syntax (printf "after all\n"))

Something after

@section{Test}

Test
