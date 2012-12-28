#lang scribble/base

@(begin
   (require (for-syntax racket/base)
            scribble/lp-include)
   ;; XXX subtitle
   (define (subtitle . a) (void))

   (begin-for-syntax
     (require racket/runtime-path)
     (define-runtime-path posts-path "posts"))

   (define-syntax (include-posts stx)
     (syntax-case stx ()
       [(_)
        (with-syntax
            ([(post-inc ...)
              (for/list ([p (in-list (directory-list posts-path))]
                         [i (in-range 1)]
                         #:when (file-exists? (build-path posts-path p)))
                (with-syntax ([include-it
                               (if (regexp-match #rx"rkt" (path->string p))
                                 #'lp-include
                                 #'include-section)])
                  (quasisyntax/loc stx
                    (include-it
                     (file
                      #,(path->string (build-path posts-path p)))))))])
             (syntax/loc stx
               (begin (begin (begin-for-syntax (printf "~v\n" 'post-inc))
                             post-inc)
                      ...)))])))

@title{Jay McCarthy}
@subtitle{'Cowards die many times before their deaths, The valiant never taste of death but once.'}

@(include-posts)
