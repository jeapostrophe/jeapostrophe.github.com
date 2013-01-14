#lang scribble/base

@(begin
   (require (for-syntax racket/base))

   (begin-for-syntax
     (require racket/runtime-path
              racket/file
              syntax/strip-context)
     (define-runtime-path posts-path "posts")
     (define-runtime-path lp-posts-path "lp-posts")
     (make-directory* lp-posts-path)
     (unless (link-exists? (build-path lp-posts-path "post.rkt"))
       (make-file-or-directory-link
        (build-path posts-path "post.rkt")
        (build-path lp-posts-path "post.rkt"))))

   (define-syntax (include-posts stx)
     (syntax-case stx ()
       [(_)
        (with-syntax
            ([(post-inc ...)
              (for/list ([p (in-list (sort (directory-list posts-path)
                                           string-ci<=?
                                           #:key path->string))])
                (define full-p (build-path posts-path p))
                (cond
                  [(file-exists? full-p)
                   (define the-path
                     (cond
                       [(regexp-match #rx"rkt" (path->string p))
                        (define scrbl-p (path-add-suffix p #".scrbl"))
                        (define the-p (build-path lp-posts-path scrbl-p))
                        (with-output-to-file the-p
                          #:exists 'replace
                          (λ ()
                            (printf "#lang scribble/base\n")
                            (printf "@(require scribble/lp-include)\n")
                            (printf "@lp-include[(file ~v)]\n"
                                    (path->string full-p))))
                        the-p]
                       [else
                        full-p]))
                   (define path-string
                     (path->string the-path))
                   (replace-context
                    stx
                    (quasisyntax/loc stx
                      (include-section (file #,path-string))))]
                  [else
                   #'(void)]))])
          (syntax/loc stx
            (begin post-inc ...)))])))

@title{Jay McCarthy}
@emph{'Cowards die many times before their deaths, The valiant never taste of death but once.'}

@(include-posts)
