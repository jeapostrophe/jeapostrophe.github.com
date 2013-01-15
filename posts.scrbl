#lang scribble/base

@(begin
   (require (for-syntax racket/base)
            scribble/manual
            racket/file
            "post-help.rkt")

   (begin-for-syntax
     (require racket/runtime-path
              racket/file
              syntax/strip-context
              "post-help.rkt"))

   (define-syntax (include-posts stx)
     (syntax-case stx ()
       [(_)
        (with-syntax
            ([(post-inc ...)
              (for/list ([p (in-list (sort (directory-list posts-path)
                                           string-ci<=?
                                           #:key path->string))])
                (define ps (path->string p))
                (define tag (filename->tag ps))
                (define full-p (build-path posts-path p))
                (cond
                  [(and tag (file-exists? full-p))
                   (define the-path
                     (cond
                       [(regexp-match #rx"rkt" (path->string p))
                        (define scrbl-p 
                          (format ".auto.~a.scrbl" 
                                  (path->string p)))
                        (define the-p (build-path posts-path scrbl-p))
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

@title[#:tag "archive" #:style '(toc)]{Archive}

@(for/list ([ps (in-list (all-posts))])
   (define tag (filename->tag ps))
   @t{@secref[tag]})

@(include-posts)
