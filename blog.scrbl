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

   (define-syntax (include-categories stx)
     (quasisyntax/loc stx
       (begin
         #,@(apply 
             append
             (for/list ([p (in-list (sort (directory-list categories-path)
                                          string-ci<=?
                                          #:key path->string))])
               (define ps (path->string p))
               (define tags (file->value (build-path categories-path p)))
               (cons (quasisyntax/loc stx @subsection[#:tag #,ps #,ps])
                     (map (λ (tag) (quasisyntax/loc stx
                                     @t{@secref[#,tag]}))
                          tags))))))))

@title{Jay McCarthy}

@centered{@emph{'Cowards die many times before their deaths, @(linebreak) The valiant never taste of death but once.'}}

This is the blog of Jay McCarthy, an assistant professor at
@link["http://byu.edu/"]{Brigham Young University} in the
@link["http://cs.byu.edu/"]{Computer Science Department}. I work on
@link["http://racket-lang.org/"]{the Racket programming language}.

If you are a student looking for information on my classes, or someone
looking for my publications, please visit my
@link["http://faculty.cs.byu.edu/~jay/home/"]{home page}.

If you'd like to read older posts, you may like to browse my
@seclink["categories"]{category list} or
@seclink["archive"]{complete archives}.

@(define RECENT-POSTS 8)

My last @(number->string RECENT-POSTS) posts were:
@itemize[(for/list ([ps (in-list (all-posts))]
                    [i (in-range RECENT-POSTS)])
           (item (secref (filename->tag ps))))]

@section[#:tag "categories"]{Categories}

@(include-categories)

@include-section["posts.scrbl"]
