#lang scribble/manual

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
               (cons (quasisyntax/loc stx @subsection[#:tag #,ps #:style 'unnumbered #,ps])
                     (map (λ (tag) (quasisyntax/loc stx
                                     @t{@postref[#,tag]}))
                          tags))))))))

@title[#:version "" #:tag "top" #:style (google-analytics)]{Jay McCarthy}

@centered{@emph{``Cowards die many times before their deaths, @(linebreak) The valiant never taste of death but once.''}}

This is the blog of @link["http://jeapostrophe.github.io/home/"]{Jay
McCarthy}. I am an associate professor at
@link["http://www.uml.edu"]{UMass Lowell} in the
@link["http://www.uml.edu/Sciences/computer-science/default.aspx"]{Computer
Science Department}. I work on @link["http://racket-lang.org/"]{the
Racket programming language}.

@itemize[

@item{For course and other teaching information, visit my
@link["http://jeapostrophe.github.io/home/"]{home page}.}

@item{For publications and other professional activities, visit my
@link["http://jeapostrophe.github.io/home/"]{home page}.}

@item{For software projects, visit my
@link["http://www.github.com/jeapostrophe"]{GitHub page}.}

@item{For pithy remarks, follow me on Twitter:
@link["http://twitter.com/jeapostrophe"]{@"@"jeapostrophe}.}

@item{For older posts, browse by @seclink["categories"
#:tag-prefixes (list "cat-")]{category} or @seclink["archive"]{date}.}

@item{For continual updates, subscribe to @link["atom.xml"]{my Atom feed}.}

]

My last @(number->string RECENT-POSTS) posts were:
@itemize[(for/list ([ps (in-list (all-posts))]
                    [i (in-range RECENT-POSTS)])
           (item (postref (filename->tag ps))))]

@section[#:tag "categories" #:tag-prefix "cat-" #:style 'unnumbered]{Categories}

@(include-categories)

@include-section["posts.scrbl"]
