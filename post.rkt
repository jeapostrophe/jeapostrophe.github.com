#lang at-exp racket/base
(require (for-syntax racket/base
                     racket/match
                     syntax/parse
                     unstable/syntax
                     "post-help.rkt")
         racket/file
         racket/list
         racket/format
         racket/match
         "post-help.rkt"
         (prefix-in sb: 
                    (combine-in scribble/base
                                scribble/core
                                scribble/html-properties
                                scribble/manual
                                scribble/decode)))

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
            #:style (google-analytics 'toc-hidden 'unnumbered)
            #:tag "post"
            #:tag-prefix (format "~a-" tag)
            #:version ""
            (format "~a-~a-~a: " year month day)
            content ...)
           (display-to-file 
            (~a content ...)
            (build-path titles-path tag)
            #:exists 'replace)
           (sb:author
            @sb:secref["top"])
           (current-finfo (list year month day code))
           (current-tag tag)
           @sb:margin-note{The source for this post is online at @sb:link[(format "https://github.com/jeapostrophe/jeapostrophe.github.com/tree/source/posts/~a" fname)]{@|fname|}.})))]))

(define current-finfo (make-parameter #f))
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
    @sb:elem{@catref[c] })

  @sb:t{@sb:bold{Categories:} @(map per-cat l)})

(define (the-jump . _) @sb:centered{-})

(define DISQUS_SHORTNAME "jaymccarthy")

(define (the-end . _)
  (match-define (list year month day code) (current-finfo))
  (define src-filename code)
  (define actual-filename (format "~a-post.html" (current-tag)))
  (sb:element
   (sb:style #f (list (sb:alt-tag "div")
                      (sb:attributes 
                       (list (cons 'id "disqus_thread")))))
   ;; XXX add script
   (sb:element 
    (sb:style 
     #f 
     (list 
      (sb:script-property 
       "text/javascript"
       (list (format "var disqus_shortname = '~a';\n" DISQUS_SHORTNAME)
             (format "var disqus_identifier = '~a/blog/~a/~a/~a/~a/';\n"
                     *BLOG-URL* year month day src-filename)
             (format "var disqus_url = '~a/~a';\n"
                     *BLOG-URL* actual-filename)
             "var disqus_script = 'embed.js';\n"
             "(function () {\n"
             "var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;\n"
             "dsq.src = 'http://' + disqus_shortname + '.disqus.com/' + disqus_script;\n"
             "(document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);\n"
             "}());\n"))))
   (list "Please enable JavaScript to view the "
         "comments powered by Disqus."))))

(require racket/path
         scribblings/quick/keep)
(define (keeplink file . more)
  (define the-link 
    (format "~a" (file-name-from-path file)))
  (apply sb:link the-link (keep-file file) more))

(provide (all-defined-out))
