#lang at-exp racket/base
(require racket/runtime-path
         racket/file
         racket/match
         scribble/base
         scribble/tag
         scribble/core
         scribble/html-properties)

(define-runtime-path here-path ".")
(define posts-path (build-path here-path "posts"))
(make-directory* posts-path)
(define categories-path (build-path here-path "categories"))
(make-directory* categories-path)
(define titles-path (build-path here-path "titles"))
(make-directory* titles-path)

(define (string-take* s n)
  (list->string
   (for/list ([c (in-string s)]
              [i (in-range n)])
     c)))
(define (filename->tag fname)
  (match fname
    [(regexp #rx"^(....)-(..)-(..)-(.*)\\.(rkt|scrbl)$"
             (list _ year month day code ext))

     (format "~a-~a-~a-~a" year month day (string-take* code 8))]
    [_ #f]))

(define (all-posts)
  (filter
   (λ (ps) (filename->tag ps))
   (sort (map path->string (directory-list posts-path))
         string-ci>?)))

(define (catref c)
  (secref c #:tag-prefixes (list "cat-")))

(define (postref p)
  (secref "post" #:tag-prefixes (list (format "~a-" p))))

(define (postlink p . more)
  (apply seclink "post" #:tag-prefixes (list (format "~a-" p)) more))

(define RECENT-POSTS 6)

(define (google-analytics . more)
  (style #f
    (append more
            (list
             (head-extra
              `(link ([href "atom.xml"]
                      [rel "alternate"]
                      [title ,*BLOG-TITLE*]
                      [type "application/atom+xml"])))
             (head-extra
              `(script ([type "text/javascript"])
                       ,(format "var _gaq = _gaq || [];
    _gaq.push(['_setAccount', '~a']);
    _gaq.push(['_trackPageview']);

    (function() {
      var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
      ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
      var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
    })();"
                                *BLOG-GA-ACCOUNT*)))))))

(define *BLOG-GA-ACCOUNT* "UA-30131476-1")
(define *BLOG-TITLE* "Jay McCarthy")
(define *BLOG-URL* "http://jeapostrophe.github.com")

(provide (all-defined-out))
