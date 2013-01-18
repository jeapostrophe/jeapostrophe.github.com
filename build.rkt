#lang racket/base
(require racket/system
         racket/path
         racket/file
         racket/port
         racket/runtime-path
         (planet ryanc/scriblogify))

(define-runtime-path posts "_posts")
(define-runtime-path posts.rkt "downloads/code")

(define (rkt->markdown rkt-p)
  (printf "~a\n" rkt-p)
  (define build-dir
    (build-path (current-directory) "build"))
  (make-directory* build-dir)
  (define scrbl-p
    (build-path build-dir
                (path-replace-suffix rkt-p #".scrbl")))
  (with-output-to-file scrbl-p
    #:exists 'replace
    (λ ()
      (printf "#lang scribble/base\n")
      (printf "@(require scribble/lp-include) @author{Jay McCarthy}\n")
      (printf "@lp-include[~v]\n"
              (path->string
               (find-relative-path build-dir
                                   (build-path (current-directory)
                                               rkt-p))))))
  (define html-str
    (with-output-to-string
      (λ ()
        (scriblogify (path->string scrbl-p) #:link-to-pre? #t))))
  (define markdown-p 
    (build-path posts (path-replace-suffix rkt-p #".html")))
  (display-to-file
   (regexp-replace #rx"^<p>---(.*?)---</p>" html-str "---\\1---\n\n")
   markdown-p 
    #:exists 'replace)
  (delete-directory/files build-dir))

(parameterize ([current-directory posts.rkt])
  (for-each rkt->markdown
            (filter
             (λ (x)
               (regexp-match? #rx"rkt$" (path->string x)))
             (directory-list))))
