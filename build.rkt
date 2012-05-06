#lang racket/base
(require racket/system
         racket/path
         racket/file
         racket/runtime-path)

(define-runtime-path posts "_posts")
(define-runtime-path posts.rkt "downloads/code")

(define (rkt->markdown rkt-p)
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
      (printf "@(require scribble/lp-include)\n")
      (printf "@lp-include[~v]\n"
              (path->string
               (find-relative-path build-dir
                                   (build-path (current-directory)
                                               rkt-p))))))
  (parameterize ([current-directory build-dir])
    (system* (find-executable-path "raco") "scribble" "--text" scrbl-p))
  (define text-p
    (build-path build-dir
                (path-replace-suffix rkt-p #".txt")))
  (define markdown-p (path-replace-suffix rkt-p #".markdown"))
  (rename-file-or-directory text-p (build-path posts markdown-p) #t)
  (delete-directory/files build-dir))

(parameterize ([current-directory posts.rkt])
  (for-each rkt->markdown
            (filter
             (λ (x)
               (regexp-match? #rx"rkt$" (path->string x)))
             (directory-list))))
