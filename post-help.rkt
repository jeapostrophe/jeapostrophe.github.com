#lang racket/base
(require racket/runtime-path
         racket/file
         racket/match)

(define-runtime-path here-path ".")
(define posts-path (build-path here-path "posts"))
(make-directory* posts-path)
(define lp-posts-path (build-path here-path "lp-posts"))
(make-directory* lp-posts-path)
(for* ([d (in-list (list posts-path lp-posts-path))]
       [p (in-list '("post.rkt" "post-help.rkt"))])
  (unless (link-exists? (build-path d p))
    (make-file-or-directory-link
     (build-path here-path p)
     (build-path d p))))

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

(provide (all-defined-out))
