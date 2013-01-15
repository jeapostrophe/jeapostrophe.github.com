#lang at-exp racket/base
(require racket/runtime-path
         racket/file
         racket/match
         scribble/base)

(define-runtime-path here-path ".")
(define posts-path (build-path here-path "posts"))
(make-directory* posts-path)
(define categories-path (build-path here-path "categories"))
(make-directory* categories-path)

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

(provide (all-defined-out))
