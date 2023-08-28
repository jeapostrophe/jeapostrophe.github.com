#lang racket/base
(require racket/list
         racket/match
         racket/pretty)

(define (random-list-ref l)
  (list-ref l (random (length l))))

(define (generate1 grammar start-sym)
  (define (loop current-string)
    (match current-string
      [(list) empty]
      [(cons a more)
       (cond
         [(hash-ref grammar a #f)
          => (λ (options)
               (loop (append (random-list-ref options)
                             more)))]
         [else
          (cons a (loop more))])]))
  (loop (list start-sym)))

(define (generate grammar start-sym how-many)
  (for ([i (in-range how-many)])
    (pretty-print
     (generate1 grammar start-sym))))

(module+ test
  (define 0n1n
    (hasheq 'S '[() (0 S 1)]))
  (generate 0n1n 'S 25)

  (define AE
    (hasheq 'AE '[(0) (1) (AE + AE) (AE * AE)]))
  #;(generate AE 'AE 25))

(require redex/reduction-semantics
         racket/list)
(module+ test
  (define-language 0n1n-lang
    [S () (0 S 1)]
    [AE (0) (1) (AE + AE) (AE * AE)])
  (for ([i (in-range 25)])
    (pretty-print
     (flatten (generate-term 0n1n-lang AE #:i-th i)))))
