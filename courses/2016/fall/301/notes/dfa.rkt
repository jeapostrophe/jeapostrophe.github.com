#lang racket/base
(require racket/list)

(define-syntax-rule
  (dfa start-state (accept-state ...)
       [state (sym -> next-state)
              ...]
       ...)
  (letrec ([accepting?
            (λ (some-state)
              (or (eq? some-state accept-state)
                  ...))]
           [state
            (λ (l)
              (if (empty? l)
                (accepting? state)
                (case (first l)
                  [(sym)
                   (next-state (rest l))]
                  ...)))]
           ...)
    start-state))

(define the-dfa
  (dfa start (odd-and-0 even-and-1)
       [start (0 -> odd-and-0)
              (1 -> odd-and-1)]
       [odd-and-0 (0 -> even-and-0)
                  (1 -> even-and-0)]
       [even-and-0 (0 -> odd-and-0)
                   (1 -> odd-and-0)]
       [odd-and-1 (0 -> even-and-1)
                  (1 -> even-and-1)]
       [even-and-1 (0 -> odd-and-1)
                   (1 -> odd-and-1)]))

(module+ test
  (require chk)
  (chk
   (the-dfa '(1 0 0)) #f
   (the-dfa '(0 0 0 0)) #f
   (the-dfa '(0 0 0)) #t
   (the-dfa '(1 0 0 0)) #t))
