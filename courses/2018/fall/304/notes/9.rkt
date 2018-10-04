#lang racket/base
(require racket/set
         racket/list
         racket/match)
(module+ test
  (require chk))

(struct dfa (start accept trans)
  #:transparent)

;; interp : DFA x String -> Bool
(define (interp some-dfa some-str)
  (match-define (dfa start accept trans) some-dfa)
  (define current-state start)
  (for ([some-char (in-list some-str)])
    (set! current-state
          (hash-ref trans (cons current-state some-char))))
  (set-member? accept current-state))

(module+ test
  ;; Start state: Even
  ;; Accept state: Even
  ;; Transitions:
  ;;  Even, 0 -> Odd
  ;;  Even, 1 -> Odd
  ;;  Odd,  0 -> Even
  ;;  Odd,  1 -> Even
  (define evens
    (dfa 'even (set 'even)
         (hash (cons 'even 0) 'odd
               (cons 'even 1) 'odd
               (cons 'odd 0) 'even
               (cons 'odd 1) 'even)))
  
  (chk (interp evens empty) #t)
  (chk (interp evens (list 0)) #f)
  (chk (interp evens '()) #t)
  (chk (interp evens '(0)) #f)
  (chk (interp evens '(0 1)) #t)
  (chk (interp evens '(0 1 1 0)) #t)
  (chk (interp evens '(0 1 1 0 1)) #f))
