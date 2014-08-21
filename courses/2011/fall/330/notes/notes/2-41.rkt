#lang racket
(require tests/eli-tester)

;; c(a|d)*r
(define (cadr-okay?/by-hand l)
  ;; check-for-a-c
  (define (check-for-a-c l)
    (cond
     [(empty? l)
      #f]
     [(eq? 'c (first l)) 
      (check-for-some-a-or-ds (rest l))]
     [else
      #f]))  
  ;; check-for-some-a-or-ds
  (define (check-for-some-a-or-ds l)
    (cond
     [(empty? l)
      #f]
     [(eq? 'a (first l)) 
      (check-for-some-a-or-ds (rest l))]
     [(eq? 'd (first l)) 
      (check-for-some-a-or-ds (rest l))]
     [(eq? 'r (first l)) 
      (check-for-a-nothing (rest l))]
     [else
      #f]))
  ;; check-for-a-nothing
  (define (check-for-a-nothing l)
    (cond
     [(empty? l)
      #t]
     [else
      #f]))

  (check-for-a-c l))

(require (for-syntax syntax/parse
                     racket))

(begin-for-syntax
  (define (member? x l)
    (printf "~v\n" `(member? ,x ,l))
    (and (member x l) #t)))

(define-syntax (at-compile-time-member? stx)
  (syntax-parse 
   stx
   [(at-compile-time-member? x l)
    (datum->syntax
     stx
     (member? (syntax->datum #'x)
              (syntax->datum #'l)))]))

(define-syntax (dfa stx)
  (syntax-parse
   stx
   [(dfa start-state
         ([state
           (char -> next-state)
           ...]
          ...)
         (end-state ...))
    (printf "Compiling dfa\n")
    (begin0
     (syntax/loc 
     stx
     (lambda (l)
       ;; state
       (define (state l)
         (cond
          [(empty? l)
           (at-compile-time-member? state (end-state ...))]
          [(eq? 'char (first l)) 
           (next-state (rest l))]
          ...
          [else
           #f]))
       ...

       (start-state l)))
     (printf "Done compiling dfa\n"))]))

(define cadr-okay?/awesome
  (dfa check-for-a-c
       ([check-for-a-c
         (c -> check-for-some-a-or-ds)]
        [check-for-some-a-or-ds
         (a -> check-for-some-a-or-ds)
         (d -> check-for-some-a-or-ds)
         (r -> check-for-a-nothing)]
        [check-for-a-nothing])
       (check-for-a-nothing)))

(printf "Running\n")

;; cadr-okay : list(char) -> bool
(define cadr-okay? cadr-okay?/awesome)
(test
 (cadr-okay? '(c r)) => #t
 (cadr-okay? '(c a r)) => #t
 (cadr-okay? '(c d r)) => #t
 (cadr-okay? '(c a d r)) => #t
 (cadr-okay? '(a d r)) => #f
 (cadr-okay? '(r c a d r)) => #f
 (cadr-okay? '(c r a d r)) => #f)

(define divisible-by-five?
  (dfa Q0
       ([Q0
         (0 -> Q0)
         (1 -> Q1)]
        [Q1
         (0 -> Q2)
         (1 -> Q3)]
        [Q2
         (0 -> Q4)
         (1 -> Q0)]
        [Q3
         (0 -> Q1)
         (1 -> Q2)]
        [Q4
         (0 -> Q3)
         (1 -> Q4)])
       (Q0)))

(test
 (divisible-by-five? '(1 0 1 0 1 1 1 1)) => #t)

;;;;;;

(define-syntax (nfa stx)
  (syntax-parse
   stx
   [(nfa (start-state ...)
         ([state
           (char -> (next-state ...))
           ...]
          ...)
         (end-state ...))
    (printf "Compiling nfa\n")
    (begin0
     (syntax/loc 
     stx
     (lambda (l)
       ;; state
       (define (state l)
         (cond
          [(empty? l)
           (at-compile-time-member? state (end-state ...))]
          [(eq? 'char (first l)) 
           (or (next-state (rest l))
               ...)]
          ...
          [else
           #f]))
       ...

       (or (start-state l)
           ...)))
     (printf "Done compiling nfa\n"))]))

(define nfa:cadr-okay?/awesome
  (nfa (check-for-a-c)
       ([check-for-a-c
         (c -> (a d))]
        [a
         (a -> (a d))
         (r -> (check-for-a-nothing))]
        [d
         (d -> (a d))
         (r -> (check-for-a-nothing))]
        [check-for-a-nothing])
       (check-for-a-nothing)))

(define nfa:cadr-okay? nfa:cadr-okay?/awesome)
(test
 (nfa:cadr-okay? '(c r)) => #t
 (nfa:cadr-okay? '(c a r)) => #t
 (nfa:cadr-okay? '(c d r)) => #t
 (nfa:cadr-okay? '(c a d r)) => #t
 (nfa:cadr-okay? '(a d r)) => #f
 (nfa:cadr-okay? '(r c a d r)) => #f
 (nfa:cadr-okay? '(c r a d r)) => #f)
