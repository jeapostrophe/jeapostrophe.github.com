#lang racket
(require (for-syntax syntax/parse
                     racket)
         tests/eli-tester)

;; dfa : list(chars) -> boolean

;; c(a|d)*r
(define (cadr-okay?/by-hand l)
  (define (loop state l)
    (case state
      [(waiting-for-a-c)
       (cond
        [(empty? l) #f]
        [else
         (if (eq? 'c (first l))
             (loop 'a-or-d (rest l))
             #f)])]
      [(a-or-d)
       (cond
        [(empty? l) #f]
        [else
         (if (or (eq? 'a (first l))
                 (eq? 'd (first l)))
             (loop 'a-or-d (rest l))
             (if (eq? 'r (first l))
                 (loop 'end (rest l))
                 #f))])]
      [(end)
       (cond
        [(empty? l) #t]
        [else
         #f])]))
  (loop 'waiting-for-a-c l))

(define (cadr-okay?/by-hand2 l)
  (define (a-or-d l)
    (cond
     [(empty? l) #f]
     [else
      (if (or (eq? 'a (first l))
              (eq? 'd (first l)))
          (a-or-d (rest l))
          (if (eq? 'r (first l))
              (end (rest l))
              #f))]))
  (define (waiting-for-a-c l)
    (cond
     [(empty? l) #f]
     [else
      (if (eq? 'c (first l))
          (a-or-d (rest l))
          #f)]))
  (define (end l)
    (cond
     [(empty? l) #t]
     [else
      #f]))
  (waiting-for-a-c l))

(begin-for-syntax
 (define (member? x l)
    (and (member x l) #t)))

(define-syntax (dfa stx)
  (syntax-parse 
   stx
   [(dfa start-state:id
         ([state:id
           (char:expr -> next-state:id)
           ...]
          ...)
         (end-state:id ...))
    (with-syntax
     ([(end-state? ...)
       (for/list ([state-stx (syntax->list #'(state ...))])
                 (member? (syntax->datum state-stx)
                          (syntax->datum #'(end-state ...))))])
     (syntax/loc 
      stx
      (lambda (l)
        (define (state l)
          (cond
           [(empty? l)
            end-state?]
           [(eq? 'char (first l))
            (next-state (rest l))]
           ...
           [else
            #f]))
        ...
        (start-state l))))]))

(define cadr-okay?/macro
  (dfa waiting-for-a-c
       ([waiting-for-a-c
         (c -> a-or-d)]
        [a-or-d
         (a -> a-or-d)
         (d -> a-or-d)
         (r -> end)]
        [end
         ])
       (end)))

(define cadr-okay? cadr-okay?/macro)
(test 
 (cadr-okay? '(c r)) => #t
 (cadr-okay? '(c a d d a d r)) => #t
 (cadr-okay? '(c a d r)) => #t
 (cadr-okay? '(r c a d)) => #f
 (cadr-okay? '(c a d)) => #f
 (cadr-okay? '(c a d r a)) => #f)

;;;; change the d into a n

(define-syntax (nfa stx)
  (syntax-parse 
   stx
   [(nfa (start-state:id ...)
         ([state:id
           (char:expr -> (next-state:id ...))
           ...]
          ...)
         (end-state:id ...))
    (with-syntax
     ([(end-state? ...)
       (for/list ([state-stx (syntax->list #'(state ...))])
                 (member? (syntax->datum state-stx)
                          (syntax->datum #'(end-state ...))))])
     (syntax/loc 
      stx
      (lambda (l)
        (define (state l)
          (cond
           [(empty? l)
            end-state?]
           [(eq? 'char (first l))
            (or (next-state (rest l))
                ...)]
           ...
           [else
            #f]))
        ...
        (or (start-state l)
            ...))))]))

(define nfa:cadr-okay?/macro
  (nfa (waiting-for-a-c)
       ([waiting-for-a-c
         (c -> (a d))]
        [a
         (a -> (a d))
         (d -> (a d))
         (r -> (end))]
        [d
         (a -> (a d))
         (d -> (a d))
         (r -> (end))]
        [end
         ])
       (end)))

(define nfa:cadr-okay? nfa:cadr-okay?/macro)
(test 
 (nfa:cadr-okay? '(c r)) => #t
 (nfa:cadr-okay? '(c a d d a d r)) => #t
 (nfa:cadr-okay? '(c a d r)) => #t
 (nfa:cadr-okay? '(r c a d)) => #f
 (nfa:cadr-okay? '(c a d)) => #f
 (nfa:cadr-okay? '(c a d r a)) => #f)
