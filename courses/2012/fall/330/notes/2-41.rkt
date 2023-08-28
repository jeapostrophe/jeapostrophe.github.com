#lang plai
(print-only-errors #t)

;; Euler's Equation: e^{i * \pi} = -1

;; Automata
;; -- interpreter
;; -- hand-compilation
;; -- compiler

;; FSM...
;;; States = { start, middle, end }
;;; Sigma = { c, a, d, r }
;;; Start = start
;;; End = { end }
;;; Transitions:
;;;;  start : c -> middle
;;;;  middle: a -> middle
;;;;          d -> middle
;;;;          r -> end
;;;;  end   :

;; Simplified FSM...
;;; Transitions:
;;;;  start : c -> middle
;;;;  middle: a -> middle
;;;;          d -> middle
;;;;          r -> end
;;;;  end   :

(define cadr-machine
  (list (list 'start
              (list 'c 'middle))
        (list 'middle
              (list 'a 'middle)
              (list 'd 'middle)
              (list 'r 'end))
        (list 'end)))

;; lookup : (list (list E D ...) ...) E -> (U #f (list D ...))
(define (lookup l e)
  (cond
    [(empty? l)
     #f]
    [(equal? e (first (first l)))
     (rest (first l))]
    [else
     (lookup (rest l) e)]))

;; interp : machine-spec string -> boolean?
(define (interp spec string)
  (define start-state
    (first (first spec)))
  (interp/loop spec start-state string))

(define (interp/loop spec state string)
  (cond
    [(empty? string)
     (eq? state (first (last spec)))]
    [else
     (define current-char (first string))
     (define next-string (rest string))
     (define trans
       (lookup spec state))
     (unless trans
       (error 'interp/loop "Unknown state: ~e" state))
     (define next-state
       (lookup trans current-char))
     (if next-state
       (interp/loop spec (first next-state) next-string)
       #f)]))

(define big-string
  (cons 'c
        (append
         (for/list ([n (in-range 80000)])
           (if (zero? (random 2))
             'a
             'd))
         (list 'r))))

(test (interp cadr-machine '(c a r))
      #t)
(test (interp cadr-machine '(c a r d))
      #f)
(test (interp cadr-machine '(c a d a d a r))
      #t)
(time (interp cadr-machine big-string))

;;;;

(define cadr-machine2
  (let ()
    (define (start string)
      (if (empty? string)
        #f
        (match (first string)
          ['c (middle (rest string))]
          [else #f])))

    (define (middle string)
      (if (empty? string)
        #f
        (match (first string)
          ['a (middle (rest string))]
          ['d (middle (rest string))]
          ['r (end (rest string))]
          [else #f])))

    (define (end string)
      (empty? string))

    start))

(define (interp2 spec string)
  (spec string))

(test (interp2 cadr-machine2 '(c a r))
      #t)
(test (interp2 cadr-machine2 '(c a r d))
      #f)
(test (interp2 cadr-machine2 '(c a d a d a r))
      #t)
(time (interp2 cadr-machine2 big-string))

;;;;;;;

(define-syntax-rule
  (fsm* start-state end-state
        [state ([char next-state] ...)]
        ...)
  (let ()
    (define (state string)
      (if (empty? string)
        #f
        (match (first string)
          ['char (next-state (rest string))]
          ...
          [else #f])))
    ...
    (define (end-state string)
      (empty? string))
    start-state))

(define-syntax-rule
  (fsm [start-state ([start-char start-next-state] ...)]
       [state ([char next-state] ...)]
       ...
       [end-state ()])
  (fsm* start-state end-state
        [start-state ([start-char start-next-state] ...)]
        [state ([char next-state] ...)]
        ...))

(define cadr-machine3
  (fsm
   [start ([c middle])]
   [middle ([a middle]
            [d middle]
            [r end])]
   [end ()]))

(test (interp2 cadr-machine3 '(c a r))
      #t)
(test (interp2 cadr-machine3 '(c a r d))
      #f)
(test (interp2 cadr-machine3 '(c a d a d a r))
      #t)
(time (interp2 cadr-machine3 big-string))

((fsm [start ([a need-b]
             [b need-a]
             [z end])]
     [need-b ([b start])]
     [need-a ([a start])]
     [end ()])
 '(a b a b b a b a a b z))

