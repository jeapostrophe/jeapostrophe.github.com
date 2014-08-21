#lang plai
(print-only-errors #t)

;; Automata
;; -- interpreter
;; -- hand-compilation
;; -- compiler

;; State machine:
;;; Sigma = {c, a, d, r}
;;; Start = start
;;; States = start, middle, end
;;; Transitions =
;;;;   start  : c -> middle
;;;;   middle : a -> middle
;;;;            d -> middle
;;;;            r -> end
;;;;   end    :
;;; Accept = { end }

;; Simple
;;; Transitions =
;;;;   start  : c -> middle
;;;;   middle : a -> middle
;;;;            d -> middle
;;;;            r -> end
;;;;   end    :

;; Recognizes... car cdr cadadadr caddddr

(define cadr-machine
  (list (list 'start
              (list 'c 'middle))
        (list 'middle
              (list 'a 'middle)
              (list 'd 'middle)
              (list 'r 'end))
        (list 'end)))

;; interp : machine-specification string -> boolean?
(define (interp spec string)
  (define start-state
    (first (first spec)))
  (interp/loop spec start-state string))

(define (lookup-state spec state)
  (cond
    [(empty? spec)
     #f]
    [(eq? state (first (first spec)))
     (rest (first spec))]
    [else
     (lookup-state (rest spec) state)]))

(define lookup-transitions
  lookup-state)

(define (interp/loop spec state string)
  (cond
    [(empty? string)
     (eq? state (first (last spec)))]
    [else
     (define state-transitions
       (lookup-state spec state))
     (unless state-transitions
       (error 'interp "Bad state: ~e" state))
     (define next-state
       (lookup-transitions state-transitions (first string)))
     (if next-state
       (interp/loop spec (first next-state) (rest string))
       #f)]))

(test (interp cadr-machine (list 'c 'a 'r))
      #t)
(test (interp cadr-machine (list 'c 'r))
      #t)
(test (interp cadr-machine (list 'c 'd 'r))
      #t)
(test (interp cadr-machine (list 'c 'a 'd 'r))
      #t)
(test (interp cadr-machine (list 'c 'a 'd 'r 'a))
      #f)
(test (interp cadr-machine (list 'c 'a 'd 'r))
      #t)

(define big-list
  (cons 'c
        (append
         (for/list ([i (in-range 80000)])
           (if (zero? (random 2))
             'a
             'd))
         (list 'r))))

(time (interp cadr-machine
              big-list))

;; hand-compiled

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

(define cadr-machine2
  start)

(define (interp2 machine string)
  (machine string))

(test (interp2 cadr-machine2 (list 'c 'a 'r))
      #t)
(test (interp2 cadr-machine2 (list 'c 'r))
      #t)
(test (interp2 cadr-machine2 (list 'c 'd 'r))
      #t)
(test (interp2 cadr-machine2 (list 'c 'a 'd 'r))
      #t)
(test (interp2 cadr-machine2 (list 'c 'a 'd 'r 'a))
      #f)
(test (interp2 cadr-machine2 (list 'c 'a 'd 'r))
      #t)
(time (interp2 cadr-machine2
               big-list))

;;; compiled

(define cadr-machine3
  (fsm
   [start
    (c middle)]
   [middle
    (a middle)
    (d middle)
    (r end)]
   [end]))

(define-syntax-rule
  (fsm [start-state (start-char start-next-state) ...]
       [state (char next-state) ...]
       ...
       [end-state])
  (fsm* start-state end-state
        [start-state (start-char start-next-state) ...]
        [state (char next-state) ...]
        ...))

(define-syntax-rule
  (fsm* start-state end-state
        [state (char next-state) ...]
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

(define (interp3 machine string)
  (machine string))

(test (interp3 cadr-machine3 (list 'c 'a 'r))
      #t)
(test (interp3 cadr-machine3 (list 'c 'r))
      #t)
(test (interp3 cadr-machine3 (list 'c 'd 'r))
      #t)
(test (interp3 cadr-machine3 (list 'c 'a 'd 'r))
      #t)
(test (interp3 cadr-machine3 (list 'c 'a 'd 'r 'a))
      #f)
(test (interp3 cadr-machine3 (list 'c 'a 'd 'r))
      #t)
(time (interp3 cadr-machine3
               big-list))

((fsm [start (good-boy presents)
            (bad-boy coal)]
     [presents (new-year start)]
     [coal (christmas-day burn-the-house-down)]
     [burn-the-house-down (cops jail)]
     [jail])
 (list 'good-boy 'new-year
       'good-boy 'new-year
       'good-boy 'new-year
       'good-boy 'new-year
       'good-boy 'new-year
       'good-boy 'new-year
       'bad-boy 'christmas-day 'cops))




