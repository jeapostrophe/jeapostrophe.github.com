#lang scheme

; Let
(define-syntax let
  (syntax-rules ()
    [(let ([v e] ...) b)
     ((lambda (v ...) b) e ...)]))

(let ([x 3]
      [y 2])
  (+ x y))

; Switch
(define-syntax switch
  (syntax-rules ()
    [(switch)
     (lambda (v)
       (error 'switch "Unknown signal: ~e" v))]
    [(switch [sym0 act0]
             [pat-rest act-rest]
             ...)
     (lambda (v)
       (if (symbol=? v 'sym0)
           act0
           ((switch [pat-rest act-rest]
                    ...)
            v)))]))

(define m
  (switch [off 0]
          [on 1]))

(m 'off)
(m 'on)

; Automata
#|
automaton init 
 init : c -> more 
 more : a -> more 
        d -> more 
        r -> end 
 end : 
|#

;; Interpreter
(define machine 
  '([init (c more)]
    [more (a more) 
          (d more) 
          (r end)]
    [end]))

(define (run machine init-state stream)
  (define (walker state stream)
    (or (empty? stream)
        (let* ([transitions (rest (assv state machine))]
               [in (first stream)]
               [new-state (assv in transitions)])
          (if new-state
              (walker (second new-state) (rest stream))
              false))))
  (walker init-state stream))

(run machine 'init '(c a d a d d r))
(run machine 'init '(c a d a d d r r))

;; Better
(define init
  (lambda (stream)
    (or (empty? stream)
        (case (first stream)
          [(c) (more (rest stream))]
          [else false]))))
(define more
  (lambda (stream)
    (or (empty? stream)
        (case (first stream)
          [(a) (more (rest stream))]
          [(d) (more (rest stream))]
          [(r) (end (rest stream))]
          [else false]))))
(define end
  (lambda (stream)
    (or (empty? stream)
        (case (first stream)
          [else false]))))
(define machine-proc init)

(machine-proc '(c a d a d d r))
(machine-proc '(c a d a d d r r))

;; Compiler
(define-syntax automaton
  (syntax-rules (: ->)
    [(automaton start
                [state : (label -> target) ...]
                ...)
     (lambda (init-stream)
       (letrec ([state
                 (lambda (stream)
                   (or (empty? stream)
                       (case (first stream)
                         [(label) (target (rest stream))]
                         ...
                         [else false])))]
                ...)
         (start init-stream)))]))

(define machine-comp
  (automaton 
   init
   [init : (c -> more)]
   [more : (a -> more)
           (d -> more)
           (r -> end)]
   [end  :]))

(machine-comp '(c a d a d d r))
(machine-comp '(c a d a d d r r))
