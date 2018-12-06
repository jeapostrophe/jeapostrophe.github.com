#lang racket/base
(require racket/set
         racket/list
         racket/match)
(module+ test
  (require chk
           racket/pretty))

(struct dfa (q start accept trans)
  #:transparent)

;; interp : DFA x String -> Bool
(define (interp some-dfa some-str)
  (match-define (dfa q start accept trans) some-dfa)
  (unless (set-member? q start)
    (error 'interp "Invalid start state: ~e" start))
  (define current-state start)
  (for ([some-char (in-list some-str)])
    (define next-state (trans current-state some-char))
    (unless (set-member? q next-state)
      (error 'interp "Invalid state returned from trans: ~e" next-state))
    (set! current-state next-state))
  (set-member? accept current-state))

(define (dfa-complement d)
  (match-define (dfa q start accept trans) d)
  (dfa q start (set-subtract q accept) trans))

(define (dfa-combine accept-combiner x y)
  (match-define (dfa x-q x-start x-accept x-trans) x)
  (match-define (dfa y-q y-start y-accept y-trans) y)
  (define q (for*/set ([xi (in-set x-q)] [yi (in-set y-q)])
              (cons xi yi)))
  (define start (cons x-start y-start))
  (define accept
    (accept-combiner x-q y-q x-accept y-accept))
  (define (trans p c)
    (match-define (cons xi yi) p)
    (cons (x-trans xi c) (y-trans yi c)))
  (dfa q start accept trans))

(define (dfa-union x y)
  (define (accept-combiner x-q y-q x-accept y-accept)
    (set-union (for*/set ([xF (in-set x-accept)] [yi (in-set y-q)])
                 (cons xF yi))
               (for*/set ([xi (in-set x-q)] [yF (in-set y-accept)])
                 (cons xi yF))))
  (dfa-combine accept-combiner x y))

(define (dfa-intersect x y)
  (define (accept-combiner x-q y-q x-accept y-accept)
    (for*/set ([xF (in-set x-accept)] [yF (in-set y-accept)])
      (cons xF yF)))
  (dfa-combine accept-combiner x y))

(define dfa-emptyset
  (dfa (set 'nothin) 'nothin (set)
       (λ (q c) 'nothin)))

(define dfa-epsilon
  (dfa (set 'yes 'no) 'yes (set 'yes)
       (λ (q c) 'no)))

(define (dfa-char c)
  (dfa (set 'not-seen 'seen 'too-much/wrong) 'not-seen (set 'seen)
       (match-lambda*
         [(list 'not-seen (== c)) 'seen]
         [(list 'not-seen _) 'too-much/wrong]
         [(list 'seen _) 'too-much/wrong]
         [(list 'too-much/wrong _) 'too-much/wrong])))

(struct nfa (q start accept trans)
  #:transparent)
(define (dfa->nfa d)
  (match-define (dfa q start accept trans) d)
  (nfa q start accept
       (λ (qi c) (set (trans qi c)))))

(define (set-tag t s)
  (for/set ([e (in-set s)])
    (cons t e)))

(define (nfa-concat x y)
  (match-define (nfa x-q x-start x-accept x-trans) x)
  (match-define (nfa y-q y-start y-accept y-trans) y)
  (define x-q/t (set-tag 'x x-q))
  (define y-q/t (set-tag 'y y-q))
  (define q (set-union x-q/t y-q/t))
  (define start (cons 'x x-start))
  (define accept (set-tag 'y y-accept))
  (define (trans qi c)
    (match qi
      [(cons 'x xi)
       (set-union
        (if (set-member? x-accept xi)
          (set (cons 'y y-start))
          (set))
        (set-tag 'x (x-trans xi c)))]
      [(cons 'y yi)
       (set-tag 'y (y-trans yi c))]))
  (nfa q start accept trans))

(module+ test
  (pretty-print (nfa-concat (dfa->nfa (dfa-char 0)) (dfa->nfa (dfa-char 1)))))

;; XXX Implement these to make it work!
(define nfa-star #f)
(define nfa->dfa #f)

(define (dfa-concat x y)
  (nfa->dfa (nfa-concat (dfa->nfa x) (dfa->nfa y))))

(define (dfa-star x)
  (nfa->dfa (nfa-star (dfa->nfa x))))

(module+ test
  ;; Start state: Even
  ;; Accept state: Even
  ;; Transitions:
  ;;  Even, 0 -> Odd
  ;;  Even, 1 -> Odd
  ;;  Odd,  0 -> Even
  ;;  Odd,  1 -> Even
  (define evens
    (dfa (set 'even 'odd) 'even (set 'even)
         (match-lambda*
           [(list 'even 0) 'odd]
           [(list 'even 1) 'odd]
           [(list 'odd 0) 'even]
           [(list 'odd 1) 'even])))

  (define (chk-dfa d s r)
    (define nd (dfa-complement d))
    (chk (interp d s) r
         (interp nd s) (not r)
         (interp (dfa-union d nd) s) #t
         (interp (dfa-intersect d nd) s) #f))

  (chk-dfa evens empty #t)
  (chk-dfa evens (list 0) #f)
  (chk-dfa evens '() #t)
  (chk-dfa evens '(0) #f)
  (chk-dfa evens '(0 1) #t)
  (chk-dfa evens '(0 1 1 0) #t)
  (chk-dfa evens '(0 1 1 0 1) #f)

  (define sigma (dfa-union (dfa-char 0) (dfa-char 1)))
  (define three-from-end-is-one
    ;; Σ* ⋅ (1 ⋅ (Σ ⋅ Σ))
    (dfa-concat (dfa-star sigma)
                (dfa-concat (dfa-char 1)
                            (dfa-concat sigma
                                        sigma))))
  (pretty-print three-from-end-is-one)

  (chk-dfa three-from-end-is-one '() #f)
  (chk-dfa three-from-end-is-one '(1) #f)
  (chk-dfa three-from-end-is-one '(0) #f)
  (chk-dfa three-from-end-is-one '(0 0 0) #f)
  (chk-dfa three-from-end-is-one '(1 0 0) #t)
  (chk-dfa three-from-end-is-one '(0 1 0 1 0 0) #t)
  (chk-dfa three-from-end-is-one '(0 1 0 0 0 0) #f))
