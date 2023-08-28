#lang racket/base
(require racket/set
         racket/list
         racket/match)
(module+ test
  (require chk
           racket/pretty))

(struct dfa (q start accept? trans)
  #:transparent)

;; interp : DFA x String -> Bool
(define (interp some-dfa some-str)
  (match-define (dfa q start accept? trans) some-dfa)
  #;(unless (set-member? q start)
      (error 'interp "Invalid start state: ~e" start))
  (define current-state start)
  (for ([some-char (in-list some-str)])
    (define next-state (trans current-state some-char))
    #;(unless (set-member? q next-state)
        (error 'interp "Invalid state returned from trans: ~e" next-state))
    (set! current-state next-state))
  (accept? current-state))

(define (dfa-complement d)
  (match-define (dfa q start accept? trans) d)
  (dfa q start (λ (q) (not (accept? q))) trans))

(define (dfa-combine accept-combiner x y)
  (match-define (dfa x-q x-start x-accept x-trans) x)
  (match-define (dfa y-q y-start y-accept y-trans) y)
  (define q #f #;(for*/set ([xi (in-set x-q)] [yi (in-set y-q)])
                   (cons xi yi)))
  (define start (cons x-start y-start))
  (define accept
    (accept-combiner x-q y-q x-accept y-accept))
  (define (trans p c)
    (match-define (cons xi yi) p)
    (cons (x-trans xi c) (y-trans yi c)))
  (dfa q start accept trans))

(define (dfa-union x y)
  (define (accept-combiner x-q y-q x-accept? y-accept?)
    (λ (qi)
      (or (x-accept? (car qi))
          (y-accept? (cdr qi)))))
  (dfa-combine accept-combiner x y))

(define (dfa-intersect x y)
  (define (accept-combiner x-q y-q x-accept? y-accept?)
    (λ (qi)
      (and (x-accept? (car qi))
           (y-accept? (cdr qi)))))
  (dfa-combine accept-combiner x y))

(define dfa-emptyset
  (dfa (set 'nothin) 'nothin (λ (q) #f)
       (λ (q c) 'nothin)))

(define dfa-epsilon
  (dfa (set 'yes 'no) 'yes (λ (q) (eq? q 'yes))
       (λ (q c) 'no)))

(define (dfa-char c)
  (dfa (set 'not-seen 'seen 'too-much/wrong) 'not-seen
       (λ (q) (eq? q 'seen))
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

(define nfa-emptyset (dfa->nfa dfa-emptyset))
(define nfa-epsilon (dfa->nfa dfa-epsilon))
(define (nfa-char c) (dfa->nfa (dfa-char c)))

(define (set-tag t s)
  (for/set ([e (in-set s)])
    (cons t e)))

(define (nfa-union x y)
  (match-define (nfa x-q x-start x-accept? x-trans) x)
  (match-define (nfa y-q y-start y-accept? y-trans) y)
  (define x-q/t (set-tag 'x x-q))
  (define y-q/t (set-tag 'y y-q))
  (define q (set-add (set-union x-q/t y-q/t) 'union-start))
  (define start 'union-start)
  (define (accept? q)
    (match q
      [(cons 'x qx) (x-accept? qx)]
      [(cons 'y qy) (y-accept? qy)]
      [_ #f]))
  (define (trans qi c)
    (match qi
      [(== start) (if (eq? c epsilon)
                    (set (cons 'x x-start)
                         (cons 'y y-start))
                    (set))]
      [(cons 'x xi) (set-tag 'x (x-trans xi c))]
      [(cons 'y yi) (set-tag 'y (y-trans yi c))]))
  (nfa q start accept? trans))

(define (nfa-concat x y)
  (match-define (nfa x-q x-start x-accept? x-trans) x)
  (match-define (nfa y-q y-start y-accept? y-trans) y)
  (define x-q/t (set-tag 'x x-q))
  (define y-q/t (set-tag 'y y-q))
  (define q (set-union x-q/t y-q/t))
  (define start (cons 'x x-start))
  (define (accept? q)
    (match q
      [(cons 'y qy) (y-accept? qy)]
      [_ #f]))
  (define (trans qi c)
    (match qi
      [(cons 'x xi)
       (set-union
        (if (and (x-accept? xi)
                 (eq? c epsilon))
          (set (cons 'y y-start))
          (set))
        (set-tag 'x (x-trans xi c)))]
      [(cons 'y yi)
       (set-tag 'y (y-trans yi c))]))
  (nfa q start accept? trans))

(define epsilon (gensym 'epsilon))

(define (nfa-star x)
  (match-define (nfa x-q x-start x-accept? x-trans) x)
  (define q (set-add (set-tag 'x x-q) 'star-start))
  (define start 'star-start)
  (define (accept? q)
    (eq? q 'star-start))
  (define (trans qi c)
    (match qi
      [(cons 'x qx)
       (set-union
        (if (and (x-accept? qx)
                 (eq? c epsilon))
          (set start)
          (set))
        (set-tag 'x (x-trans qx c)))]
      [(== start)
       (if (eq? c epsilon)
         (set (cons 'x x-start))
         (set))]))
  (nfa q start accept? trans))

(define (powerset s)
  (list->set (map list->set (combinations (set->list s)))))

(define (nfa->dfa x)
  (match-define (nfa x-q x-start x-accept? x-trans) x)
  (define q #f #;(powerset x-q))
  (define (E S)
    (define S+epsilons
      (for/fold ([s S]) ([qi (in-set S)])
        (set-union s (x-trans qi epsilon))))
    (if (set=? S S+epsilons)
      S
      (E S+epsilons)))
  (define start (E (set x-start)))
  (define (accept? set-of-qx)
    (for/or ([qx (in-set set-of-qx)])
      (x-accept? qx)))
  (define (trans set-of-qi c)
    (E (for/fold ([s (set)]) ([qi (in-set set-of-qi)])
         (set-union s (x-trans qi c)))))
  (dfa q start accept? trans))

(define (dfa-concat x y)
  (nfa->dfa (nfa-concat (dfa->nfa x) (dfa->nfa y))))

(define (dfa-star x)
  (nfa->dfa (nfa-star (dfa->nfa x))))

(define (dfa-not-empty? d)
  (define sigma (set 0 1))
  (match-define (dfa _ start accept? trans) d)
  (define seen? (make-hash))
  (let/ec return
    (define (explore! q)
      (when (accept? q)
        (return #t))
      (unless (hash-has-key? seen? q)
        (hash-set! seen? q #t)
        (for ([c (in-set sigma)])
          (explore! (trans q c)))))
    (explore! start)
    #f))

(define (dfa-empty? d)
  (not (dfa-not-empty? d)))

;; Everything in X is also in Y
(define (dfa-subset? x y)
  (dfa-empty? (dfa-intersect x (dfa-complement y))))

(define (dfa-equal? x y)
  (and (dfa-subset? x y)
       (dfa-subset? y x)))

(module+ test
  ;; Start state: Even
  ;; Accept state: Even
  ;; Transitions:
  ;;  Even, 0 -> Odd
  ;;  Even, 1 -> Odd
  ;;  Odd,  0 -> Even
  ;;  Odd,  1 -> Even
  (define evens
    (dfa (set 'even 'odd) 'even (λ (q) (eq? q 'even))
         (match-lambda*
           [(list 'even 0) 'odd]
           [(list 'even 1) 'odd]
           [(list 'odd 0) 'even]
           [(list 'odd 1) 'even])))

  (define (chk-dfa d s r)
    (define nd (dfa-complement d))
    (with-chk (['d d] ['s s] ['r r])
      (with-chk (['mode 'normal])
        (chk (interp d s) r))
      (with-chk (['mode 'complement])
        (chk (interp nd s) (not r)))
      (with-chk (['mode 'intersect])
        (chk (interp (dfa-intersect d nd) s) #f))
      (with-chk (['mode 'union])
        (chk (interp (dfa-union d nd) s) #t))))

  (chk-dfa evens empty #t)
  (chk-dfa evens (list 0) #f)
  (chk-dfa evens '() #t)
  (chk-dfa evens '(0) #f)
  (chk-dfa evens '(0 1) #t)
  (chk-dfa evens '(0 1 1 0) #t)
  (chk-dfa evens '(0 1 1 0 1) #f)

  (define sigma (nfa-union (nfa-char 0)
                           (nfa-char 1)))
  (define three-from-end-is-one/nfa
    (nfa-concat (nfa-star sigma)
                 (nfa-concat (nfa-char 1)
                             (nfa-concat sigma
                                         sigma))))
  (define three-from-end-is-one
    ;; Σ* ⋅ (1 ⋅ (Σ ⋅ Σ))
    (nfa->dfa
     three-from-end-is-one/nfa))
  (printf "constructed!\n")
  (pretty-print three-from-end-is-one)

  (chk-dfa three-from-end-is-one '() #f)
  (chk-dfa three-from-end-is-one '(1) #f)
  (chk-dfa three-from-end-is-one '(0) #f)
  (chk-dfa three-from-end-is-one '(0 0 0) #f)
  (chk-dfa three-from-end-is-one '(1 0 0) #t)
  (chk-dfa three-from-end-is-one '(0 0 0 0) #f)
  (chk-dfa three-from-end-is-one '(0 1 0 0) #t)
  (chk-dfa three-from-end-is-one '(1 0 0 0) #f)
  (chk-dfa three-from-end-is-one '(1 1 0 0) #t)
  (chk-dfa three-from-end-is-one '(0 1 0 1 0 0) #t)
  (chk-dfa three-from-end-is-one '(0 1 0 0 0 0) #f)

  (chk (dfa-not-empty? evens) #t)
  (chk (dfa-not-empty? (dfa-intersect evens (dfa-complement evens))) #f)
  (chk (dfa-not-empty? three-from-end-is-one) #t)
  (chk (dfa-not-empty? dfa-emptyset) #f)

  (chk (dfa-subset? dfa-emptyset evens) #t)
  (chk (dfa-subset? dfa-epsilon evens) #t)
  (chk (dfa-subset? evens dfa-epsilon) #f)
  (chk (dfa-subset? evens three-from-end-is-one) #f)
  (chk (dfa-subset? three-from-end-is-one evens) #f)
  (chk (dfa-subset? evens (dfa-union evens three-from-end-is-one)) #t)

  (define three-from-end-is-one-manual/nfa
    (nfa #f 'start (λ (q) (eq? q 'end))
         (λ (q c)
           ;;     0,1
           ;;    ----
           ;;   |    |   1               0,1                0,1
           ;; start -|-----  saw-a-one ------ almost-done ------ end
           (match* (q c)
             [('start 0) (set 'start)]
             [('start 1) (set 'start 'saw-a-one)]
             [('saw-a-one (or 0 1)) (set 'almost-done)]
             [('almost-done (or 0 1)) (set 'end)]
             [(_ _) (set)]))))
  (define three-from-end-is-one-manual
    (nfa->dfa
     three-from-end-is-one-manual/nfa))

  (chk (dfa-equal? three-from-end-is-one
                   three-from-end-is-one-manual) #t))

(define (nfa-interp some-nfa some-str)
  (match-define (nfa _ start accept? trans) some-nfa)
  (let/ec return
    (define queue (list (λ () (try start some-str))))
    (define (trys qs rest-of-str)
      (for/or ([q (in-set qs)])
        (set! queue
              (append queue
                      (list (λ () (try q rest-of-str)))))))
    (define (try q rest-of-str)
      (when (and (empty? rest-of-str) (accept? q))
        (return #t))
      (unless (empty? rest-of-str)
        (trys (trans q (first rest-of-str)) (rest rest-of-str)))
      (trys (trans q epsilon) rest-of-str))
    (let loop ()
      (match queue
        ['() #f]
        [(cons next-try more-tries)
         (set! queue more-tries)
         (next-try)
         (loop)]))))

(module+ test
  
  (chk (nfa-interp three-from-end-is-one-manual/nfa '()) #f)
  (chk (nfa-interp three-from-end-is-one-manual/nfa '(0)) #f)
  (chk (nfa-interp three-from-end-is-one-manual/nfa '(1)) #f)
  (chk (nfa-interp three-from-end-is-one-manual/nfa '(0 0)) #f)
  (chk (nfa-interp three-from-end-is-one-manual/nfa '(1 0)) #f)
  (chk (nfa-interp three-from-end-is-one-manual/nfa '(1 0 0)) #t)
  (chk (nfa-interp three-from-end-is-one-manual/nfa '(0 0 0)) #f)
  (chk (nfa-interp three-from-end-is-one-manual/nfa '(0 0 0 1 0 0)) #t)
  #;(chk (nfa-interp three-from-end-is-one/nfa '(0 0 0 1 0 0)) #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turing Machines

(struct tape (behind ahead) #:transparent)
(struct tm (start accept? trans) #:transparent)

(define (tape-head tp)
  (match-define (tape _ ahead) tp)
  (match ahead
    ['() '_]
    [(cons a-char _) a-char]))

(define (tape-write c tp)
  (match-define (tape behind ahead) tp)
  (define new-ahead
    (match ahead
      ['() (list c)]
      [(cons _ more-ahead)
       (cons c more-ahead)]))
  (tape behind new-ahead))

(define (tape-move dir tp)
  (match-define (tape behind ahead) tp)
  (match dir
    ['L
     (match behind
       ['() (tape '() (cons '_ ahead))]
       [(cons a-char more-behind)
        (tape more-behind (cons a-char ahead))])]
    ['R
     (match ahead
       ['() (tape (cons '_ behind) '())]
       [(cons a-char more-ahead)
        (tape (cons a-char behind) more-ahead)])]))

(define (tm-interp a-tm a-str)
  (match-define (tm start accept? trans) a-tm)
  (let loop ([qi start] [tp (tape empty a-str)])
    (cond
      [(accept? qi)
       #t]
      [else
       (match-define (list qj b dir) (trans qi (tape-head tp)))
       (loop qj (tape-move dir (tape-write b tp)))])))

(module+ test
  (chk (tape-move 'L (tape-write 0 (tape-move 'R (tape '() '(0 1 0)))))
       (tape '() '(0 0 0))))
