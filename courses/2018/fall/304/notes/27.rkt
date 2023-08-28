#lang racket/base
(require racket/set
         racket/list
         racket/match)
(module+ test
  (require chk
           racket/pretty))

(struct dfa (q start accept? trans)
  #:transparent)

;; dfa-interp : DFA x String -> Bool
(define (dfa-interp some-dfa some-str)
  (match-define (dfa q start accept? trans) some-dfa)
  #;(unless (set-member? q start)
      (error 'dfa-interp "Invalid start state: ~e" start))
  (define current-state start)
  (for ([some-char (in-list some-str)])
    (define next-state (trans current-state some-char))
    #;(unless (set-member? q next-state)
        (error 'dfa-interp "Invalid state returned from trans: ~e" next-state))
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

  (define (dfa&tm-interp d s)
    (define dr (dfa-interp d s))
    (define tr (tm-interp (dfa->tm d) s))
    (if (eq? dr tr)
      dr
      (error 'dfa&tm-interp "returned different things!")))

  (define (chk-dfa d s r)
    (define nd (dfa-complement d))
    (with-chk (['d d] ['s s] ['r r])
      (with-chk (['mode 'normal])
        (chk (dfa&tm-interp d s) r))
      (with-chk (['mode 'complement])
        (chk (dfa&tm-interp nd s) (not r)))
      (with-chk (['mode 'intersect])
        (chk (dfa&tm-interp (dfa-intersect d nd) s) #f))
      (with-chk (['mode 'union])
        (chk (dfa&tm-interp (dfa-union d nd) s) #t))))

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
(struct tm (start trans) #:transparent)

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

(define (display-config qi tp)
  (match-define (tape behind ahead) tp)
  (for-each display (reverse behind))
  (printf "[~a]" qi)
  (for-each display ahead)
  (printf "\n"))

(define (tm-interp a-tm a-str #:display? [display? #f])
  (match-define (tm start trans) a-tm)
  (let loop ([qi start] [tp (tape empty a-str)])
    (when display?
      (display-config qi tp))
    (cond
      [(eq? 'accept qi) #t]
      [(eq? 'reject qi) #f]
      [else
       (match-define (list qj b dir) (trans qi (tape-head tp)))
       (loop qj (tape-move dir (tape-write b tp)))])))

(module+ test
  (chk (tape-move 'L (tape-write 0 (tape-move 'R (tape '() '(0 1 0)))))
       (tape '() '(0 0 0))))

(module+ test
  (define w!w
    (tm 'start
        (match-lambda*
          ;; In the beginning, check each character
          ['(start 0) '(move-to-right&look-for-0 _ R)]
          ['(move-to-right&look-for-0 0) '(move-to-right&look-for-0 0 R)]
          ['(move-to-right&look-for-0 1) '(move-to-right&look-for-0 1 R)]
          ['(move-to-right&look-for-0 !) '(look-for-0 ! R)]
          ['(look-for-0 ☠) '(look-for-0 ☠ R)]
          ['(look-for-0 0) '(go-all-the-way-left ☠ L)]

          ['(start 1) '(move-to-right&look-for-1 _ R)]
          ['(move-to-right&look-for-1 0) '(move-to-right&look-for-1 0 R)]
          ['(move-to-right&look-for-1 1) '(move-to-right&look-for-1 1 R)]
          ['(move-to-right&look-for-1 !) '(look-for-1 ! R)]
          ['(look-for-1 ☠) '(look-for-1 ☠ R)]
          ['(look-for-1 1) '(go-all-the-way-left ☠ L)]

          ;;; Go back to the start
          ['(go-all-the-way-left 0) '(go-all-the-way-left 0 L)]
          ['(go-all-the-way-left 1) '(go-all-the-way-left 1 L)]
          ['(go-all-the-way-left !) '(go-all-the-way-left ! L)]
          ['(go-all-the-way-left ☠) '(go-all-the-way-left ☠ L)]
          ['(go-all-the-way-left _) '(start _ R)]

          ['(start !) '(check-skulls _ R)]
          ['(check-skulls ☠) '(check-skulls _ R)]
          ['(check-skulls _) '(accept _ R)]

          [_ '(reject _ R)])))

  (chk (tm-interp w!w '(0 1 1 ! 0 1 1)) #t)
  (chk (tm-interp w!w '(0 1 0 ! 0 1 1)) #f)
  (chk (tm-interp w!w '(0 1 ! 0 1 1)) #f)
  (chk (tm-interp w!w '(0 1 0 ! 0 1)) #f)
  (chk (tm-interp w!w '(0 ! 0 ! 0 1)) #f))

(define (dfa->tm x)
  (match-define (dfa _ x-start x-accept? x-trans) x)
  (define start x-start)
  (define (trans qi c)
    (cond
      [(eq? c '_)
       (if (x-accept? qi)
         '(accept _ R)
         '(reject _ R))]
      [else
       (list (x-trans qi c) '_ 'R)]))
  (tm start trans))

;; Deterministic PDAs

(struct dpda (start accept? trans) #:transparent)

(define (dpda-interp d s)
  (match-define (dpda start accept? trans) d)
  (let loop ([qi start] [stack '($)] [input (append s (list '^))])
    ;;
    (for-each display (reverse stack))
    (printf "[~a]" qi)
    (for-each display input)
    (printf "\n")
    ;;
    (cond
      [(empty? input)
       (accept? qi)]
      [else
       (match-define (list qj action) (trans qi (first input) (first stack)))
       (define new-stack
         (match action
         ['pop (rest stack)]
         [(list 'push c) (cons c stack)]
         [(list 'replace c) (cons c (rest stack))]
         ['ignore stack]))
       (loop qj new-stack (rest input))])))

(module+ test
  (define 0n1n ;; where n>1
    (dpda 'start (λ (qi) (eq? qi 'accept))
          (match-lambda*
            [(list 'start 0 _) '(start (push 0))]
            [(list 'start 1 0) '(poppin pop)]
            [(list 'poppin 1 0) '(poppin pop)]
            [(list 'poppin '^ '$) '(accept pop)]
            [_ '(reject ignore)])))

  (chk (dpda-interp 0n1n '(0 0 1 1)) #t)
  (chk (dpda-interp 0n1n '()) #f)
  (chk (dpda-interp 0n1n '(0 1)) #t)
  (chk (dpda-interp 0n1n '(0 0 1)) #f)
  (chk (dpda-interp 0n1n '(0 0 1 1 1)) #f)
  (chk (dpda-interp 0n1n '(0 0 0 0 0 1 1 1 1 1)) #t))

;;; CFGs

(struct cfg (start rules) #:transparent)

(define (random-list-ref l)
  (list-ref l (random (length l))))

(define (cfg-generate c)
  (match-define (cfg start rs) c)
  (let loop ([s (list start)])
    (cond
      [(empty? s) s]
      [(hash-ref rs (first s) #f)
       => (λ (opts)
            (loop (append (random-list-ref opts) (rest s))))]
      [else
       (cons (first s) (loop (rest s)))])))

(module+ test
  (define 0n1n-cfg ;; where n > 0
    (cfg 'S (hasheq 'S '((0 1) (0 S 1)))))

  (for ([i (in-range 100)])
    (chk (dpda-interp 0n1n (cfg-generate 0n1n-cfg)) #t)))

;;; Binary Addition Checker

(module+ test
  (define bin-eq-chk
    (tm 'check-if-lhs-0
        (match-lambda*
          ['(check-if-lhs-0 0) '(check-if-lhs-0 0 R)]
          ['(check-if-lhs-0 1) '(move-right&sub1-lhs&sub1-rhs 1 R)]
          ['(check-if-lhs-0 =) '(move-left&erase-lhs = L)]
          ['(move-left&erase-lhs 0) '(move-left&erase-lhs 0 L)]
          ['(move-left&erase-lhs 1) '(move-left&erase-lhs 1 L)]
          ['(move-left&erase-lhs _) '(erase-lhs _ R)]
          ['(erase-lhs 0) '(erase-lhs _ R)]
          ['(erase-lhs 1) '(erase-lhs _ R)]
          ['(erase-lhs =) '(check-if-rhs-0 _ R)]

          ['(move-right&sub1-lhs&sub1-rhs 0) '(move-right&sub1-lhs&sub1-rhs 0 R)]
          ['(move-right&sub1-lhs&sub1-rhs 1) '(move-right&sub1-lhs&sub1-rhs 1 R)]
          ['(move-right&sub1-lhs&sub1-rhs =) '(sub1-lhs&sub1-rhs = L)]

          ;; Move to right of number, and change the first 1 into a 0,
          ;; and the 0s before that into 1s
          ['(sub1-lhs&sub1-rhs 0) '(sub1-lhs&sub1-rhs 1 L)]
          ['(sub1-lhs&sub1-rhs 1) '(find-rhs&sub1 0 R)]

          ['(find-rhs&sub1 0) '(find-rhs&sub1 0 R)]
          ['(find-rhs&sub1 1) '(find-rhs&sub1 1 R)]
          ['(find-rhs&sub1 =) '(find-rhs&sub1 = R)]
          ['(find-rhs&sub1 _) '(sub1-rhs _ L)]

          ['(sub1-rhs 0) '(sub1-rhs 1 L)]
          ['(sub1-rhs 1) '(find-beginning 0 L)]

          ['(find-beginning 0) '(find-beginning 0 L)]
          ['(find-beginning 1) '(find-beginning 1 L)]
          ['(find-beginning =) '(find-beginning = L)]
          ['(find-beginning _) '(check-if-lhs-0 _ R)]

          ['(check-if-rhs-0 0) '(check-if-rhs-0 _ R)]
          ['(check-if-rhs-0 1) '(reject _ R)]
          ['(check-if-rhs-0 _) '(accept _ R)]
          
          [_ '(reject _ R)])))

  (chk (tm-interp bin-eq-chk '(0 = 0)) #t)
  (chk (tm-interp bin-eq-chk '(0 = 1)) #f)
  (chk (tm-interp #:display? #t bin-eq-chk '(1 = 1)) #t)
  (chk (tm-interp bin-eq-chk '(1 = 0)) #f)
  (chk (tm-interp bin-eq-chk '(0 1 = 0 1)) #t)
  (chk (tm-interp #:display? #t bin-eq-chk '(0 1 = 0 0 1)) #t)
  (chk (tm-interp bin-eq-chk '(0 0 1 = 0 1)) #t)
  (chk (tm-interp #:display? #t bin-eq-chk '(0 0 1 = 1 1)) #f)
  (chk (tm-interp bin-eq-chk '(1 1 = 1 1)) #t)

  (define (random-number-of-0s)
    (for/list ([i (in-range (random 100))]) 0))
  (for ([i (in-range 100)])
    (define n
      (map (match-lambda
             [#\0 0] [#\1 1])
           (string->list (number->string (random 1000) 2))))
    (chk (tm-interp bin-eq-chk
                    (append (random-number-of-0s) n (list '=) (random-number-of-0s) n))
         #t)))
