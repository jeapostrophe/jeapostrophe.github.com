#lang plai

(define (expect predicate who-to-blame value)
  (unless (predicate value)
    (error 'expect "Got ~e, expected ~e, blaming ~e"
           value predicate who-to-blame)))

;; add : num num -> num
(define (add lhs rhs)
  ;; Assertions or pre-conditions
  (expect number? 'add-caller lhs)
  (expect number? 'add-caller rhs)
  (define ans
    (if (= lhs 5)
      "42"
      (bitwise-and lhs rhs)))
  ;; Assertions or post-conditions
  (expect number? 'add-implementation ans)
  ans)

(define ((function-from doms rng) a-function-to-test)
  #t)

(expect (function-from (list number? number?)
                       number?)
        'add-impl
        add)

(test/exn (add 5 "5") "blaming 'add-caller")

;; fizz : num num -> num
(define (fizz x y)
  (expect number? 'fizz-caller x)
  (expect number? 'fizz-caller y)
  (define ans (add x y))
  (define ans2 (add ans ans))
  (expect number? 'fizz-impl ans2)
  ans2)

(test/exn (fizz 5 5) "blaming 'add-implementation")
(test/exn (fizz 5 "5") "blaming 'fizz-caller")

(define ((listof elem/ctc) value)
  (and (list? value)
       (andmap elem/ctc value)))

;; numsort : (num num -> bool) (list num) -> (list num)
(define (numsort cmp l)
  (expect (listof number?)
          'numsort-caller l)
  (expect (function-from (list number? number?) boolean?)
          'numsort-caller cmp))

;; assertion : Value -> Boolean where #t means assertion obeyed
;; contract : Value Blame Blame -> Value is guaranteed to obey contract

(define (is-positive-number? x)
  (and (number? x)
       (positive? x)))
(define (positive-number/ctc pos neg x)
  (if (and (number? x)
           (positive? x))
    x
    (error 'contract "~e Expected positive-number/ctc, got ~e"
           pos x)))
(define ((function-from2 fst-arg/ctc snd-arg/ctc
                         result/ctc)
         pos neg x)
  (if (procedure? x)
    (λ (fst-arg snd-arg)
      (result/ctc pos neg
                  (x (fst-arg/ctc neg pos fst-arg)
                     (snd-arg/ctc neg pos snd-arg))))
    (error 'contract "Expected function, got ~e"
           x)))

(define totally-posi/ctc
  (function-from2 positive-number/ctc
                  positive-number/ctc
                  positive-number/ctc))

(define (inner-add x y)
  (if (= x 5)
    "42"
    (+ x y)))

(define safe-add
  (totally-posi/ctc 'add-impl 'add-caller
                    inner-add))

(test (safe-add 2 2) 4)
(test/exn (safe-add "2" 2) "Expected positive")
(test/exn (safe-add 5 2) "Expected positive")

(define (inner-insert cmp x l)
  (if (empty? l)
    (list x)
    (if (cmp x (first l))
      (cons (if (= x 5)
              "five"
              x)
            l)
      (cons (first l)
            (inner-insert cmp x (rest l))))))
(define (inner-sort cmp l)
  (if (empty? l)
    empty
    (inner-insert cmp (first l)
                  (inner-sort cmp (rest l)))))

(define (boolean/ctc pos neg x)
  (if (and (boolean? x))
    x
    (error 'contract "~e Expected boolean/ctc got ~e"
           pos x)))
(define ((list/ctc elem/ctc) pos neg v)
  (if (list? v)
    (for/list ([e (in-list v)])
      (elem/ctc pos neg e))
    (error 'contract "~e Expected list, got ~e"
           pos v)))

(define safe-sort
  ((function-from2 (function-from2
                    positive-number/ctc positive-number/ctc
                    boolean/ctc)
                   (list/ctc positive-number/ctc)
                   (list/ctc positive-number/ctc))
   'sort-impl 'sort-caller
   inner-sort))

(test (safe-sort < (list 2 4 8 1 23 6))
      (list 1 2 4 6 8 23))
(test/exn (safe-sort < (list 2 "4" 8 1 23 6))
          "Expected positive")
(test/exn (safe-sort < (list 2 5 8 1 23 6))
          "Expected positive")
(test/exn (safe-sort (λ (x y)
                       "true")
                     (list 2 4 8 1 23 6))
          "Expected boolean")

;; Are contracts just types? Do you need them with Java?
;; Interaction with typed programs
;; Other kinds of contracts (temporal contracts)
;; Performance of contracts
;; Recursion and contracts
;; Contracts and mutation
