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
(define saved-cmp (box #f))
(define (inner-sort real-cmp l)
  (define cmp 
    (if (unbox saved-cmp)
      (unbox saved-cmp)
      (begin (set-box! saved-cmp real-cmp)
             real-cmp)))
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

;; Contracts are "refinement types"
(define (digit/ctc x)
  (if (and (number? x)
           (>= x 0)
           (<= x 9))
    x
    (error 'digit)))

;; Dependent contract == dependent types

(define (oracle/ctc x)
  (printf "Is this value okay? ~v\n" x)
  (if (read)
    x
    (error)))

;; Other kinds of contracts (temporal contracts)
;; -- Contract monitor decides if the "trace" is okay

;; Interaction with typed programs

;; untyped f : a -> b
;;   typed g : c -> d
;;;; (f X)

;; Things to go wrong:
;; X is not an a ----- not possible
;; (f X) is not a b ---    possible

;; Typed calls Untyped a Contract checks the Result
;; Untyped calls Typed a Contract checks the Input
;; The theorem that proves is "Typed programs can't be blamed"

;; Polymorphism

;; untyped f : forall A, A -> A
;; Jacob Matthews and Arjun Guha

;; Performance of contracts
;; -- They cost /something/

;; f : num num -> num
;; g : num num -> num

;; (g (f 1 2) (f 3 4))

;; Jeremy Siek... gradual typing

;; Langs Value = Pair Actual-Value {Checked Contracts}

;; Recursion and contracts
(define (odd? x)
  (if (zero? x)
    #f
    (even? (sub1 x))))
(define (even? x)
  (if (zero? x)
    #t
    (odd? (sub1 x))))

;; odd? : num -> bool
;; even? : num -> bool

(safe-even? 3)
(bool/ctc (even? 3))
(bool/ctc (bool/ctc (odd? 2)))
(bool/ctc (bool/ctc (bool/ctc (even? 1))))
(bool/ctc (bool/ctc (bool/ctc (bool/ctc (odd? 0)))))
(bool/ctc (bool/ctc (bool/ctc (bool/ctc false))))
;; continuation-marks do this

;; Space-Efficient Gradual Typing

;; Contracts and mutation

(define (box/ctc elem/ctc)
  (λ (x)
    (if (box? x)
      (box-chaperone x elem/ctc)
      (error))))
