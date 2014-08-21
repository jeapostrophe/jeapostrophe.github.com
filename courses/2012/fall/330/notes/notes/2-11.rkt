#lang lazy
(module+ main

  5
  "Je"
  (+ 1 1)

  (define the-promise-formerly-known-as-4
    (let ([x (+ 1 1)])
      (printf "Adding\n")
      (+ x x)))
  "After def"
  the-promise-formerly-known-as-4
  (! the-promise-formerly-known-as-4)
  the-promise-formerly-known-as-4
  (! the-promise-formerly-known-as-4)

  (define the-promise-formerly-known-as-sigsegv
    (let ([x (/ 1 0)])
      (+ x x)))
  the-promise-formerly-known-as-sigsegv

  ;; lists

  (define l1
    (list 0
          6
          (+ 7 5)
          (* 3 3)
          (modulo 99 6)))

  l1
  (! l1)
  l1

  (define l2
    (map add1 l1))

  l2
  (first l2)
  l2
  (length l2)
  l2
  l1

  (third l2)
  l2
  l1

  (define l3
    (filter even? l1))

  l3
  (first l3)
  l3
  (second l3)
  l3
  (third l3)
  l3
  (length l3)
  l3

  l1

  l2

  ;; useful starts here

  (define ones
    (cons 1
          ones))

  ;; ones      = 1 : 1 : 1 : ....
  ;; ones      = (cons 1 (rest ones))
  ;; rest ones = 1 : 1 : 1 : ...

  (list-ref ones 700)
  (list-ref ones 7000)

  ones

  ;;(length ones)

  (define (cycle e)
    (cons e
          (cycle e)))

  (define twos
    (cycle 2))

  twos
  (list-ref twos 7)
  twos

  (define (bi-cycle x y)
    (cons x
          (cons y
                (bi-cycle x y))))
  (define 01s (bi-cycle 0 1))
  (list-ref 01s 7)
  01s

  (define 01s-only-1s
    (filter odd? 01s))
  (list-ref 01s-only-1s 7)
  01s-only-1s

  ;;(first (filter even? ones))


  (define nats
    (cons 0
          (map add1 nats)))

  ;; nats      = 0 : 1 : 2 : ...
  ;; rest nats = 1 : 2 : 3 : ...

  (list-ref nats 7)
  nats

  (define (2map f l1 l2)
    (cons (f (first l1)
             (first l2))
          (2map f (rest l1) (rest l2))))

  (define fibs
    (cons 0
          (cons 1
                (2map + fibs (rest fibs)))))

  (cons 0
        (cons 1
              (cons (+ 0
                       1)
                    (cons (+ 1
                             1)
                          (2map + (rest (rest fibs)) (rest (rest (rest fibs))))))))

  ;; fibs = 0 : 1 : 1 : 2 : 3 : 5 : ...
  ;; rest = 1 : 1 : 2 : 3 : 5 : ...
  ;; rest = 1 : 2 : 3 : 5 : ...

  (!!list (take 10 fibs))
  (list-ref fibs 700)

  (!!list (take 5 (filter even? fibs)))

  )
