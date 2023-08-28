#lang lazy
(module+ main

  4
  "string"
  (+ 1 1)

  (define add-promise
    (let ([x (+ 1 1)])
      (printf "Adding\n")
      (+ x x)))
  add-promise
  "Knock"
  (! add-promise)
  add-promise
  "Knock"
  (! add-promise)
  "Who's there?"

  (define div-promise
    (let ([x (/ 1 0)])
      (printf "Diving\n")
      (+ x x)))
  
  ;; lists

  (define l 
    (list 1
          (modulo 13 7)
          5
          7
          (+ 8 3)
          (* 9 3)
          8
          (/ 7 0)))

  l
  (first l)
  (second l)
  l
  (sixth l)
  l

  (define l2
    (list 1
          (modulo 13 7)
          5
          7
          (+ 8 3)
          (* 9 3)
          (/ 7 0)))
  l2
  (third l2)
  l2

  (define l3-i
    (list 1
          (modulo 13 7)
          5
          7
          (+ 8 3)
          (* 9 3)
          8
          (/ 7 0)))
  (define l3
    (filter even? l3-i))
  l3
  (second l3)
  l3

  (define l4-i
    (list 1
          (modulo 13 7)
          5
          7
          (+ 8 3)
          (* 9 3)
          8))
  (define l4
    (map add1 l4-i))
  l4
  (second l4)
  l4
  (length l4)
  ;; equiv
  ;;(!list l4)
  l4

  (!!list l4)
  l4

  ;;

  (define ones
    (cons 1
          ones))

  (first ones)
  (third ones)
  (list-ref ones 30)
  (list-ref ones 3000)
  (list-ref ones (random 100))

  ones

  (define (cycle e)
    (cons e (cycle e)))

  (define ones2 (cycle 1))

  (first ones2)
  (third ones2)
  (list-ref ones2 6)

  ones2

  (define (bi-cycle x y)
    (cons x (cons y (bi-cycle x y))))

  (list-ref (bi-cycle 0 1) 70)

  (define even-ones 
    (filter even? ones))
  even-ones
  ;;(first even-ones)

  (define nats
    (cons 0
          (map add1 nats)))

;; nats        =  0 : 1 : 2 : 3 : 4 ....
;; (rest nats) =      1 : 2 : 3 : 4 : 5
;; (rest nats) = (map add1 nats)

  (list-ref nats 70)

  (define fibs
    (cons 0
          (cons 1
                (map +
                     fibs
                     (rest fibs)))))


;;  fibs               =  1 : 1 : 2 : 3 : 5 : ...
;;  (rest fibs)        =  1 : 2 : 3 : ...
;;  (rest (rest fibs)) =  2 : 3 : 5 : ...

  (!!list (take 10 fibs))
  (list-ref fibs 500)

  ;; (define (every-chess-board-ever initial-board)
  ;;   (list initial-board
  ;;         (map all-the-first-players-moves initial-board)
  ;;         (map all-the-second-players-moves
  ;;              (map all-the-first-players-moves 
  ;;                   initial-board))
  ;;         (map every-chess-board-ever
  ;;              (map all-the-second-players-moves
  ;;                   (map all-the-first-players-moves 
  ;;                        initial-board)))))
  ;; (every-chess-board-ever the-real-initial-board)

  )
