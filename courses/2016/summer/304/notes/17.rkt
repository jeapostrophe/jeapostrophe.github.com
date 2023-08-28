#lang racket/base
(require data/enumerate)

(module+ main
  ;; from-nat : Set(A) x Nat -> A
  ;; to-nat   : Set(A) x A -> Nat
  ;; list/e : Set(A) x Set(B) -> Set(A x B)
  (for ([i (in-range 100)])
    (printf "~a. ~a\n"
            i
            (from-nat
             (list/e natural/e
                     natural/e)
             i)))

  (to-nat
   (list/e natural/e
           natural/e)
   (list 8873 42))

  (to-nat
   (list/e natural/e
           natural/e
           natural/e)
   (list 8873 42 67))

  (require data/enumerate/lib)
  (to-nat
   (listof/e natural/e)
   (list 1 2 3 4 5 6 7))

  (for ([i (in-range 100)])
    (printf "~a => ~a\n"
            i
            (from-nat (listof/e natural/e)
                      i)))

  (require math)
  (from-nat
   (listof/e natural/e)
   (random-bits 100)))
