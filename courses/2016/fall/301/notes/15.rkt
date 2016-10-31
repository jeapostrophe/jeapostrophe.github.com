#lang plai

(module+ idea1
  (define ZERO #b0000)
  (define  ONE #b0001)

  (for ([i (in-range 10)])
    (printf "Hey, listen!\n")))

(module+ main
  (define (ZERO f z)
    z)

  
  ;; FOUR : (Ans[n] -> Ans[n+1]) Ans[0] -> Ans[4]
  (define (FOUR f z)
    (f (f (f (f z)))))

  (FOUR (λ (a)
          (printf "Hey, you!\n"))
        (void))

  (define (SUCC n)
    (λ (f z)
      (f (n f z))))

  (define other-FOUR
    (SUCC (SUCC (SUCC (SUCC ZERO)))))

  (printf "Time 2\n")
  (other-FOUR (λ (a)
                (printf "Hey, you!\n"))
              (void))

  (define (PLUS m n)
    (λ (f z)
      (m f (n f z))))

  ((PLUS other-FOUR other-FOUR)
   (λ (a) (printf "Should be eight\n"))
   (void))


  (define MT
    (λ (c n) n))
  (define (CONS f r)
    (λ (c n) (c f (r c n))))
  
  )
