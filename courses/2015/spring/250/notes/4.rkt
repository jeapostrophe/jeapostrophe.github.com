#lang racket/base
(require racket/generator
         racket/math)

(define (in-prng prng s)
  (in-generator
   (let loop ([s s])
     (define n (prng s))
     (yield n)
     (loop n))))

(define (bit x i)
  (bitwise-bit-field x i (+ i 1)))

(define ((bbs m) x_0)
  (modulo (sqr x_0) m))
(module+ test
  (printf "BBS: m = 11 * 19, x_0 = 3\n")
  (define ?
    (or (λ (x) x)
        (λ (x) (bit x 0))
        
        even?
        odd?))
  (for ([i (in-range 25)]
        [v (in-prng (bbs (* 11 19)) 3)])
    (printf " ~a" (? v)))
  (printf "\n"))

(define ((lfsr) x)
  (define other-bit 1)
  (define feedback
    (bitwise-xor (bit x 0)
                 (bit x other-bit)))
  (define shifted-ref (arithmetic-shift x -1))
  (define feedback-at-bit14 (arithmetic-shift feedback 14))
  (bitwise-ior shifted-ref feedback-at-bit14))

(module+ test
  (printf "LFSR\n")
  (for ([i (in-range 50)]
        [v (in-prng (lfsr) 1)])
    (printf " ~a" v))
  (printf "\n"))
