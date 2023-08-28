#lang racket/base
(require racket/list)
(module+ main

  (define (thread1)
    (for ([i (in-range 20)])
      (os-printf "ping!\n"))
    (os-exit))
  (define (thread2)
    (for ([i (in-range 20)])
      (os-printf "pong!\n"))
    (os-exit))

  (define THREADS empty)
  (define (os-printf s)
    (printf s)
    (yield))
  (define (os-exit) (call-next-thread))
  (define (call-next-thread)
    (define next-thread (first THREADS))
    (set! THREADS (rest THREADS))
    (next-thread))
  (define (yield)
    (call/cc
     (λ (current-thread-state)
       (set! THREADS (append THREADS (list current-thread-state)))
       (call-next-thread))))

  (define (kernel list-of-threads)
    (set! THREADS list-of-threads)
    (define (loop)
      (unless (empty? list-of-threads)
        (yield)
        (loop)))
    (loop))

  #;(kernel (list thread1 thread2))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (one-of list-of-options)
    (cond
      [(empty? list-of-options)
       (fail)]
      [else
       (let ([old-fail fail])
         (call/cc
          (λ (how-to-go-back-and-try-again)
            (set! fail
                  (λ ()
                    (set! fail old-fail)
                    (how-to-go-back-and-try-again
                     (one-of (rest list-of-options)))))
            (first list-of-options))))]))
  (define fail
    (λ ()
      (error 'fail "No way to win")))

  (let ([a (one-of (list 4 5 6))]
        [b (one-of (list 8 9 10))])
    (printf "TRY a = ~v, b = ~v\n"
            a b)
    (unless (= (+ a b) 15)
      (fail))
    (printf " OK a = ~v, b = ~v\n"
            a b))

  


  )
