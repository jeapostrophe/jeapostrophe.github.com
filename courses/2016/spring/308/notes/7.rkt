#lang racket/base
(require racket/match
         racket/list)

(define-syntax-rule (~struct arg ...)
  (struct arg ... #:transparent))

;; Computer = OS x Processes
;; Process = Local-State x Threads
;; Thread = Control

(~struct ~thread (code stack))

;; Code...
(~struct ~code ())
(~struct ~binary ~code (lhs op rhs))
(~struct ~if ~code (cond-c true-c false-c))
(~struct ~num ~code (num))
(~struct ~while ~code  (cond-c body-c))
(~struct ~seqn ~code (first-c second-c))
(~struct ~memset ~code (addr-c val-c))
(~struct ~memref ~code (addr-c))
(~struct ~0x80 ~code (eval-c uneval-c))

;; code-run : code -> answer
#;
(define (code-run code)
  (match code
    [(~binary lhs op rhs)
     (op (code-run lhs)
         (code-run rhs))]
    [(~if cond-c true-c false-c)
     (if (zero? (code-run cond-c))
         (code-run true-c)
         (code-run false-c))]
    [(~num num)
     num]
    [(~while cond-c body-c)
     (code-run
      (~if cond-c
           (~seqn body-c
                  code)
           (~num -1)))]
    [(~seqn first-c second-c)
     (begin (code-run first-c)
            (code-run second-c))]))
#;
(code-run
 (~while (~num 1) (~binary (~num 5) + (~num 37))))

(~struct ~stack ())
(~struct ~halt ~stack ())
(~struct ~seqn-after-first ~stack (second-c st))
(~struct ~if-after-cond ~stack (true-c false-c st))
(~struct ~binary-after-lhs ~stack (op rhs-c st))
(~struct ~binary-after-rhs ~stack (lhs-v op st))
(~struct ~memset-after-addr ~stack (val-c st))
(~struct ~memref-after-addr ~stack (st))
(~struct ~memset-after-val ~stack (addr-c st))
(~struct ~0x80-after-eval ~stack (uneval-c st))

;; thread-step : mem x thread -> thread
(define (thread-step mem t)
  (match-define (~thread c st) t)
  (match c
    [(~0x80 eval-c uneval-c)
     (~thread eval-c
              (~0x80-after-eval uneval-c st))]
    [(~memset addr-c val-c)
     (~thread addr-c
              (~memset-after-addr val-c st))]
    [(~memref addr-c)
     (~thread addr-c
              (~memref-after-addr st))]
    [(~binary lhs op rhs)
     (~thread lhs
              (~binary-after-lhs op rhs
                                 st))]
    [(~if cond-c true-c false-c)
     (~thread cond-c
              (~if-after-cond true-c false-c
                              st))]
    [(~while cond-c body-c)
     (~thread
      (~if cond-c
           (~seqn body-c
                  c)
           (~num -1))
      st)]
    [(~seqn first-c second-c)
     (~thread first-c
              (~seqn-after-first second-c
                                 st))]
    [(~num num)
     (match st
       [(~halt) t]
       [(~seqn-after-first second-c st)
        (~thread second-c st)]
       [(~if-after-cond true-c false-c st)
        (if (zero? num)
            (~thread true-c st)
            (~thread false-c st))]
       [(~binary-after-lhs op rhs-c st)
        (~thread rhs-c
                 (~binary-after-rhs num op st))]
       [(~binary-after-rhs lhs-v op st)
        (~thread (~num (op lhs-v num)) st)]
       [(~memref-after-addr st)
        (~thread (~num (vector-ref mem num)) st)]
       [(~memset-after-addr val-c st)
        (~thread val-c
                 (~memset-after-val num st))]
       [(~memset-after-val addr-v st)
        (vector-set! mem addr-v num)
        (~thread (~num num) st)]
       [(~0x80-after-eval uneval-c st)
        (~thread-system-call! num uneval-c st)])]))

(define EXIT-THREAD 0)
(define SPAWN-THREAD 1)
(define SPAWN-PROC 2)
(define EXIT-PROC 3)
(~struct ~thread-system-call!
         (num uneval-c calling-st))
(~struct ~process-system-call!
         (num uneval-c calling-st proc-mem other-proc-ts))

(thread-step
 (make-vector 32 0)
 (~thread
  (~while (~num 0)
          (~binary (~num 5) + (~num 37)))
  (~halt)))

#;
(define (thread-stepN mem t num)
  (cond
    [(zero? num)
     t]
    [else
     (define next-t (thread-step mem t))
     (if (~system-call!? next-t)
         next-t
         (thread-stepN mem next-t (sub1 num)))]))

#;
(define test-mem0 (make-vector 32 0))
#;
(thread-stepN
 test-mem0
 (~thread
  (~while (~num 0)
          (~memset (~num 0)
                   (~binary (~num 1) + (~memref (~num 0)))))
  (~halt))
 600)
#;
(vector-ref test-mem0 0)

;; Processes...

(~struct ~process (mem threads))

(define (process-step p)
  (match-define (~process mem ts) p)
  ;; Pick a thread...
  (define chosen-t (first ts))
  (match (thread-step mem chosen-t)
    [(? ~thread? new-chosen-t)
     (define new-ts (append (rest ts) (list new-chosen-t)))
     (~process mem new-ts)]
    [(~thread-system-call! syscall uneval-c st)
     (cond
       [(= EXIT-THREAD syscall)
        (~process mem (rest ts))]
       [(= SPAWN-THREAD syscall)
        (~process mem
                  (append (rest ts)
                          (list (~thread uneval-c (~halt))
                                (~thread (~num 0) st))))]
       [else
        (~process-system-call! syscall
                               uneval-c
                               st
                               mem
                               (rest ts))])]))

#;
(define (process-stepN p num)
  (if (zero? num)
      p
      (process-stepN (process-step p) (sub1 num))))

#;
(define test-mem1 (make-vector 32 0))
#;
(process-stepN
 (~process
  test-mem1
  (list
   (~thread
    (~while (~num 0)
            (~memset (~num 0)
                     (~binary (~memref (~num 0)) + (~num 1))))
    (~halt))
   (~thread
    (~while (~num 0)
            (~seqn
             (~binary (~num 37) + (~num 5))
             (~memset (~num 0)
                      (~binary (~memref (~num 0)) - (~num 1)))))
    (~halt))))
 1000)
#;
(vector-ref test-mem1 0)

;; Computer
(~struct ~computer (procs))

(define (computer-step c)
  (match-define (~computer ps) c)
  (define chosen-p (first ps))
  (match (process-step chosen-p)
    [(? ~process? new-chosen-p)
     (~computer (append (rest ps) (list new-chosen-p)))]
    [(~process-system-call! syscall uneval-c
                            calling-st proc-mem other-proc-ts)
     (cond
       [(= syscall SPAWN-PROC)
        (define new-process
          (~process (make-vector 32 0)
                    (list (~thread uneval-c
                                   (~halt)))))
        (define new-chosen-p
          (~process proc-mem
                    (append other-proc-ts
                            (list (~thread (~num 0)
                                           calling-st)))))
        (~computer (append (rest ps)
                           (list new-process
                                 new-chosen-p)))]
       [(= syscall EXIT-PROC)
        (~computer (rest ps))]
       [else
        (error 'syscall "Unknown syscall")])]))

(define (computer-stepN p num)
  (if (zero? num)
      p
      (computer-stepN (computer-step p) (sub1 num))))

(define (fast-doer op)
  (~thread
   (~while (~num 0)
           (~memset (~num 0)
                    (~binary (~memref (~num 0)) op (~num 1))))
   (~halt)))
(define fast-incrementer (fast-doer +))
(define fast-decrementer (fast-doer -))
(define slow-decrementer
  (~thread
   (~while (~num 0)
           (~seqn
            (~binary (~num 37) + (~num 5))
            (~memset (~num 0)
                     (~binary (~memref (~num 0)) - (~num 1)))))
   (~halt)))
(define test-fast-proc
  (~process (make-vector 32 0)
            (list fast-incrementer fast-decrementer)))
(define test-slow-proc
  (~process (make-vector 32 0)
            (list fast-incrementer slow-decrementer)))
(define test-computer
  (~computer (list test-fast-proc test-slow-proc)))

(computer-stepN test-computer 2000)

;; Three things...
;; - Spawn threads & processes

(define dying-thread
  (~0x80 (~num EXIT-THREAD) (~num 6)))
(define spawn-thread-proc
  (~while (~num 0)
          (~0x80 (~num SPAWN-THREAD)
                 dying-thread)))
(define spawn-proc-thread
  (~thread
   (~while (~num 0)
           (~0x80 (~num SPAWN-PROC)
                  spawn-thread-proc))
   (~halt)))
(define spawning-proc
  (~process (make-vector 32 0)
            (list spawn-proc-thread)))
(define spawning-computer
  (~computer (list spawning-proc)))
(computer-stepN spawning-computer 2000)

;; Useful spawning

(define (spawn-fast-doer op)
  (~while (~num 0)
          (~memset (~num 0)
                   (~binary (~memref (~num 0)) op (~num 1)))))
(define spawn-fast-incrementer (spawn-fast-doer +))
(define spawn-fast-decrementer (spawn-fast-doer -))
(define spawn-slow-decrementer
  (~while (~num 0)
          (~seqn
           (~binary (~num 37) + (~num 5))
           (~memset (~num 0)
                    (~binary (~memref (~num 0)) - (~num 1))))))
(define spawn-fast-proc
  (~seqn (~0x80 (~num SPAWN-THREAD) spawn-fast-incrementer)
         (~seqn (~0x80 (~num SPAWN-THREAD) spawn-fast-decrementer)
                (~0x80 (~num EXIT-THREAD) (~num 0)))))
(define spawn-slow-proc
  (~seqn (~0x80 (~num SPAWN-THREAD) spawn-fast-incrementer)
         (~seqn (~0x80 (~num SPAWN-THREAD) spawn-slow-decrementer)
                (~0x80 (~num EXIT-THREAD) (~num 0)))))
(define useful-spawning-thread
  (~thread
   (~seqn (~0x80 (~num SPAWN-PROC) spawn-fast-proc)
          (~seqn (~0x80 (~num SPAWN-PROC) spawn-slow-proc)
                 (~0x80 (~num EXIT-PROC) (~num 0))))
   (~halt)))
(define useful-spawning-proc
  (~process (make-vector 32 0)
            (list useful-spawning-thread)))
(define useful-spawning-computer
  (~computer (list useful-spawning-proc)))
(computer-stepN useful-spawning-computer 2000)

;; - Show virtual memory
;; - Print to the screen
