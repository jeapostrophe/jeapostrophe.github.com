#lang plai
(print-only-errors #t)
(halt-on-errors #t)

;; Control

;; - Where the program is executing... i.e. the program counter or the
;; Turing Machine State

;; - Non-local control (not like ssh)... like threads/forks,
;; exceptions, interrupts, GOTOs, COMEFROM

;;           int f () {
;;        FILE fd;
;;        int ans = -1;

;;        fd = fopen("/etc/passwd", O_READ);
;;        if (! fd) { goto fail; }

;;        void* mblock = malloc(42);
;;        if (! mblock ) { goto fail1; }

;;        ans = mblock[5] + fd;

;;        free(mblock);
;;  fail1: fclose(fd);
;;  fail: return ans;
;; }

;; int f2 () {
;;   FILE fd;
;;   int ans = -1;

;;   fd = fopen("/etc/passwd", O_READ);
;;   if ( fd ) {
;;     void* mblock = malloc(42);
;;     if ( mblock ) {
;;       ans = mblock[5] + fd;

;;       free(mblock);
;;     }
;;   }
;;   fclose(fd);
;;   return ans;
;; }

(define (read-number prompt)
  (displayln prompt)
  (read))

(define (add-two-numbers.exe)
  (displayln (+ (read-number "First")
                (read-number "Second"))))

;; (add-two-numbers.exe)

;; Web sites are stateless because HTTP is stateless
(define dispatch-table (make-hash))
(define (new-label)
  (hash-count dispatch-table))

(define (read-number/and-then prompt and-then)
  (define label (new-label))
  (hash-set! dispatch-table label
             and-then)
  (printf "~a\nGo to ~a with answer\n"
          prompt
          label)
  (error 'end))

(define (firefox label answer)
  ((hash-ref dispatch-table label)
   answer))

(define (add-two-numbers.com)
  (read-number/and-then "First"
                        add-two-numbers.com/given-first))
(define first-num-cookie 0)
(define (add-two-numbers.com/given-first first-num)
  ;; Closure contained first-num
  ;; (define (add-two-numbers.com/given-second second-num)
  ;;   (displayln (+ first-num
  ;;                 second-num)))
  (set! first-num-cookie first-num)
  (read-number/and-then "Second"
                        add-two-numbers.com/given-second))
(define (add-two-numbers.com/given-second second-num)
  (displayln (+ first-num-cookie
                second-num)))


;;(add-two-numbers.com)

;;; hidden form fields

(define (read-number/and-then+hidden prompt and-then hidden)
  (define label (new-label))
  (hash-set! dispatch-table label
             and-then)
  (printf "~a\nGo to ~a with answer and tell me ~a\n"
          prompt
          label
          hidden)
  (error 'end))

(define (firefox+hidden label answer hidden)
  ((hash-ref dispatch-table label)
   answer hidden))

(define (add-two-numbers.com+hidden)
  (read-number/and-then+hidden
   "First"
   add-two-numbers.com/given-first+hidden
   #f))
(define (add-two-numbers.com/given-first+hidden 
         first-num
         ignored)
  (read-number/and-then+hidden
   "Second"
   add-two-numbers.com/given-second+hidden
   first-num))
(define (add-two-numbers.com/given-second+hidden 
         second-num first-num)
  (displayln (+ first-num
                second-num)))

(add-two-numbers.com+hidden)

;; CPS
