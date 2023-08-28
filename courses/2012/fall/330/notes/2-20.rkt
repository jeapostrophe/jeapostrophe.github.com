#lang plai

;; TODAY
;; - Orbitz error
;; -- general property
;; -- silent actions      [0:10]
;; - stateless protocols  [0:15]
;; - add-two-numbers.exe  [0:20]
;; - add-two-numbers.com
;; -- simulated           [0:30]
;; -- lifted              [0:50]

;; COOKIE: Jay's looking at flight: #f
;; (1) Tell me how to get to Dagstulh street
;; A: 7am - 10am, $560
;; B: 7am - 8am, $576
;; ....

;; COOKIE: Jay's looking at flight: B
;; (2) Open in new window B
;; <details ....>
;; !
;; a long lay over (4 hours)

;; COOKIE: Jay's looking at flight: A
;; (1) Click on A
;; <details ....>
;; also a long lay over (3.5 hrs)

;; Jay thinks "Buy" means "Buy the flight you are looking at"
;; Orbitz thinks "Buy" means "Buy the flight in the cookie"
;; (2) Click Buy
;; <credit card>
;; <confirmation>
;; !!!!
;; You bought flight A

;; s/cookie/session

;; 2009.. class action lawsuit against Orbitz
;; Orbitz "fixed it"
;; disallowing Back and Open in New

;; HTTP is "stateless"
;; in contrast to "stateful"
;; FTP

;; FTP session:
;; $ ftp ftp.lolz.com
;; < HELO You are connected to 101 lolz all the time. Login?
;; > LOGIN username
;; > PASSWD password
;; < OKAY
;; > CWD dogz
;; < OKAY
;; > LIST
;; < awesome.jpg cool.jpg
;; > CWD catz
;; < OKAY
;; > LIST
;; < 1.jpg 2.jpg 3.jpg
;; > GET 1.jpg
;; < ....data....
;; > QUIT
;; < OKAY

;; HTTP session:
;; $ curl http://www.lolz.com/dogz/index.html
;; < awesome.jpg cool.jpg
;; $ curl http://www.lolz.com/catz/index.html
;; < 1.jpg 2.jpg 3.jpg
;; $ curl http://www.lolz.com/catz/1.jpg
;; < ...data...

(define (prompt text)
  (displayln text)
  (read))
(define (add-two-numbers.exe)
  (displayln
   (+ (prompt "First number:")
      (prompt "Second number:"))))

;; >>>>>> add-two-numbers.com >>>>>>>>>>
;; (add-two-numbers.exe) (exit 0)
;; STACK: (prompt "First number:") (prompt "Second number:") (+ ... ...) (displayln ...) (exit 0)
;; STACK: first-number (prompt "Second number:") (+ ... ...) (displayln ...) (exit 0)
;; >>>>>> add-two-numbers.com/first.cgi >>>>>
;; STACK: (prompt "Second number:") (+ first-number ...) (displayln ...) (exit 0)
;; STACK: second-number (+ first-number ...) (displayln ...) (exit 0)
;; >>>>>> add-two-numbers.com/second.cgi + first-number >>>>
;; STACK: (+ first-number second-number) (displayln ...) (exit 0)
;; STACK: answer (displayln ...) (exit 0)
;; STACK: (displayln answer) (exit 0)
;; STACK: (exit 0)

;; STACK stores "what we're doing next" AND "local variables"

(define (web-prompt text whose-next more)
  (web-displayln text whose-next more))
(define (web-displayln text whose-next more)
  (printf "<html><input type=\"hidden\" value=\"~a\"/>\n~a</html>\n" more text)
  (whose-next (read) more)
  (error 'http "I'm stateless"))
(define (add-two-numbers.com)
  (web-prompt
   "First number:"
   add-two-numbers.com/first.cgi
   #f))

(define THE-FIRST-NUMBER #f)
(define (add-two-numbers.com/first.cgi
         first-number junk)
  ;; uses a session/cookie/whatever
  ;; (set! THE-FIRST-NUMBER first-number)
  (web-prompt 
   "Second number:"
   add-two-numbers.com/second.cgi
   first-number))

(define (add-two-numbers.com/second.cgi
         second-number first-number)  
  ;; (define first-number THE-FIRST-NUMBER)
  (web-displayln
   (+ first-number
      second-number)
   void
   #f))

(add-two-numbers.com)

;; Web programming is programming without a stack
;; thus... the stack must be made manually by the programmer

;; This is error prone and painful and uggghhh
