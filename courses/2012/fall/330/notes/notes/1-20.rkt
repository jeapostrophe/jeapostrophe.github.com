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

;; GET /A/
;; GET /B/
;; <click Back>
;; GET /A/

;; HTTP vs FTP

;; FTP session
;; $ ftp ftp.lolz.com
;; < HELO "We haz ur catsz"
;; > LOGIN loldood
;; > PASSWD chezzebagrgr
;; < OKAY
;; > LIST /
;; < 1.jpg 2.jpg 3.jpg
;; > GET 1.jpg
;; < "funny cat wearing melon hat" as JPG
;; > CWD /dogz
;; < OKAY
;; > LIST
;; < different stuff
;; > QUIT

;; FTP is "stateful" protocol

;; HTTP session
;; $ curl http://www.lolz.com/1.jpg
;; > GET http://www.lolz.com/1.jpg
;; < "funny cat wearing melon hat" as JPG
;; $ curl http://www.lolz.com/awesome.jpg
;; > GET http://www.lolz.com/1.jpg
;; $ curl http://www.lolz.com/dogz/index.html
;; $ curl http://www.lolz.com/dogz/amazing.jpg



;; * HTTP/1.1. has optimizations for this sort of thing

;; HTTP is "stateless"

(define (prompt text)
  (printf "~a\n" text)
  (read))

(define (add-two-numbers.exe)
  (displayln
   (+ (prompt "First number:")
      (prompt "Second number:"))))

;; .com >>>>>
;; STACK: (displayln ...) (exit 0)
;; PUSH: (+ ... ...) (displayln ...) (exit 0)
;; PUSH: (prompt "First") (prompt "Second") (+ ... ...) (displayln ...) (exit 0)
;; <<<<<<<<<<
;; .com/first.cgi >>>>>
;; EVAL: (prompt "Second") (+ first-number ...) (displayln ...) (exit 0)
;; <<<<<<<<<<
;; .com/second.cgi + first-number >>>>
;; EVAL: (+ first-number second-number) (displayln ...) (exit 0)
;; EVAL: (displayln answer) (exit 0)
;; EVAL: (exit 0)
;; DONE!

;; (add-two-numbers.exe)

(define (web-prompt text
                    where-to-send-the-answer
                    more-stuff)
  (web-displayln text
                 where-to-send-the-answer
                 more-stuff))

(define (web-displayln v
                       where-to-send-the-answer
                       more-stuff)
  (printf "<html>~a</html>\n" v)
  (where-to-send-the-answer (read) more-stuff))

(define (add-two-numbers.com)
  (web-prompt
   "First number:"
   add-two-numbers.com/i-have-the-first-number.cgi
   empty))

(define THE-FIRST-NUMBER #f)
(define (add-two-numbers.com/i-have-the-first-number.cgi first-number extra)
  ;; not multi-user
  (set! THE-FIRST-NUMBER first-number)
  ;; not multi-window
  ;; (cookie-set! THE-FIRST-NUMBER first-number)
  ;; (session-set! THE-FIRST-NUMBER first-number)
  (web-prompt
   "Second number:"
   add-two-numbers.com/i-have-the-second-number.cgi
   empty
   #;(list first-number)))

(define (add-two-numbers.com/i-have-the-second-number.cgi second-number extra)
  (define first-number 
    THE-FIRST-NUMBER
    #;(first extra))
  (web-displayln
   (+ first-number
      second-number)
   (λ (answer more)
     (error 'done))
   empty))

(add-two-numbers.com)
