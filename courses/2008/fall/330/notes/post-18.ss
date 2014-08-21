;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")
; Producers & Consumers
(define (make-producer generate-output)
  (local [(define prev-k (box false))]
    (lambda (real-send)
      (local [(define real-send-box (box real-send))
              (define (send ans)
                (set-box! real-send-box
                          (let/cc k
                            (begin (set-box! prev-k k)
                                   ((unbox real-send-box) ans)))))]
        (if (unbox prev-k)
            ((unbox prev-k) real-send)
            (generate-output send))))))

(define (get p)
  (let/cc k (p k)))

(define route-producer
  (make-producer
   (lambda (send)
     (begin (send 'palmyra)
            (send 'kirtland)
            (send 'nauvoo)))))

(list (get route-producer)
      (get route-producer)
      (get route-producer))

(define odds
  (make-producer
   (lambda (send)
     (let loop ([i 1])
       (send i)
       (loop (+ 2 i))))))

(get odds)
(get odds)
(get odds)
(get odds)
(get odds)
(get odds)
