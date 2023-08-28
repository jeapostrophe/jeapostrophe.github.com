;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
#|

interactive programming --- how fast changes affect the result
general enough to apply to anything but still usable
memory management & garbage collection <-------- tyler doesn't care... tyler doesn't need it
clarity and non-ambiguity of the syntax *
what type of problem the language is supposed to solve --- C vs Java
impl -- compiled vs. interpreted
impl -- what type of env will it run in -- os vs web
impl -- what tools and libs are available (tools = libs, and builtins)
expressivity and conciseness (Ruby's array manipulation)
what abstractions manage complexity (functions, classes, ...)
"a language is just syntax anyways"
without libraries... aren't they all the same?

|#

5

"something string"

'symbol

(- 100 0)
(- 100 (* 2 5))

;; ( fun arg ... )

;; 1  + 2
;; f(2,3);

(string-append "Jay" " " "McCarthy")

(check-expect (- 100 (* 3 5))
              85)
(check-expect (- 100 (* 4 5))
              80)