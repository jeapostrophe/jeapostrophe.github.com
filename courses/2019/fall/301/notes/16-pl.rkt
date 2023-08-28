#lang parenlog

(! (A-could-be (succ zero)))
(! (A-could-be (succ (succ zero))))
(! (A-could-be (succ (succ (succ zero)))))

(! (B-could-be zero))
(! (B-could-be (succ zero)))
(! (B-could-be (succ (succ zero))))

(:- (add zero B B))
(:- (add (succ X) B (succ C))
    (add X B C))

(:- (the-problem A B)
    (A-could-be A)
    (B-could-be B)
    (add A B (succ (succ (succ zero)))))

(? (the-problem A B))
