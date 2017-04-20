(module |25| racket/base
  (#%module-begin
   (module configure-runtime '#%kernel
     (#%module-begin (#%require racket/runtime-config) (#%app configure '#f)))
   (#%require racket/list)
   (#%require racket/match)
   (define-values
    (read-until-space)
    (lambda (ip)
      (let-values (((temp1) (#%app peek-char ip)))
        (let-values (((fail2)
                      (lambda ()
                        (#%app
                         match:error
                         temp1
                         (#%app syntax-srclocs (quote-syntax srcloc))
                         'match))))
          (let-values (((f3)
                        (lambda ()
                          (let-values ()
                            (let-values ()
                              (let-values ()
                                (#%app
                                 cons
                                 (#%app read-char ip)
                                 (#%app read-until-space ip))))))))
            (let-values (((esc*6) (lambda () (#%app values '#f))))
              (let-values (((success?7)
                            (if (#%app equal? temp1 '#\()
                              (let-values ()
                                (let-values ()
                                  (let-values () (#%app values '#t))))
                              (if (#%app equal? temp1 '#\space)
                                (let-values ()
                                  (let-values ()
                                    (let-values () (#%app values '#t))))
                                (if (#%app equal? temp1 '#\))
                                  (let-values ()
                                    (let-values ()
                                      (let-values () (#%app values '#t))))
                                  (let-values () (#%app esc*6)))))))
                (if success?7
                  (let-values () (let-values () (let-values () empty)))
                  (#%app f3)))))))))
   (define-values
    (se-read)
    (lambda (ip)
      (let-values (((temp18) (#%app peek-char ip)))
        (let-values (((fail19)
                      (lambda ()
                        (#%app
                         match:error
                         temp18
                         (#%app syntax-srclocs (quote-syntax srcloc))
                         'match))))
          (let-values (((f20)
                        (lambda ()
                          (let-values ()
                            (let-values ()
                              (let-values (((some-char) temp18))
                                (let-values ()
                                  (let-values (((list-of-cs)
                                                (#%app read-until-space ip)))
                                    (let-values (((str)
                                                  (#%app
                                                   list->string
                                                   list-of-cs)))
                                      (if (#%app
                                           andmap
                                           char-numeric?
                                           list-of-cs)
                                        (#%app string->number str)
                                        (#%app string->symbol str)))))))))))
            (if (#%app equal? temp18 '#\()
              (let-values ()
                (let-values ()
                  (let-values ()
                    (let-values ()
                      (let-values ((()
                                    (begin
                                      (#%app read-char ip)
                                      (#%app values))))
                        (letrec-values (((read-until-rparen)
                                         (lambda ()
                                           (let-values (((temp28)
                                                         (#%app peek-char ip)))
                                             (let-values (((fail29)
                                                           (lambda ()
                                                             (#%app
                                                              match:error
                                                              temp28
                                                              (#%app
                                                               syntax-srclocs
                                                               (quote-syntax
                                                                srcloc))
                                                              'match))))
                                               (let-values (((f30)
                                                             (lambda ()
                                                               (let-values ()
                                                                 (let-values ()
                                                                   (let-values ()
                                                                     (#%app
                                                                      cons
                                                                      (#%app
                                                                       se-read
                                                                       ip)
                                                                      (#%app
                                                                       read-until-rparen))))))))
                                                 (if (#%app equal? temp28 '#\))
                                                   (let-values ()
                                                     (let-values ()
                                                       (let-values ()
                                                         (let-values ()
                                                           (#%app read-char ip)
                                                           empty))))
                                                   (if (#%app
                                                        equal?
                                                        temp28
                                                        '#\space)
                                                     (let-values ()
                                                       (let-values ()
                                                         (let-values ()
                                                           (let-values ()
                                                             (#%app
                                                              read-char
                                                              ip)
                                                             (#%app
                                                              read-until-rparen)))))
                                                     (let-values ()
                                                       (#%app f30))))))))))
                          (#%app read-until-rparen)))))))
              (if (#%app equal? temp18 '#\))
                (let-values ()
                  (let-values ()
                    (let-values ()
                      (let-values ()
                        (#%app error 'parse '"Unbalanced paren")))))
                (if (#%app equal? temp18 '#\space)
                  (let-values ()
                    (let-values ()
                      (let-values ()
                        (let-values () (#%app read-char) (#%app se-read ip)))))
                  (let-values () (#%app f20))))))))))
   (define-values
    (c-read-atom)
    (lambda (ip)
      (let-values (((temp40) (#%app peek-char ip)))
        (let-values (((fail41)
                      (lambda ()
                        (#%app
                         match:error
                         temp40
                         (#%app syntax-srclocs (quote-syntax srcloc))
                         'match))))
          (let-values (((f42)
                        (lambda ()
                          (let-values ()
                            (let-values () (let-values () '#f))))))
            (let-values (((f44)
                          (lambda ()
                            (if (#%app char-numeric? temp40)
                              (let-values ()
                                (let-values ()
                                  (let-values ()
                                    (let-values ()
                                      (#%app
                                       -
                                       (#%app
                                        char->integer
                                        (#%app read-char ip))
                                       (#%app char->integer '#\0))))))
                              (let-values () (#%app f42))))))
              (if (#%app char-alphabetic? temp40)
                (let-values ()
                  (let-values ()
                    (let-values ()
                      (let-values ()
                        (#%app
                         string->symbol
                         (#%app string (#%app read-char ip)))))))
                (let-values () (#%app f44)))))))))
   (define-values
    (c-read)
    (lambda (ip)
      (let-values (((a) (#%app c-read-atom ip)))
        (let-values (((temp53) (#%app peek-char ip)))
          (let-values (((fail54)
                        (lambda ()
                          (#%app
                           match:error
                           temp53
                           (#%app syntax-srclocs (quote-syntax srcloc))
                           'match))))
            (let-values (((f55)
                          (lambda ()
                            (if (#%app eof-object? temp53)
                              (let-values ()
                                (let-values ()
                                  (let-values () (let-values () a))))
                              (let-values () (#%app fail54))))))
              (if (#%app equal? temp53 '#\+)
                (let-values ()
                  (let-values ()
                    (let-values ()
                      (let-values ()
                        (#%app read-char ip)
                        (#%app list '+ a (#%app c-read ip))))))
                (if (#%app equal? temp53 '#\-)
                  (let-values ()
                    (let-values ()
                      (let-values ()
                        (let-values ()
                          (#%app read-char ip)
                          (#%app list '- a (#%app c-read ip))))))
                  (let-values () (#%app f55))))))))))
   (define-values
    (parse-id)
    (lambda (ip)
      (let-values (((c) (#%app peek-char ip)))
        (if (if (#%app char? c) (#%app char-alphabetic? c) '#f)
          (#%app string->symbol (#%app string (#%app read-char ip)))
          (#%app fail)))))
   (define-values
    (parse-num)
    (lambda (ip)
      (let-values (((c) (#%app peek-char ip)))
        (if (if (#%app char? c) (#%app char-numeric? c) '#f)
          (#%app string->number (#%app string (#%app read-char ip)))
          (#%app fail)))))
   (define-values (parse-add) (lambda (ip) (#%app fail)))
   (define-values (parse-sub) (lambda (ip) (#%app fail)))
   (define-values
    (parse-or)
    (lambda (ip opts)
      (if (#%app empty? opts)
        (let-values () (#%app fail))
        (let-values ()
          (let-values (((opt) (#%app first opts)))
            (let-values (((old-fail) fail))
              (let-values (((this-pos) (#%app file-position ip)))
                (let-values (((or-part)
                              (#%app
                               call/cc
                               (lambda (this-fail)
                                 (set! fail
                                   (lambda ()
                                     (set! fail old-fail)
                                     (#%app file-position ip this-pos)
                                     (#%app this-fail '#f)))
                                 (#%app opt ip)))))
                  (if or-part
                    or-part
                    (#%app parse-or ip (#%app rest opts)))))))))))
   (define-values (fail) (lambda () (#%app error 'parse '"Couldn't")))
   (define-values
    (c-read2)
    (lambda (ip)
      (let-values (((ans)
                    (#%app
                     parse-or
                     ip
                     (#%app list parse-id parse-num parse-add parse-sub))))
        (if (#%app eof-object? (#%app peek-char ip))
          (#%app void)
          (let-values () (#%app fail)))
        ans)))
   (#%require parser-tools/yacc)
   (#%require parser-tools/lex)
   (#%require (prefix : parser-tools/lex-sre))
   (define-syntaxes
    (c-val-tokens)
    (#%app make-terminals-def (quote-syntax (NUM VAR))))
   (define-values (token-NUM) (lambda (x) (#%app make-token 'NUM x)))
   (define-values (token-VAR) (lambda (x) (#%app make-token 'VAR x)))
   (define-syntaxes
    (c-op-tokens)
    (#%app make-e-terminals-def (quote-syntax (ADD SUB EOF))))
   (define-values (token-ADD) (lambda () 'ADD))
   (define-values (token-SUB) (lambda () 'SUB))
   (define-values (token-EOF) (lambda () 'EOF))
   (define-values
    (c-lexer)
    (let-values (((|25184|)
                  (lambda (start-pos-p end-pos-p lexeme-p input-port-p)
                    (let-values () (let-values () 'ADD))))
                 ((|25185|)
                  (lambda (start-pos-p end-pos-p lexeme-p input-port-p)
                    (let-values () (let-values () 'SUB))))
                 ((|25186|)
                  (lambda (start-pos-p end-pos-p lexeme-p input-port-p)
                    (let-values ()
                      (let-values ()
                        (#%app token-NUM (#%app string->number lexeme-p))))))
                 ((|25187|)
                  (lambda (start-pos-p end-pos-p lexeme-p input-port-p)
                    (let-values ()
                      (let-values ()
                        (#%app token-VAR (#%app string->symbol lexeme-p)))))))
      (let-values (((proc)
                    (#%app
                     lexer-body
                     '0
                     '#(#(#(43 43 4) #(45 45 3) #(48 57 2) #(97 122 1))
                        #f
                        #f
                        #f
                        #f)
                     (#%app vector '#f |25187| |25186| |25185| |25184|)
                     '#(#f #t #t #t #t)
                     (lambda (start-pos-p end-pos-p lexeme-p input-port-p)
                       (let-values () (let-values () (#%app void))))
                     '#f
                     (lambda (start-pos-p end-pos-p lexeme-p input-port-p)
                       (let-values () (let-values () '#f)))
                     (lambda (start-pos-p end-pos-p lexeme-p input-port-p)
                       (let-values () (let-values () 'EOF))))))
        (lambda (port) (#%app proc port)))))
   (define-values
    (c-parser)
    (begin
      (if '#f
        (let-values ()
          (let-values (((cexp) void)
                       ((error) void)
                       ((NUM) void)
                       ((VAR) void)
                       ((ADD) void)
                       ((SUB) void)
                       ((EOF) void))
            (#%app
             void
             NUM
             VAR
             cexp
             ADD
             cexp
             cexp
             SUB
             cexp
             '#f
             '#f
             cexp
             EOF
             ADD
             SUB)))
        (#%app void))
      (#%app
       parser-body
       '#f
       (lambda (a b c) (#%app error 'c-parse '"Can't: ~v ~v ~v" a b c))
       '(cexp)
       '(EOF)
       '#(#hasheq((VAR . 4) (NUM . 3) (cexp . -3) (|25313| . -2))
          #hasheq()
          #hasheq((SUB . 6) (ADD . 5) (EOF . accept))
          #hasheq((SUB . #(2 cexp 1)) (ADD . #(2 cexp 1)) (EOF . #(2 cexp 1)))
          #hasheq((SUB . #(3 cexp 1)) (ADD . #(3 cexp 1)) (EOF . #(3 cexp 1)))
          #hasheq((VAR . 4) (NUM . 3) (cexp . -9))
          #hasheq((VAR . 4) (NUM . 3) (cexp . -10))
          #hasheq()
          #hasheq((SUB . #(4 cexp 3)) (ADD . #(4 cexp 3)) (EOF . #(4 cexp 3)))
          #hasheq((SUB . #(5 cexp 3)) (ADD . #(5 cexp 3)) (EOF . #(5 cexp 3))))
       '#hasheq((EOF . #t)
                (VAR . #t)
                (NUM . #t)
                (ADD . #t)
                (error . #t)
                (SUB . #t))
       (#%app
        vector
        (lambda (x) x)
        (lambda (x) x)
        (lambda ($1) $1)
        (lambda ($1) $1)
        (lambda ($1 |25314| $3) (#%app list '+ $1 $3))
        (lambda ($1 |25315| $3) (#%app list '- $1 $3)))
       '#f)))
   (define-values
    (c-read3)
    (lambda (ip) (#%app c-parser (lambda () (#%app c-lexer ip)))))
   (module*
    test
    #f
    (#%module-begin
     (module configure-runtime '#%kernel
       (#%module-begin
        (#%require racket/runtime-config)
        (#%app configure '#f)))
     (#%require chk)
     (define-values
      (t*)
      (lambda (*)
        (lambda (actual-str expected-se)
          (let-values ()
            (#%app
             check
             D-equal?
             (let-values (((stx-id)
                           (#%app
                            datum->syntax
                            (quote-syntax ctx)
                            (#%app
                             cons
                             (#%app
                              datum->syntax
                              (quote-syntax ctx)
                              '*
                              '#(#<path:/Users/jay/Dev/scm/github.jeapostrophe/work/courses/2017/spring/406/notes/25.rkt>
                                 7
                                 10
                                 145
                                 1))
                             (#%app
                              cons
                              (#%app
                               datum->syntax
                               (quote-syntax ctx)
                               (#%app
                                cons
                                (#%app
                                 datum->syntax
                                 (quote-syntax ctx)
                                 'open-input-string
                                 '#(#<path:/Users/jay/Dev/scm/github.jeapostrophe/work/courses/2017/spring/406/notes/25.rkt>
                                    7
                                    13
                                    148
                                    17))
                                (#%app
                                 cons
                                 (#%app
                                  datum->syntax
                                  (quote-syntax ctx)
                                  'actual-str
                                  '#(#<path:/Users/jay/Dev/scm/github.jeapostrophe/work/courses/2017/spring/406/notes/25.rkt>
                                     7
                                     31
                                     166
                                     10))
                                 '()))
                               '#(#<path:/Users/jay/Dev/scm/github.jeapostrophe/work/courses/2017/spring/406/notes/25.rkt>
                                  7
                                  12
                                  147
                                  30))
                              '()))
                            '#(#<path:/Users/jay/Dev/scm/github.jeapostrophe/work/courses/2017/spring/406/notes/25.rkt>
                               7
                               9
                               144
                               34))))
               (let-values (((with-handlers-predicate1) exn?)
                            ((with-handlers-handler2)
                             (lambda (x) (#%app res:exn2 stx-id x))))
                 (let-values (((bpz)
                               (#%app
                                continuation-mark-set-first
                                '#f
                                break-enabled-key)))
                   (#%app
                    call-handled-body
                    bpz
                    (lambda (e)
                      (#%app
                       select-handler/no-breaks
                       e
                       bpz
                       (#%app
                        list
                        (#%app
                         cons
                         with-handlers-predicate1
                         with-handlers-handler2))))
                    (lambda ()
                      (#%app
                       call-with-values
                       (lambda ()
                         (#%app * (#%app open-input-string actual-str)))
                       (lambda vs (#%app res:values3 stx-id vs))))))))
             (let-values (((stx-id)
                           (#%app
                            datum->syntax
                            (quote-syntax ctx)
                            'expected-se
                            '#(#<path:/Users/jay/Dev/scm/github.jeapostrophe/work/courses/2017/spring/406/notes/25.rkt>
                               8
                               9
                               188
                               11))))
               (let-values (((with-handlers-predicate3) exn?)
                            ((with-handlers-handler4)
                             (lambda (x) (#%app res:exn2 stx-id x))))
                 (let-values (((bpz)
                               (#%app
                                continuation-mark-set-first
                                '#f
                                break-enabled-key)))
                   (#%app
                    call-handled-body
                    bpz
                    (lambda (e)
                      (#%app
                       select-handler/no-breaks
                       e
                       bpz
                       (#%app
                        list
                        (#%app
                         cons
                         with-handlers-predicate3
                         with-handlers-handler4))))
                    (lambda ()
                      (#%app
                       call-with-values
                       (lambda () expected-se)
                       (lambda vs (#%app res:values3 stx-id vs)))))))))))))
     (define-values (tse) (#%app t* se-read))
     (#%app
      call-with-values
      (lambda () (#%app tse '"(()(()()())())" '(() (() () ()) ())))
      print-values)
     (#%app
      call-with-values
      (lambda () (#%app tse '"(let ((x 17)) (+ x x))" '(let ((x 17)) (+ x x))))
      print-values)
     (define-values (tc) (#%app t* c-read))
     (#%app call-with-values (lambda () (#%app tc '"x" 'x)) print-values)
     (#%app call-with-values (lambda () (#%app tc '"7" '7)) print-values)
     (#%app
      call-with-values
      (lambda () (#%app tc '"4+4" '(+ 4 4)))
      print-values)
     (#%app
      call-with-values
      (lambda () (#%app tc '"4-4" '(- 4 4)))
      print-values)
     (define-values (tc3) (#%app t* c-read3))
     (#%app call-with-values (lambda () (#%app tc3 '"x" 'x)) print-values)
     (#%app call-with-values (lambda () (#%app tc3 '"7" '7)) print-values)
     (#%app
      call-with-values
      (lambda () (#%app tc3 '"4+4" '(+ 4 4)))
      print-values)
     (#%app
      call-with-values
      (lambda () (#%app tc3 '"4-4" '(- 4 4)))
      print-values)
     (#%app
      call-with-values
      (lambda () (#%app tc3 '"4+4-4" '(- (+ 4 4) 4)))
      print-values)
     (#%app
      call-with-values
      (lambda () (#%app tc3 '"x+y-z" '(- (+ x y) z)))
      print-values)
     (#%app
      call-with-values
      (lambda () (#%app tc3 '"4-4+4" '(+ (- 4 4) 4)))
      print-values)))))
