#lang at-exp racket/base
(require scribble/base
         (for-syntax racket/base
                     unstable/syntax)
         scribble/lp)

(define yaml verbatim)
(define more
  @verbatim{
<!-- more -->
}
  )

(define-syntax-rule (chunky . e)
  (begin @verbatim{{% codeblock lang:scheme %}} (chunk . e) @verbatim{{% endcodeblock %}}))

(define-syntax-rule (verbatim:codeblock e ...)
  (verbatim "{% codeblock %}\n"
            e ...
            "{% endcodeblock %}\n"))

;; (define-syntax (download-link stx)
;;   (quasisyntax/loc stx @verbatim[(format "[Download](/downloads/code/~a)" #,(path->string (syntax-source-file-name stx)))]))

(define-syntax (download-link stx)
  (syntax/loc stx (void)))

(provide (all-defined-out))
