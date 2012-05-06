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

(define-syntax (download-link stx)
  (quasisyntax/loc stx @verbatim[(format "[Download](/downloads/code/~a)" #,(path->string (syntax-source-file-name stx)))]))

(provide (all-defined-out))
