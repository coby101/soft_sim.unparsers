

(defpackage :simian.ruby-unparser
  (:nicknames :ruby :ruby-unparser)
  (:use :simian :cl)
  (:export #:comment
           #:comment-with-warning
           #:comment-out
           #:indent-block
           #:unparse
           #:unparse-method
           #:unparse-expression
           #:negate-expression
           #:unparse-datatype
           #:unparse-formatting
           #:unparse-array
           #:unparse-hash
           #:unparse-hash-key
           #:unparse-range
           #:unparse-lambda
           #:is-range?
           #:unparse-data
           #:unparse-if-statement
           #:make-indent
           #:make-legal-name
           #:model-name
           #:schema-name
           #:*ruby-constants*
           #:*include-rails*))

(load (merge-pathnames "ruby.lisp" *load-truename*))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
