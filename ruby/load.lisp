

(defpackage :simian.ruby-unparser
  (:nicknames :ruby :ruby-unparser)
  (:use :cl :software-simian :unparser)
  (:export #:comment
           #:comment-with-warning
           #:comment-out
           #:indent-block
           #:unparse-method
           #:unparse-datatype
           #:unparse-formatting
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
           #:*include-rails*
           #:*nesting-level*))

(load (merge-pathnames "ruby.lisp" *load-truename*))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
