;;;========================================================================================
;;;
;;;  - package definitions and source code loading for unparsing simian expressions into ruby
;;;
;;;========================================================================================

(defpackage :simian.ruby-unparser
  (:nicknames :ruby :ruby-unparser)
  (:use :cl :software-simian :unparser)
  (:export
   #:comment
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
   ))

(defpackage :simian.ruby-unparser.tests
  (:nicknames :ruby-tests)
  (:use :cl :tests :lisp-unit2 :ruby :unparser))

(load (merge-pathnames "ruby.lisp" *load-truename*))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
