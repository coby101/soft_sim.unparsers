
(defpackage :simian.sql-unparser
  (:nicknames :sql-unparser :sql)
  (:use :simian :simian.tests :cl)
  (:export #:comment
           #:unparse
           #:unparse-table-definition
           #:unparse-attribute-references
           #:format-expression
           #:unparse-expression
           #:indent))

(load (merge-pathnames "sql.lisp" *load-truename*))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
