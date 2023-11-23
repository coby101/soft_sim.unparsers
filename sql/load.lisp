
(defpackage :simian.sql-unparser
  (:nicknames :sql-unparser :sql)
  (:use :cl :software-simian :unparser)
  (:export
   #:comment
   #:unparse-table-definition
   #:unparse-attribute-references
   #:format-expression
   #:indent))

(defpackage :simian.sql-unparser.tests
  (:nicknames :sql-tests)
  (:use :cl :tests :lisp-unit2 :sql :unparser))

(load (merge-pathnames "sql.lisp" *load-truename*))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
