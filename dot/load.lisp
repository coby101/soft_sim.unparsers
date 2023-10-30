
(defpackage :simian.dot-unparser
  (:nicknames :dot)
  (:use :simian :simian.tests :cl)
  (:export #:unparse-entity
           #:unparse-entity-cluster
           #:unparse-view
           #:indent
           #:unparse-graph))

(load (merge-pathnames "dot.lisp" *load-truename*))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
