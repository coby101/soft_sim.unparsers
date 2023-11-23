;;;========================================================================================
;;;
;;;  - package definitions and source loading for unparsing simian objects into simian
;;;    specification forms
;;;
;;;========================================================================================

(defpackage :simian.simian-unparser.tests
  (:nicknames :simian-tests)
  (:use :cl :tests :lisp-unit2 :unparser))

(load (merge-pathnames "simian.lisp" *load-truename*))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
