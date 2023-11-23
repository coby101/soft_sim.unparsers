
(defpackage simian.english-unparser
  (:nicknames :english :english-unparser)
  (:use :cl :software-simian :unparser)
  (:export #:unparse
           #:unparse-expression
           #:unparse-multiplicity
           #:with-article
           #:designation-with-article))

(defpackage :simian.english-unparser.tests
  (:nicknames :english-tests)
  (:use :cl :tests :lisp-unit2 :english :unparser))

(load (merge-pathnames "english.lisp" *load-truename*))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
