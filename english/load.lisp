
(defpackage simian.english-unparser
  (:nicknames :english :english-unparser)
  (:use :simian :cl)
  (:export #:unparse
           #:unparse-expression
           #:unparse-multiplicity
           #:with-article
           #:designation-with-article))

(load (merge-pathnames "english.lisp" *load-truename*))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
