
(defpackage :simian.js-unparser
  (:nicknames :js)
  (:use :simian :simian.tests :cl)
  (:export #:comment-out
           #:make-indent))

(load (merge-pathnames "js.lisp" *load-truename*))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
