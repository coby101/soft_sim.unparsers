
(defpackage :simian.css-unparser
  (:nicknames :css)
  (:use :simian :cl)
  (:export #:comment-out))

(load (merge-pathnames "css.lisp" *load-truename*))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End: