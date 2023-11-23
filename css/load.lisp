
(defpackage :simian.css-unparser
  (:nicknames :css)
  (:use :cl :software-simian :unparser)
  (:export #:comment-out))

(defpackage :simian.css-unparser.tests
  (:nicknames :css-tests)
  (:use :cl :tests :lisp-unit2 :css :unparser))

(load (merge-pathnames "css.lisp" *load-truename*))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
