
;;;========================================================================================
;;;
;;;  - package definitions and source code loading for 
;;;    unparsing simian expressions into java script
;;;
;;;========================================================================================

(defpackage :simian.javascript-unparser
  (:nicknames :javascript-unparser :javascript :js)
  (:use :cl :software-simian :unparser)
  (:export 
    #:comment-out
    #:make-indent
    ))

(defpackage :simian.javascript-unparser.tests
  (:nicknames :javascript-tests)
  (:use :cl :tests :lisp-unit2 :javascript :unparser))

(load (merge-pathnames "javascript.lisp" *load-truename*))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
