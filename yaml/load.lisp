

(defpackage :simian.yml-unparser
  (:nicknames :yml :yml-unparser :yaml :yaml-unparser)
  (:use :cl :software-simian :unparser)
  (:export 
    #:comment
    #:indent
    #:unparse
    #:unparse-pair-tree
    #:unparse-mapping))

(defpackage :simian.yaml-unparser.tests
  (:nicknames :yaml-tests :yml-tests)
  (:use :cl :tests :lisp-unit2 :yaml :unparser))

(load (merge-pathnames "yaml.lisp" *load-truename*))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
