
(defpackage simian.html-unparser
  (:nicknames :html)
  (:use :cl :software-simian :unparser)
  (:export #:comment
           #:unparse
           #:make-indent
           #:ltag
           #:div
           #:theading
           #:table
           #:tcell
           #:tbody
           #:thead
           #:trow
           #:heading
           #:image
           #:link
           #:button
           #:tag
           #:open-tag
           #:close-tag
           #:p
           ))

(defpackage :simian.html-unparser.tests
  (:nicknames :html-tests)
  (:use :cl :tests :lisp-unit2 :html :unparser))

(load (merge-pathnames "html.lisp" *load-truename*))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
