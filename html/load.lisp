
(defpackage simian.html-unparser
  (:nicknames :html)
  (:use :simian :simian.tests :cl)
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

(load (merge-pathnames "html.lisp" *load-truename*))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
