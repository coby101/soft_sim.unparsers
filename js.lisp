;;;===========================================================================
;;; file:   js.lisp
;;; auth:   Coby Beck
;;; date:   2021-11-15
;;;
;;;---------------------------------------------------------------------------
;;;
;;;  - code related to unparsing java script for simian:*application* objects
;;;
;;;===========================================================================

(defpackage :simian.js-unparser
  (:nicknames :js)
  (:use :simian :cl)
  (:export #:comment-out
           #:make-indent))

(in-package :js)

(defun comment-out (stream str &rest args)
  (let ((comment (apply #'format nil str args)))
    (format stream
            (strcat "~a// "
                    (replace-all comment
                                 (format nil "~%")
                                 (format nil "~%~a// " (make-indent)))
                    (format nil "~%"))
            (make-indent))))

(defun make-indent ()
  (make-string (* 2 *nesting-level*) :initial-element #\Space))


;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
