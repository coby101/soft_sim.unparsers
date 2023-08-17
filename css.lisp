;;;===========================================================================
;;; file:   unparsers/css.lisp
;;; auth:   Coby Beck
;;; date:   2021-11-30
;;;
;;;---------------------------------------------------------------------------
;;;
;;;  - code related to unparsing CSS data
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :simian.css-unparser
  (:nicknames :css)
  (:use :simian :cl)
  (:export #:comment-out))

(in-package :css)

(defun comment-out (stream str &rest args)
  (let ((comment (apply #'format nil str args)))
    (format stream "~%/* ~a ~%*/~%" comment)))


;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:


