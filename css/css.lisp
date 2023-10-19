;;;====================================================================================
;;;
;;;  - methods and functions related to unparsing CSS from simian generators
;;;
;;;====================================================================================

(in-package :css)

(defun comment-out (stream str &rest args)
  (let ((comment (apply #'format nil str args)))
    (format stream "~%/* ~a ~%*/~%" comment)))


;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
