;;;===========================================================================================
;;;
;;;  - methods and functions related to unparsing java script for simian:*application* objects
;;;
;;;===========================================================================================

(in-package :js)

(defun make-indent ()
  (make-string (* 2 *nesting-level*) :initial-element #\Space))

(defun comment-out (stream str &rest args)
  (let ((comment (apply #'format nil str args)))
    (format stream
            (strcat "~a// "
                    (replace-all comment
                                 (format nil "~%")
                                 (format nil "~%~a// " (make-indent)))
                    (format nil "~%"))
            (make-indent))))


;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
