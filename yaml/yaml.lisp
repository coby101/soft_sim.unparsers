;;;=========================================================================================
;;;
;;;   methods and functions to generate yml data files for simian generators
;;;
;;;=========================================================================================

(in-package :yml)

(defun indent (&optional (level *nesting-level*))
  (make-string (* 2 level) :initial-element #\Space))

;; careful here, comments are not defined in yaml, please revisit
(defun comment (str &rest args)
  (let ((comment (apply #'format nil str args)))
    (format nil
            (strcat "~a# "
                    (replace-all comment
                                 (format nil "~%")
                                 (format nil "~%~a# " (indent)))
                    (format nil "~%"))
            (indent))))

(defun pair? (item)
  (and (listp item)
       (= 2 (length item))
       (atom (car item))))

(defun unparse-mapping (pair)
  (unless (pair? pair)
    (error "inappropriate data"))
  (format nil "~a~(~a~): ~a" (indent) (car pair) (unparse (cadr pair))))

(defmethod unparse ((obj string) (language (eql :yaml))) (format nil "~s" obj))
(defmethod unparse ((obj symbol) (language (eql :yaml))) (format nil "~(~a~)" (symbol-name obj)))

(defun unparse-pair-tree (tree)
  (cond
    ((and (pair? tree) (atom (cadr tree)))
     (unparse-mapping tree))
    ((pair? tree)
     (format nil "~a~(~a~):~%~a" (indent) (car tree)
       ;; maybe written before with-nesting macro?
	     (let ((*nesting-level* (1+ *nesting-level*)))
	       (unparse-pair-tree (cadr tree)))))
    ((listp tree)
     (let ((fmt-str (format nil "~~{~a~~a~~^~~%~~}" "")))
       (format nil fmt-str (mapcar #'unparse-pair-tree tree))))))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
