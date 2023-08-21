;;;===========================================================================
;;; file:   yaml.lisp
;;; auth:   Coby Beck
;;; date:   2022-01-21
;;;
;;;---------------------------------------------------------------------------
;;;   code to generate yml data files
;;;---------------------------------------------------------------------------  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defpackage :simian.yml-unparser
  (:nicknames :yml :yml-unparser)
  (:use :cl :simian)
  (:export #:comment
           #:indent
           #:unparse
           #:unparse-pair-tree
           #:unparse-mapping))

(in-package :yml)

(defun comment (str &rest args)
  (let ((comment (apply #'format nil str args)))
    (format nil
            (strcat "~a# "
                    (replace-all comment
                                 (format nil "~%")
                                 (format nil "~%~a# " (indent)))
                    (format nil "~%"))
            (indent))))

(defun indent (&optional (level *nesting-level*))
  (make-string (* 2 level) :initial-element #\Space))

(defun pair? (item)
  (and (listp item)
       (= 2 (length item))
       (atom (car item))))

(defun unparse-mapping (pair)
  (unless (pair? pair)
    (error "inappropriate data"))
  (format nil "~a~(~a~): ~a" (indent) (car pair) (unparse (cadr pair))))

(defmethod unparse ((obj string))
  (format nil "~s" obj))

(defmethod unparse ((obj symbol))
  (format nil "~(~a~)" (symbol-name obj)))

(defun unparse-pair-tree (tree)
  (cond
    ((and (pair? tree) (atom (cadr tree)))
     (unparse-mapping tree))
    ((pair? tree)
     (format nil "~a~(~a~):~%~a" (indent) (car tree)
	     (let ((*nesting-level* (1+ *nesting-level*)))
	       (unparse-pair-tree (cadr tree)))))
    ((listp tree)
     (let ((fmt-str (format nil "~~{~a~~a~~^~~%~~}" "")))
       (format nil fmt-str (mapcar #'unparse-pair-tree tree))))))


#|
tests: 
|#

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
