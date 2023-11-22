(in-package :unparser)

(defmethod unparse-list ((list list) (language (eql :simian)))
  ;; we should only be here if we have a list without an operator in first position
  ;; which means we do not expect anything besides atom elements or lists of
  ;; atomic elements (ie no nested expressions)
  (format nil "(~{~a~^ ~})" (mapcar #'(lambda (obj) (unparse obj language)) list)))

(defmethod unparse-array ((obj list) (language (eql :simian))) (unparse-list obj language))
(defmethod unparse ((obj list) (language (eql :simian))) (unparse-list obj language))


(defmethod unparse ((obj string)  (language (eql :simian)))  (format nil "~s" obj))
(defmethod unparse ((obj float)   (language (eql :simian)))   (format nil "~f" obj))
(defmethod unparse ((obj integer) (language (eql :simian))) (format nil "~d" obj))
(defmethod unparse ((obj symbol)  (language (eql :simian)))  (format nil "~(~a~)" (symbol-name obj)))

(defmethod unparse-expression ((att attribute) (language (eql :simian)) &optional args)
  (when args
    (error "unparse-expression called on an attribute should not have any additional argument (~a)"
           args))
  (list (id (my-entity att)) (id att)))
