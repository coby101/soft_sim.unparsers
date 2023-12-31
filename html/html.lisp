;;;=======================================================================================
;;;
;;;  - methods and functions related to writing HTML for simian generators
;;;
;;;=======================================================================================

(in-package :html)

(defun make-indent ()
  (make-string (* 2 *nesting-level*) :initial-element #\Space))

(defun open-tag (tag &rest properties)
  (format nil "~a<~a~{ ~a=\"~a\"~}>"
          (make-indent) (string-downcase tag) (downcase-keys properties)))

(defun close-tag (tag)
  (format nil "~a</~a>" (make-indent) (string-downcase tag)))

(defun tag (tag text &rest attributes)
  (format nil "~a~a~a" (apply #'open-tag tag attributes) text (close-tag tag)))

(defmethod ltag (ordered? (content string) &rest attributes)
  (let ((tag (if ordered? "ol" "ul")))
    (apply #'tag tag content attributes)))

(defmethod ltag (ordered? (content list) &rest attributes)
  (let ((tag (if ordered? "ol" "ul")))
    (format nil "~a~a~%~a~a"
            (apply #'open-tag tag attributes)
            (with-nesting
                (let ((fmt-str (format nil "~~{~%~a<li>~~a</li>~~}" (make-indent))))
                  (format nil fmt-str content)))
            (make-indent) (close-tag tag))))

(defmethod div ((content string) &rest attributes)
  (apply #'tag "div" content attributes))

(defmethod div ((content list) &rest attributes)
  (format nil "~a~a~%~a</div>"
          (apply #'open-tag "div" attributes)
          (with-nesting
              (let ((fmt-str (format nil "~~{~%~a~~a~~}" (make-indent))))
                (format nil fmt-str content)))
          (make-indent)))

(defmethod theading ((text string) &rest attributes)
  (apply #'tag "th" text attributes))

(defmethod theading ((att attribute) &rest attributes)
  (apply #'tag "th" (short-name att) attributes))

(defun tcell (content &rest attributes)
  (apply #'tag "td" content attributes))

(defmethod trow ((content string) &rest attributes)
  (apply #'tag "tr" content attributes))

(defmethod tbody ((content string) &rest attributes)
  (apply #'tag "tbody" content attributes))

(defmethod tbody ((rows list) &rest attributes)
  (apply #'tag "tbody"
         (format nil "~%~{~a~^~%~}" (mapcar #'trow rows))
         attributes))

(defmethod thead ((content string) &rest attributes)
  (apply #'tag "thead" content attributes))

(defmethod thead ((col-headings list) &rest attributes)
  (let ((fmt-str (with-nesting
                     (format nil "~~{~%~a<th>~~a</th>~~}~%~a"
                             (with-nesting (make-indent)) (make-indent)))))
    (apply #'tag "thead"
           (format nil "~%~a"
                   (with-nesting
                       (tag "tr" (format nil fmt-str col-headings))))
           attributes)))

(defun table (head body &rest attributes)
  (format nil "~a~%~a~&~a~&~a</table>"
          (apply #'open-tag "table" attributes)
          (if head (with-nesting (thead head)) "")
          (if body (with-nesting (tbody body)) "")
          (make-indent)))

(defmethod trow ((cells list) &rest attributes)
  (let ((fmt-str (format nil "~~{~%~a<td>~~a</td>~~}~%~a"
                         (with-nesting (make-indent)) (make-indent))))
    (apply #'tag "tr" (format nil fmt-str cells) attributes)))
  
(defun heading (level text &rest attributes)
  (let ((*nesting-level* 0))
    (apply #'tag (format nil "h~a" level) text attributes)))
  
(defun image (location &rest attributes)
  (apply #'open-tag "img" :src location attributes))

(defun link (text link &rest attributes)
  (let ((*nesting-level* 0))
    (apply #'tag "a" text (append (list :href link) attributes))))

(defun button (text &rest attributes &key (type "button") &allow-other-keys)
  (let ((*nesting-level* 0))
    (apply #'tag "button" text (append (list :type type) attributes))))

(defun p (content &rest attributes)
  (let ((*nesting-level* 0))
    (apply #'tag "p" (or content "") attributes)))

;;; just a place holder, this needs to escape all manner of crap
(defmethod unparse ((obj string) (language (eql :html)))
  (warn "we have not really written this one yet...")
  (format nil "~a" obj))
(defmethod unparse ((obj number) (language (eql :html))) obj)
(defmethod unparse ((obj symbol) (language (eql :html))) (format nil "~a" (string-downcase (symbol-name obj))))

(defmethod unparse ((obj list) (language (eql :html))) (mapcar #'(lambda (item) (unparse item language)) obj))
(defmethod unparse ((ent entity) (language (eql :html))) (long-name ent))
(defmethod unparse ((att attribute) (language (eql :html))) (long-name att))

(defmethod unparse ((panel flat-listing) (language (eql :html)))
  (table nil (list (mapcar #'(lambda (item) (unparse item language)) (panel-items panel)))))

(defmethod unparse ((panel view-panel) (language (eql :html)))
  (table nil (mapcar #'(lambda(row)
                         (mapcar #'(lambda (item) (unparse item language)) row))
                     (panel-items panel))))

;; this looks wrong, wait til refactor is complete. It also clobbers a definition above
(defmethod unparse ((list list) (language (eql :html)))
  (if (field-reference-expression? list)
      (format nil "~a ~a" (name (car list)) (name (cadr list)))
      (mapcar #'(lambda (item) (unparse item language)) list)))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
