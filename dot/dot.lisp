;;;===================================================================================
;;;
;;;  - methods and functions related to creating .DOT files for tools such as graphviz
;;;    to create entity relationship diagrams for simian generators
;;;   
;;;===================================================================================

(in-package :dot)

;; sytax defined here: https://graphviz.org/doc/info/lang.html
;;; Test your stuff here: http://magjac.com/graphviz-visual-editor/

(defmethod unparse ((obj t))
  (warn "no unparse method written for objects of type ~a" (type-of obj))
  (simian::unparse obj))

(defmethod unparse ((obj string))
  obj)

(defmethod unparse ((obj number))
  obj)
(defmethod unparse ((obj symbol))
  (format nil "~(~a~)" obj))

(defmethod unparse ((ent entity))
          (short-name ent)
          (mapcar #'unparse (relationships ent)))

(defmethod unparse-attribute-statement ((att string))
  att)
;; if being passed a list it is assumed to be in the form:
;;   (ID &rest attributes) where elements of attributes are
;;     either literal strings or (name value) pairs
(defmethod unparse-attribute-statement ((att list))
  (format nil "~a [~{~a~^; ~}]" (car att) (mapcar #'unparse-attribute (cdr att))))

(defmethod unparse-attribute ((att string))
  att)
(defmethod unparse-attribute ((att list))
  (if (html-string? (cadr att))
      (progn
        (unless (string-equal (car att) "label")
          (error "do not use html except as a label value"))
        (format nil "~a = ~a" (car att) (cadr att)))      
      (format nil "~a = ~s" (car att) (cadr att))))

(defmethod unparse-node ((node string))
  node)
;; if passed a list it should be of the form (node-id &rest attributes)
(defmethod unparse-node ((node list))
  ;; we could do some checking of legal attribute properties for nodes
  (unparse-attribute-statement node))

(defmethod unparse-node ((obj symbol))
  (let ((entity (find-entity obj)))
    (unparse-node entity)))

(defmethod unparse-node ((obj named-object))
  (unparse-node (list (name obj) (list "label" (short-name obj)))))

(defmethod unparse-edge ((edge string) &optional verbose)
  (declare (ignorable verbose))
  edge)

(defmethod unparse-graph (name nodes edges attributes)
  (format nil "
digraph ~s{
~{    ~a;~%~}
~{    ~a;~%~}
~{    ~a;~%~}
}"
          name
          (mapcar #'unparse-attribute-statement attributes)
          (mapcar #'unparse-node nodes)
          (mapcar #'unparse-edge edges)))

(defmethod unparse-edge ((rel binary-relationship) &optional verbose)
  (let* ((lhs-mult (multiplicity (lhs rel)))
         (rhs-mult (multiplicity (rhs rel)))
         (arrowhead (cond
                      ((equalp rhs-mult '(1 1)) "tee")
                      ((equalp rhs-mult '(0 1)) "none")
                      ((equalp rhs-mult '(0 *)) "crow") ;; should be distinct from '(1 *)
                      ((equalp rhs-mult '(1 *)) "crow")
                      (t (error "unhandled multiplicity: ~a" rhs-mult))))
         (arrowtail (cond
                      ((equalp lhs-mult '(1 1)) "tee")
                      ((equalp lhs-mult '(0 1)) "none")
                      ((equalp lhs-mult '(0 *)) "crow") ;; should be distinct from '(1 *)
                      ((equalp lhs-mult '(1 *)) "crow")
                      (t (error "unhandled multiplicity: ~a" lhs-mult))))
         (lhs-label (if verbose
                        (unparse-multiplicity lhs-mult)
                        (format nil "~a" lhs-mult)))
         (rhs-label (if verbose
                        (unparse-multiplicity rhs-mult)
                        (format nil "~a" rhs-mult)))
         (line-style (if (member 0 lhs-mult) "dashed" "solid")))
    (unparse-edge (list (name (entity (lhs rel))) "->" (name (entity (rhs rel)))
                        (list "fontsize" 11)
                        (list "taillabel" lhs-label)
                        (list "headlabel" rhs-label)
                        (list "style" line-style)
                        (list "arrowhead" arrowhead)
                        (list "arrowtail" arrowtail)))))

 ;; list should be (lhs operator rhs &rest attributes)
(defmethod unparse-edge ((edge list) &optional verbose)
  (declare (ignorable verbose))
  (format nil "~a ~a ~a~a"
          (first edge) (second edge) (third edge)
          (if (cdddr edge)
              (format nil "[~{~a; ~}]" (mapcar #'unparse-attribute (cdddr edge)))
              "")))

;; :complete is every entity with a relation of any sort
;; :basic is every relationship needed for CRUD
;;        operations (mandatory controls, required parents, repeated attributes)
;; :logical is for important relationships for application logic
;;        required or optional parents, aggregates, no lookups, no repeated attributes
(defun unparse-entity (entity &optional (extent :complete))
  (let* ((extended-relations (extended-relationships entity extent))
         (edges (mapcar #'(lambda(r) (list (entity (lhs r)) (entity (rhs r))))
                        extended-relations))
         (nodes (if edges
                    (remove-duplicates (apply #'append edges))
                    (list entity))))
    (format nil "~a"
            (unparse-graph (name entity)
                           (list* (list (name entity)
                                        (list "label" (short-name entity))
                                        (list "fontsize" 25))
                                  (remove entity nodes))
                           (mapcar #'unparse-edge extended-relations)
                           (list "bgcolor = \"transparent\"")))))

;; :complete is every viewable entity plus all their relations of any sort
;; :basic is every viewable entity
(defun unparse-view (view &optional (extent :basic))
  (let* ((entities (mapcar #'entity (aspects view)))
	 (extended-relations (if (eql extent :basic)
				 (find-edges entities)
				 (apply #'append (mapcar #'(lambda (e)
							     (extended-relationships e extent))
							 entities))))
         (edges (mapcar #'(lambda(r) (list (entity (lhs r)) (entity (rhs r))))
                        extended-relations))
         (nodes (if edges
                    (remove-duplicates (apply #'append edges))
                    entities)))
    (format nil "~a"
            (unparse-graph (name view)
                           nodes
                           (mapcar #'unparse-edge (remove-duplicates extended-relations))
                           (list "bgcolor = \"transparent\"")))))

(defun unparse-entity-cluster (name ents)
  (let* ((entities (mapcar #'(lambda (e) (if (typep e 'entity) e (find-entity e)))
                           ents))
         (edges (find-edges entities)))
    (format nil "~a"
            (unparse-graph name
                           entities
                           (remove-duplicates edges)
                           (list "bgcolor = \"transparent\"")))))

(defun find-edges (entities)
  (remove-if-not
   #'(lambda (r)
       (and (member (entity (lhs r)) entities)
            (member (entity (rhs r)) entities)))
   (apply #'append
          (mapcar #'relationships entities))))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End: 
