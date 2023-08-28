;;;===========================================================================
;;; file:   sql.lisp
;;; auth:   Coby Beck
;;; date:   2021-01-12
;;;
;;;---------------------------------------------------------------------------
;;;
;;;  - code related to writing SQL for simian:*application* objects
;;;   
;;;===========================================================================

(defpackage :simian.sql-unparser
  (:nicknames :sql-unparser :sql)
  (:use :simian :cl)
  (:export #:comment
           #:unparse
           #:unparse-table-definition
           #:unparse-attribute-references
           #:format-expression
           #:unparse-expression
           #:indent))

(in-package :sql)

(defparameter *pretty-print* t)

#|
(defmethod unparse-pk-sequence ((field primary-key) (db database-platform))
  (declare (ignorable db))
  (format nil "CREATE SEQUENCE k~a;~a" (name field)
          (if *pretty-print* (format nil "~%") " ")))

(defmethod unparse-table-definition ((ent entity) (db database-platform) &optional (stream t))
  (let ((line-break (if *pretty-print* (format nil "~%    ") " ")))
    (format stream "~a" (unparse-pk-sequence (primary-key ent) db))
    (format stream "CREATE TABLE ~A (" (schema-name ent))
    (format stream "~a~a," line-break (unparse-field-definition (primary-key ent) db))
    (let* ((pointers (foreign-keys ent))
           (summaries (summary-attributes ent))
           (user-fields (user-attributes ent))
           (cached (append (cached-inherits ent) (cached-calculations ent)))
           (meta-data (when user-fields (audit-attributes ent))))
      (format stream "~{~a~^,~}"
              (sort (mapcar
                     #'(lambda (a)
                         (format nil "~A~A" line-break (unparse-field-definition a db)))
                     (append pointers cached summaries user-fields meta-data))
                    #'string-lessp)))
    (format stream ");")))

(defmethod unparse-field-definition ((field attribute) (db database-platform)
                             &optional (stream nil))
  (format stream "~a ~a~a~a~a" (schema-name field)
          (format-data-type (data-type field) *sql*)
          (if (nullable? field) "" " NOT NULL") (if (candidate-key? field) " UNIQUE" "")
          (let ((default (default-value field)))
            (if default (format nil " DEFAULT ~a"
                                (format-expression default *sql*)) ""))))

(defmethod unparse-field-definition ((field primary-key) (db database-platform)
                             &optional (stream nil))
  (format stream "~a INTEGER DEFAULT ~a PRIMARY KEY" (schema-name field)
          (format-expression (default-value field) *sql*)))


(defmethod format-expression ((exp cons) (lang programming-language))
  (ecase (car exp)
    ($unique "UNIQUE")
    ($current-date "now()")
    ($current-timestamp "CURRENT_TIMESTAMP")
    ($next-sequence-value (format nil "NEXTVAL('k~a')" (cadr exp)))))
|#

  
(defmethod unparse ((obj t))
  (warn "no unparse method written for objects of type ~a" (type-of obj))
  (simian::unparse obj))

(defmethod unparse ((obj string))
  (format nil "'~a'" (escape-characters obj #\' #\')))
(defmethod unparse ((obj number)) obj)
(defmethod unparse ((obj symbol)) (format nil "~a" (string-downcase (symbol-name obj))))

(defmethod unparse ((obj list)) (mapcar #'unparse obj))

(defmethod unparse ((obj attribute))
  ;; snake-case is a bit arbitrary but a common convention
  (format nil "~a.~a" (snake-case (plural (my-entity obj))) (snake-case (name obj))))

(defmethod unparse ((obj entity))
  ;; snake-case is a bit arbitrary but a common convention
  (format nil "~a" (snake-case (plural obj))))

(defmethod unparse ((obj relation))
  ;; snake-case is a bit arbitrary but a common convention
  (format nil "~a" (snake-case (plural (entity obj)))))

(defmethod unparse ((obj db-key))
  ;; this is all a bit arbitrary but a common convention 
  (strcat (snake-case (name obj)) "_id"))

(defmethod unparse ((obj calculated-attribute))
  (unparse-expression (formula obj)))

(defmethod unparse ((obj operator))
  (unparse (operator-key obj)))

(defmethod unparse ((obj formula))
  (let ((exp (expression obj)))
    (etypecase exp
      (string (unparse exp))
      (list (unparse-expression (car exp) (cdr exp)))
      (attribute (unparse exp)))))

(defmethod unparse-expression ((obj attribute) &optional args)
  (when args
    (error "we shouldn't have any args here...? (~a)" args))
  (unparse obj))

(defmethod unparse-expression ((obj calculated-attribute) &optional args)
  (when args
    (error "we shouldn't have any args here...? (~a)" args))
  (unparse-expression (formula obj)))

(defmethod unparse-expression ((obj t) &optional args)
  (when args (error "we shouldn't have any args here...? (~a)" args))
  (unparse obj))

(defmethod unparse-expression ((exp string) &optional args)
  (when args (error "we shouldn't have any args here...? (~a)" args))
  (unparse exp))

(defmethod unparse-expression ((exp number) &optional args)
  (when args (error "we shouldn't have any args here...? (~a)" args))
  exp)

(defmethod unparse-expression ((obj list) &optional args)
  (if (or (typep (car obj) 'operator) (operator-symbol? (car obj)))
      (progn
        (when args (error "we shouldn't have any args here...? (~a)" args))
	(unparse-expression (car obj) (cdr obj)))
      (if (field-reference-expression? obj)
	  (if (and (= 2 (length obj))
		   (eq (car obj) (my-entity (cadr obj))))
	      (unparse (cadr obj))
	      (unparse-select (list (car obj)) (list (cadr obj)) (cddr obj)))
	  (unparse obj))))

(defmethod unparse-attribute-references ((exp t) (context t) &optional obj-var)
  (declare (ignorable obj-var))
  (error "this one fell through the cracks: ~a - ~a" exp context))
(defmethod unparse-attribute-references ((exp operator) (context t) &optional obj-var)
  (declare (ignorable obj-var))
  exp)
(defmethod unparse-attribute-references ((exp number) (context t) &optional obj-var)
  (declare (ignorable obj-var))
  exp)
(defmethod unparse-attribute-references ((exp string) (context t) &optional obj-var)
  (declare (ignorable obj-var))
  exp)
(defmethod unparse-attribute-references ((exp entity) (context t) &optional obj-var)
  (declare (ignorable obj-var))
  (warn "not sure why this one is needed")
  exp)
(defmethod unparse-attribute-references ((obj entity-state) (context t) &optional obj-var)
  (unparse-attribute-references (expression (predicate obj)) context obj-var))
(defmethod unparse-attribute-references ((att attribute) (context attribute) &optional obj-var)
  (unparse-attribute-references att (my-entity context) obj-var))

;;; need a specialization for calculated-attribute
(defmethod unparse-attribute-references ((att attribute) (context entity) &optional obj-var)
  (if (eq (my-entity att) context)
      (as-literal (if obj-var (strcat obj-var "." (snake-case (name att)))
		      (unparse att)))
      (error "unable to resolve attribute reference (~a, ~a) without a condition component" att context)))

(defmethod unparse-attribute-references ((att attribute) (context relation) &optional obj-var)
  (if (find att (attributes context))
      (let ((entity (entity context)))
	(as-literal
	 (format nil "(select ~a from ~a where ~a.~a = ~a~a)" (unparse att) (unparse entity)
		 (unparse entity) (unparse (primary-key entity)) (if obj-var (strcat obj-var ".") "")
		 (reference-field-name context))))
      (error "unable to resolve attribute reference (~a, ~a)" att context)))

(defmethod reference-field-name ((rel relation))
  (snake-case (strcat (name rel) "_id")))
  
(defmethod unparse-attribute-references ((expr list) (context t) &optional obj-var)
  (if (field-reference-expression? expr)
      ;; stuck here: if expr is 3 long there is a where expression with two contexts for attribute var names
      (if (= 2 (length expr))
	  (progn
            (unless (typep (car expr) 'relation)
              (error "unless an attribute is specified in the context of a relationship there ~
                  needs to be a conditional component for a proper reference (~a - context: ~a)"
		     expr context))
            (unparse-attribute-references (cadr expr) (car expr) obj-var))
	  ;; we are here because this is a reference to an attribute in an unrelated entity
	  (as-literal (format nil "(~a)"
			      (unparse-select (list (cadr expr)) (list (first expr))
					      (cddr expr)))))
      (mapcar #'(lambda (ex)
                  (unparse-attribute-references ex context obj-var))
              expr)))

;;qqq debuggin this (sql::unparse-expression (caddr (predicate (first (states @project)))))
(defun unparse-select (fields tables wheres)
  (format nil "select ~{~a~^, ~} from ~{~a~^, ~}~a LIMIT 1" (mapcar #'unparse fields)
	  (mapcar #'(lambda (table) (snake-case (plural table))) tables)
	  (if wheres (format nil " where ~{~a~^ AND ~}" (mapcar #'unparse-expression wheres))
	      "")))

#|
"'Neo' = (select code from lookups where parent_entities.name = lookups.name"
'($eql "Neo" (Lookup Code
                    ($eql (ParentEntity Name) (Lookup Name))))
|#

(defmethod unparse-expression ((operator operator) &optional args)
  (unparse-expression (operator-key operator) args))

(defmethod unparse-expression ((operator (eql '$current-date)) &optional args)
  (declare (ignorable args))
  (format nil "CURRENT_DATE"))

(defmethod unparse-expression ((operator (eql '$current-timestamp)) &optional args)
  (declare (ignorable args))
  (format nil "CURRENT_TIMESTAMP"))

(defmethod unparse-expression ((operator (eql '$literal)) &optional args)
  (format nil "~a" (first args)))

(defmethod unparse-expression ((operator (eql '$next-sequence-value)) &optional args)
  (unless (string-equal "PostgreSQL" (name (db-platform *implementation*)))
    (warn "no implementation specific code written for ~a. Using PostgreSQL form"
          (name (db-platform *implementation*))))
  (format nil "nextval('~a')" (first args)))

(defmethod unparse-expression ((operator (eql '$not)) &optional args)
  (format nil "NOT(~a)" (unparse-expression (car args))))

(defmethod unparse-expression ((operator (eql '$=)) &optional args)
  (unparse-expression '$eql args))
(defmethod unparse-expression ((operator (eql '$eql)) &optional args)
  (format nil "~a = ~a" (unparse-expression (car args))
          (unparse-expression (cadr args))))

(defmethod unparse-expression ((operator (eql '$=)) &optional args)
  (unparse-expression '$not-eql args))
(defmethod unparse-expression ((operator (eql '$not-eql)) &optional args)
  (format nil "~a != ~a" (unparse-expression (car args))
          (unparse-expression (cadr args))))

(defmethod unparse-expression ((operator (eql '$>)) &optional args)
  (unparse-expression '$gt args))
(defmethod unparse-expression ((operator (eql '$gt)) &optional args)
  (format nil "~a > ~a" (unparse-expression (car args))
          (unparse-expression (cadr args))))
(defmethod unparse-expression ((operator (eql '$>=)) &optional args)
  (format nil "~a >= ~a" (unparse-expression (car args))
          (unparse-expression (cadr args))))

(defmethod unparse-expression ((operator (eql '$<)) &optional args)
  (unparse-expression '$lt args))
(defmethod unparse-expression ((operator (eql '$lt)) &optional args)
  (format nil "~a < ~a" (unparse-expression (car args))
          (unparse-expression (cadr args))))
(defmethod unparse-expression ((operator (eql '$<=)) &optional args)
  (format nil "~a <= ~a" (unparse-expression (car args))
          (unparse-expression (cadr args))))

(defmethod unparse-expression ((operator (eql '$null)) &optional args)
  (format nil "~a IS NULL" (unparse-expression (car args))))

(defmethod unparse-expression ((operator (eql '$not-null)) &optional args)
  (format nil "~a IS NOT NULL" (unparse-expression (car args))))

(defmethod unparse-expression ((operator (eql '$length)) &optional args)
  (unparse-expression '$eql
     (list (list '$literal
                 (format nil "LENGTH(~a)" (unparse-expression (car args))))
           (unparse-expression (cadr args)))))

(defmethod unparse-expression ((operator (eql '$between)) &optional args)
  (format nil "~a BETWEEN ~a AND ~a" (unparse-expression (first args))
          (unparse-expression (second args)) (unparse-expression (third args))))

(defmethod unparse-expression ((operator (eql '$length-between)) &optional args)
  (format nil "LENGTH(~a) BETWEEN ~a AND ~a" (unparse-expression (first args))
          (unparse-expression (second args)) (unparse-expression (third args))))

(defmethod unparse-expression ((operator (eql '$and)) &optional args)
  (format nil "~{~a~^ AND ~}" (mapcar #'unparse-expression args)))

(defmethod unparse-expression ((operator (eql '$or)) &optional args)
  (format nil "~{~a~^ OR ~}" (mapcar #'unparse-expression args)))

(defmethod unparse-expression ((operator (eql '$divide)) &optional args)
  (format nil "(~{~a~^ / ~})" (mapcar #'unparse-expression args)))

(defmethod unparse-expression ((operator (eql '$multiply>)) &optional args)
  (format nil "(~{~a~^ * ~})" (mapcar #'unparse-expression args)))

(defmethod unparse-expression ((operator (eql '$add)) &optional args)
    (format nil "(~{~a~^ + ~})" (mapcar #'unparse-expression args)))

(defmethod unparse-expression ((operator (eql '$subtract>)) &optional args)
    (format nil "(~{~a~^ - ~})" (mapcar #'unparse-expression args)))

(defmethod unparse-expression ((operator (eql '$in)) &optional args)
    (format nil "~a IN (~{~a~^, ~})" (unparse-expression (car args)) (mapcar #'unparse-expression (cdr args))))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
