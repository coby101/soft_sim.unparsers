;;;==================================================================================
;;;
;;;  - methods and functions related to writing SQL for simian generators
;;;   
;;;==================================================================================

(in-package :sql)

(defparameter *pretty-print* t)

(defmethod unparse ((obj string) (language (eql :sql)))
  (format nil "'~a'" (escape-characters obj #\' #\')))
(defmethod unparse ((obj number) (language (eql :sql))) obj)
(defmethod unparse ((obj symbol) (language (eql :sql)))
  (cond ((eq obj :null) "NULL")
        ((eq obj t) "TRUE")
        ((eq obj nil) "FALSE")
        ((keywordp obj) (format nil ":~a" (snake-case (case-sensitive-symbol-name obj))))
        (t (format nil "~a" (format nil "~a" (string-downcase (symbol-name obj)))))))

(defmethod unparse ((obj list) (language (eql :sql)))
  (format nil "(~{~a~^, ~})" (mapcar #'(lambda (item) (unparse item language)) obj)))

(defmethod unparse ((obj attribute) (language (eql :sql)))
  ;; snake-case is a bit arbitrary but a common convention
  (format nil "~a.~a" (snake-case (plural (my-entity obj))) (snake-case (name obj))))

(defmethod unparse ((obj entity) (language (eql :sql)))
  ;; snake-case is a bit arbitrary but a common convention
  (format nil "~a" (snake-case (plural obj))))

(defmethod unparse ((obj relation) (language (eql :sql)))
  ;; snake-case is a bit arbitrary but a common convention
  (format nil "~a" (snake-case (plural (entity obj)))))

(defmethod unparse ((obj db-key) (language (eql :sql)))
  ;; this is all a bit arbitrary but a common convention 
  (strcat (snake-case (name obj)) "_id"))

;; what is this for? if really useful, should go to soft_sim/src/unparser.lisp FIXME
(defmethod unparse ((obj operator) (language (eql :sql)))
  (unparse (operator-key obj) language))

;; if really useful, should go to soft_sim/src/unparser.lisp FIXME
(defmethod unparse-expression ((obj attribute) (language (eql :sql)) &optional args)
  (when args
    (error "we shouldn't have any args here...? (~a)" args))
  (unparse obj language))

(defmethod unparse-expression ((exp string) (language (eql :sql)) &optional args)
  (when args (error "we shouldn't have any args here...? (~a)" args))
  ;; in ruby version we escape characters first. should refactor and have
  ;; generic (defmethod unparse-expression ((exp string) (language t) &optional args) with
  ;; language specific string escape code
  (unparse exp language))

(defun unparse-select (fields tables wheres)
  (format nil "select ~{~a~^, ~} from ~{~a~^, ~}~a LIMIT 1" (mapcar #'(lambda (field) (unparse field :sql)) fields)
	  (mapcar #'(lambda (table) (snake-case (plural table))) tables)
	  (if wheres (format nil " where ~{~a~^ AND ~}" (mapcar #'(lambda (clause) (unparse-expression clause :sql)) wheres))
	      "")))

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
		      (unparse att :sql)))
      (error "unable to resolve attribute reference (~a, ~a) without a condition component" att context)))

(defmethod reference-field-name ((rel relation))
  (snake-case (strcat (name rel) "_id")))

(defmethod unparse-attribute-references ((att attribute) (context relation) &optional obj-var)
  (if (find att (attributes context))
      (let ((entity (entity context)))
	(as-literal
	 (format nil "(select ~a from ~a where ~a.~a = ~a~a)" (unparse att :sql) (unparse entity :sql)
		 (unparse entity :sql) (unparse (primary-key entity) :sql) (if obj-var (strcat obj-var ".") "")
		 (reference-field-name context))))
      (error "unable to resolve attribute reference (~a, ~a)" att context)))
  
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

#|
"'Neo' = (select code from lookups where parent_entities.name = lookups.name"
'($eql "Neo" (Lookup Code
                    ($eql (ParentEntity Name) (Lookup Name))))
|#

(defmethod unparse-expression ((operator (eql :current-date)) (language (eql :sql)) &optional args)
  (declare (ignorable args))
  (format nil "CURRENT_DATE"))

(defmethod unparse-expression ((operator (eql :current-timestamp)) (language (eql :sql)) &optional args)
  (declare (ignorable args))
  (format nil "CURRENT_TIMESTAMP"))

(defmethod unparse-expression ((operator (eql :next-sequence-value)) (language (eql :sql)) &optional args)
  (unless (string-equal "PostgreSQL" (name (db-platform *implementation*)))
    (warn "no implementation specific code written for ~a. Using PostgreSQL form"
          (name (db-platform *implementation*))))
  (format nil "nextval('~a')" (first args)))

(defmethod unparse-expression ((operator (eql :call)) (language (eql :sql)) &optional args)
  (format nil "~a~a" (first args)
              (if (cdr args)
                  (format nil "(~{~a~^, ~})" (mapcar #'(lambda (arg) (unparse-expression arg language)) (cdr args)))
                  "")))

(defmethod unparse-expression ((operator (eql :not)) (language (eql :sql)) &optional args)
  (format nil "NOT(~a)" (unparse-expression (car args) language)))

(defmethod unparse-expression ((operator (eql :=)) (language (eql :sql)) &optional args)
  (unparse-expression :eql language args))
(defmethod unparse-expression ((operator (eql :eql)) (language (eql :sql)) &optional args)
  (format nil "~a = ~a" (unparse-expression (car args) language)
          (unparse-expression (cadr args) language)))

(defmethod unparse-expression ((operator (eql :!=)) (language (eql :sql)) &optional args)
  (unparse-expression :not-eql language args))
(defmethod unparse-expression ((operator (eql :not-eql)) (language (eql :sql)) &optional args)
  (format nil "~a != ~a" (unparse-expression (car args) language)
          (unparse-expression (cadr args) language)))

(defmethod unparse-expression ((operator (eql :>)) (language (eql :sql)) &optional args)
  (unparse-expression :gt language args))
(defmethod unparse-expression ((operator (eql :gt)) (language (eql :sql)) &optional args)
  (format nil "~a > ~a" (unparse-expression (car args) language)
          (unparse-expression (cadr args) language)))
(defmethod unparse-expression ((operator (eql :>=)) (language (eql :sql)) &optional args)
  (format nil "~a >= ~a" (unparse-expression (car args) language)
          (unparse-expression (cadr args) language)))

(defmethod unparse-expression ((operator (eql :<)) (language (eql :sql)) &optional args)
  (unparse-expression :lt language args))
(defmethod unparse-expression ((operator (eql :lt)) (language (eql :sql)) &optional args)
  (format nil "~a < ~a" (unparse-expression (car args) language)
          (unparse-expression (cadr args) language)))
(defmethod unparse-expression ((operator (eql :<=)) (language (eql :sql)) &optional args)
  (format nil "~a <= ~a" (unparse-expression (car args) language)
          (unparse-expression (cadr args) language)))

(defmethod unparse-expression ((operator (eql :null)) (language (eql :sql)) &optional args)
  (format nil "~a IS NULL" (unparse-expression (car args) language)))

(defmethod unparse-expression ((operator (eql :not-null)) (language (eql :sql)) &optional args)
  (format nil "~a IS NOT NULL" (unparse-expression (car args) language)))

(defmethod unparse-expression ((operator (eql :length)) (language (eql :sql)) &optional args)
  (unparse-expression :eql :sql
     (list (list :literal
                 (format nil "LENGTH(~a)" (unparse-expression (car args) language)))
           (unparse-expression (cadr args) language))))

(defmethod unparse-expression ((operator (eql :between)) (language (eql :sql)) &optional args)
  (format nil "~a BETWEEN ~a AND ~a" (unparse-expression (first args) language)
          (unparse-expression (second args) language) (unparse-expression (third args) language)))

(defmethod unparse-expression ((operator (eql :length-between)) (language (eql :sql)) &optional args)
  (format nil "LENGTH(~a) BETWEEN ~a AND ~a" (unparse-expression (first args) language)
          (unparse-expression (second args) language) (unparse-expression (third args) language)))

(defmethod unparse-expression ((operator (eql :concatenate)) (language (eql :sql)) &optional args)
  (unparse-expression :strcat language args))
(defmethod unparse-expression ((operator (eql :strcat)) (language (eql :sql)) &optional args)
  (format nil "~{~a~^ || ~}" (mapcar #'(lambda(exp)
                                          (unparse-expression (list :to-string exp) language))
                                      args)))
(defmethod unparse-expression ((operator (eql :to-string)) (language (eql :sql)) &optional args)
  (if (returns-string? (car args))
      (unparse-expression (car args) language)
      (format nil "~a::TEXT" (unparse-expression (car args) language))))

(defmethod unparse-expression ((operator (eql :and)) (language (eql :sql)) &optional args)
  (format nil "~{~a~^ AND ~}" (mapcar #'(lambda (arg) (unparse-expression arg language)) args)))

(defmethod unparse-expression ((operator (eql :or)) (language (eql :sql)) &optional args)
  (format nil "~{~a~^ OR ~}" (mapcar #'(lambda (arg) (unparse-expression arg language)) args)))

(defmethod unparse-expression ((operator (eql :divide)) (language (eql :sql)) &optional args)
  (format nil "(~{~a~^ / ~})" (mapcar #'(lambda (arg) (unparse-expression arg language)) args)))

(defmethod unparse-expression ((operator (eql :multiply)) (language (eql :sql)) &optional args)
  (format nil "(~{~a~^ * ~})" (mapcar #'(lambda (arg) (unparse-expression arg language)) args)))

(defmethod unparse-expression ((operator (eql :add)) (language (eql :sql)) &optional args)
  (format nil "(~{~a~^ + ~})" (mapcar #'(lambda (arg) (unparse-expression arg language)) args)))

(defmethod unparse-expression ((operator (eql :subtract)) (language (eql :sql)) &optional args)
  (format nil "(~{~a~^ - ~})" (mapcar #'(lambda (arg) (unparse-expression arg language)) args)))

(defmethod unparse-expression ((operator (eql :in)) (language (eql :sql)) &optional args)
  (format nil "~a IN (~{~a~^, ~})" (unparse-expression (car args) language) (mapcar #'(lambda (arg) (unparse-expression arg language)) (cdr args))))

(defmethod unparse-expression ((operator (eql :not-in)) (language (eql :sql)) &optional args)
  (format nil "~a NOT IN (~{~a~^, ~})" (unparse-expression (car args) language) (mapcar #'(lambda (arg) (unparse-expression arg language)) (cdr args))))

(defmethod unparse-expression ((operator (eql :coalesce)) (language (eql :sql)) &optional args)
  (format nil "COALESCE~a" (unparse args language)))

(defmethod unparse-expression ((operator (eql :regex)) (language (eql :sql)) &optional args)
  (format nil "~a ~~* '~a'" (unparse-expression (car args) language) (cadr args)))


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


;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
