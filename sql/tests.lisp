;;;===========================================================================
;;;
;;;  tests for code in unparsers/sql.lisp
;;;  
;;;===========================================================================

(in-package :sql-tests)

(define-test simple-unparse-tests
    (:tags '(simple-unparsing))
  (assert-equal 1 (unparse 1 :sql))
  (assert-equal "TRUE" (unparse t :sql))
  (assert-equal "'1'" (unparse "1" :sql))
  (assert-equal "FALSE" (unparse nil :sql)) ; don't know if this is correct, I have no usecase at the moment
  (assert-equal "NULL" (unparse :null :sql))
  (assert-equal "(1, 'foo', TRUE)" (unparse '(1 "foo" t) :sql)))

(define-test simple-operator-tests-by-key
    (:tags '(expressions operators simple-unparsing))
  (assert-equal "(1 + 1)" (unparse-expression '($add 1 1) :sql))
  (assert-equal "(1 + 1)" (unparse-expression :add :sql '(1 1)))
  (assert-equal "(1 - 1)" (unparse-expression '($subtract 1 1) :sql))
  (assert-equal "(1 * 1)" (unparse-expression '($multiply 1 1) :sql))
  (assert-equal "(1 / 1)" (unparse-expression '($divide 1 1) :sql))
  (assert-equal "((1 * (5 + 4)) / 17)" (unparse-expression '($divide ($multiply 1 ($add 5 4)) 17) :sql))
  (assert-equal "COALESCE(1, 1)" (unparse-expression :coalesce :sql '(1 1)))
  (assert-equal "'abn' ~* '[0-9]{11}'" (unparse-expression :regex :sql '("abn" "[0-9]{11}")))
  (assert-equal "'yes' IN ('yes', 'no')" (unparse-expression '(:in "yes" "yes" "no") :sql))
  (assert-equal "'yes' IN ('yes', 'no')" (unparse-expression :in :sql '("yes" "yes" "no")))
  (assert-equal "'nope' NOT IN ('yes', 'no')" (unparse-expression :not-in :sql '("nope" "yes" "no")))
  (assert-equal "LENGTH('foo') = 3" (unparse-expression :length :sql '("foo" 3)))
  (assert-equal "LENGTH('foo') BETWEEN 5 AND 7" (unparse-expression :length-between :sql '("foo" 5 7)))
;  (assert-equal "LENGTH('foo') > 5" (unparse-expression :length-gt :sql '("foo" 5)))
;  (assert-equal "LENGTH('foo') < 5" (unparse-expression :length-lt :sql '("foo" 5)))
  (assert-equal "'first' || ' ' || 'last'" (unparse-expression :concatenate :sql '("first" " " "last")))
  (assert-equal "'first' || ' ' || 'last'" (unparse-expression :strcat :sql '("first" " " "last")))
  (assert-equal "1 <= 1" (unparse-expression :<= :sql '(1 1)))
  (assert-equal "1 < 1" (unparse-expression :lt :sql '(1 1)))
  (assert-equal "1 < 1" (unparse-expression :< :sql '(1 1)))
  (assert-equal "1 > 1" (unparse-expression :gt :sql '(1 1)))
  (assert-equal "1 > 1" (unparse-expression :> :sql '(1 1)))
  (assert-equal "1 >= 1" (unparse-expression :>= :sql '(1 1)))
  (assert-equal "1 = 1" (unparse-expression := :sql '(1 1)))
  (assert-equal "1 != 1" (unparse-expression :!= :sql '(1 1)))
  (assert-equal "1 != 1" (unparse-expression :not-eql :sql '(1 1)))
  (assert-equal "1 = 1" (unparse-expression :eql :sql '(1 1)))
  (assert-equal "1 OR 2 OR 3" (unparse-expression :or :sql '(1 2 3)))
  (assert-equal "1 AND 2 AND 3" (unparse-expression :and :sql '(1 2 3)))
  (assert-signal 'error (unparse-expression :function :sql '(1 1)))
  (assert-equal "my_function(1)" (unparse-expression :call :sql '("my_function" 1)))
  (assert-equal "'string' IS NULL" (unparse-expression :null :sql '("string")))
  (assert-equal "'string' IS NOT NULL" (unparse-expression :not-null :sql '("string")))
  (assert-equal "(1 + 1)" (unparse-expression :add :sql '(1 1)))
  (assert-equal "(1 - 1)" (unparse-expression :subtract :sql '(1 1)))
  (assert-equal "(1 * 1)" (unparse-expression :multiply :sql '(1 1)))
  (assert-equal "(1 / 1)" (unparse-expression :divide :sql '(1 1)))
  (assert-equal "NOT(okay)" (unparse-expression :not :sql '(okay)))
  (assert-equal "CURRENT_DATE" (unparse-expression :current-date :sql))
  (assert-equal "CURRENT_TIMESTAMP" (unparse-expression :current-timestamp :sql))
  (assert-equal "100::TEXT" (unparse-expression :to-string :sql '(100)))
  (assert-equal "my_value BETWEEN 5 AND 7" (unparse-expression :between :sql '(my_value 5 7)))
  (assert-equal "complex.code(\"can not\", \"generate\"" (unparse-expression :literal :sql '("complex.code(\"can not\", \"generate\"")))
  )

(define-pending-test unparse-attribute-reference-tests :tags '(unparsing))
(define-pending-test unparse-select-tests :tags '(unparsing))
(define-pending-test unparse-attribute-entity-relationship :tags '(unparsing))
(define-pending-test unparse-table-definition :tags '(unparsing))


#|

'(define-test :sql
  "CREATE TABLE looks good"
(with-new-schema
    (define-lookup-table ("Lookup"))
  (with-output-to-string (str)  
   ;; see unparse-table-definition, create-table is old code 
    (sql::create-table (find-entity :lookup) (make-instance 'database-platform) str)))
  "CREATE SEQUENCE kLookup;
CREATE TABLE tLookups (
    kLookup INTEGER DEFAULT NEXTVAL('kLookup') PRIMARY KEY,
    fDescription TEXT,
    fName TEXT NOT NULL UNIQUE,
    mSysAddTime TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    mSysChangeCount INTEGER NOT NULL DEFAULT 0,
    mSysChangeOrigin TEXT NOT NULL DEFAULT 'unknown',
    mSysCreator TEXT NOT NULL DEFAULT 'unknown',
    mSysEditor TEXT NOT NULL DEFAULT 'unknown',
    mSysModTime TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP);")

'(define-test :sql "simple unparsers are working"
  (with-new-schema
      (define-entity ("Ent")
          :attributes (:code
                       ("Quantity" :type quantity :default 1 :nullable? nil)
                       ("Rate"    :type money :formula ($divide Amount Quantity))
                       ("Amount" :type money :nullable? nil)))
    (resolve-attribute-derivations nil)
    (mapcar #'sql::unparse-expression
            (list
             (list '$add (find-field :rate :ent) (find-field :Amount :ent) 2)
             (list '$length (find-field :code :ent) 4)
             (list '$between (find-field :rate :ent) 1 1000)
             (list '$not-null (find-field :Amount :ent))
             (list '$eql (find-field :Amount :ent) (find-field :Quantity :ent))
             (list '$>= (find-field :Quantity :ent) 2))))
  '("((Ents.amount / Ents.quantity) + Ents.amount + 2)"
    "LENGTH(Ents.code) = 4"
    "(Ents.amount / Ents.quantity) BETWEEN 1 AND 1000"
    "Ents.amount IS NOT NULL"
    "Ents.amount = Ents.quantity"
    "Ents.quantity >= 2"))

'(define-test :sql "attribute referencing expressions can be unparsed"
  (with-new-schema
      (define-entity ("Parent")
            :attributes ((:entity-name) :email
                         ("ChildrenLabel" :type label :formula (Other Value ($eql Index Children))))
            :states (("LotsOfKids" ($>= Children 4))
                     ("IsSpecial" ($eql Name (Other Value ($eql Index 0))))))
        (define-entity ("Child")
            :attributes ((:entity-name) :birthdate
                         ("EmailContact" :type email :formula (MyParent Email)))
            :states (("ParentHasEmail" ($not-null (MyParent Email)))
                     ("AlsoParentHasEmail" ($not-null (Parent Email)))))
        (define-entity ("Other")
            :attributes (("Index" :type integer)
                         ("Value" :type label)))
        (define-relationship ((Parent (1 1))
                              ("has" "have")
                              (Child (0 *)))
            :name ("Family")
            :lhs-properties (:dependency :independent :name "MyParent")
            :rhs-properties (:dependency :dependent))
        (post-parse-project)
        (states (find-entity :parent))
        (mapcar #'sql::unparse-expression
                (list (sql::unparse-attribute-references (find-field :Birthdate :Child)  (find-entity :child))
;                      (sql::unparse-attribute-references (find-field :EmailContact :Child)  (find-entity :child))
                      (sql::unparse-attribute-references
                       (find-field :email :parent) (lhs (car (relationships (find-entity :child)))))
                      (sql::unparse-attribute-references
                       (find-state (find-entity :parent) :lotsofkids) (find-entity :parent))
                      (sql::unparse-attribute-references
                       (find-state (find-entity :parent) :IsSpecial) (find-entity :parent))
                      (sql::unparse-attribute-references
                       (find-state (find-entity :child) :ParentHasEmail) (find-entity :child))
                      (sql::unparse-attribute-references
                       (find-state (find-entity :child) :AlsoParentHasEmail) (find-entity :child)))))
  '("children.birth_date"
    "(select parents.email from parents where parents.parent_id = my_parent_id)"
    "parents.children >= 4"
    "parents.name = (select others.value from others where others.index = 0 LIMIT 1)"
    "(select parents.email from parents where parents.parent_id = my_parent_id) IS NOT NULL"
    "(select parents.email from parents where parents.parent_id = my_parent_id) IS NOT NULL"))

'(define-test :sql "predicate unparsing is working"
  (with-new-schema
      (define-lookup-table ("Lookup") :with-code? t)
    (define-aggregation :parent-dependent
        :name "TestAgg"
        :parent (("ParentEntity")
                 :attributes ((:entity-name) :email)
                 :states (("Contactable" ($not-null Email))
                          ("VeryFertile" ($> ChildEntities 3))
                          ("TheOne" ($eql "Neo" (Lookup Code
                                                        ($eql (ParentEntity Name) (Lookup Name)))))))
        :child (("ChildEntity")
                :attributes (:name)
                :states (("ChildOfNeo" ($eql "Neo" (ParentEntity Name))))))
    (resolve-entity-state-specifications)
    (loop for state in (append (states (find-entity :ParentEntity))
                               (states (find-entity :ChildEntity)))
          collect (format nil "~a: ~a" (name state)
                          (sql::unparse-expression
                           (sql::unparse-attribute-references (expression (predicate state))
                                                              (my-entity state))))))
  '("Contactable: parent_entities.email IS NOT NULL"
    "VeryFertile: parent_entities.child_entities > 3"
    "TheOne: 'Neo' = (select lookups.code from lookups where parent_entities.name = lookups.name LIMIT 1)"
    "ChildOfNeo: 'Neo' = (select parent_entities.name from parent_entities where parent_entities.parent_entity_id = parent_entity_id)"))
|#


;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
