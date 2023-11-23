;;;===========================================================================
;;;
;;;  tests for code in unparsers/sql.lisp
;;;  
;;;===========================================================================

(in-package :sql-tests)

(define-test simple-unparse-tests
    (:tags '(simple-unparsing))
  (assert-equal "1" (unparse 1 :sql))
  (assert-equal "TRUE" (unparse t :sql))
  (assert-equal "'1'" (unparse "1" :sql))
  (assert-equal "FALSE" (unparse nil :sql))
  (assert-equal "NULL" (unparse :null :sql))
  (assert-equal "[1, 'foo', true]" (unparse '(1 "foo" t) :sql)) ; this is just what it does, I don't think it is correct FIXME
  (assert-equal "[1, 2, 3]" (unparse-array '(1 2 3) :sql))
  (assert-equal (format nil "{~%  :key1 => 1,~%  :key2 => 2,~%  :key3 => 3~%}") (unparse-hash '((:key1 1) (:key2 2) (:key3 3))))
  (assert-equal (format nil "{ :key1 => 1, :key2 => 2, :key3 => 3 }") (unparse-hash '((:key1 1) (:key2 2) (:key3 3)) :one-line t))
  (assert-equal (format nil "{~%  :key1 => 1,~%  :key4 =>~%  {~%    :key2 => 2,~%    :key3 => 3~%  }~%}") (unparse-hash '((:key1 1) (:key4 ((:key2 2) (:key3 3))))))
  (assert-equal (format nil "{ :key1 => 1, :key4 => { :key2 => 2, :key3 => 3 } }") (unparse-hash '((:key1 1) (:key4 ((:key2 2) (:key3 3)))) :one-line t)))

(define-test simple-operator-tests-by-key
    (:tags '(expressions operators simple-unparsing))
  (assert-equal "(1 + 1)" (unparse-expression '($add 1 1) :sql))
  (assert-equal "(1 + 1)" (unparse-expression :add :sql '(1 1)))
  (assert-equal "(1 - 1)" (unparse-expression '($subtract 1 1) :sql))
  (assert-equal "(1 * 1)" (unparse-expression '($multiply 1 1) :sql))
  (assert-equal "(1 / 1)" (unparse-expression '($divide 1 1) :sql))
  (assert-equal "((1 * (5 + 4)) / 17)" (unparse-expression '($divide ($multiply 1 ($add 5 4)) 17) :sql))
  (assert-equal "(1 || 1)" (unparse-expression :coalesce :sql '(1 1)))
  (assert-equal "'abn' =~ /[0-9]{11}/" (unparse-expression :regex :sql '("abn" "[0-9]{11}")))
  (assert-equal "['yes', 'no'].include?('yes')" (unparse-expression '(:in "yes" "yes" "no") :sql))
  (assert-equal "['yes', 'no'].include?('yes')" (unparse-expression :in :sql '("yes" "yes" "no")))
  (assert-equal "!['yes', 'no'].include?('nope')" (unparse-expression :not-in :sql '("nope" "yes" "no")))
  (assert-equal "(foo.length == 3)" (unparse-expression :length :sql '(foo 3)))
  (assert-equal "'foo'.length.between?(5, 7)" (unparse-expression :length-between :sql '("foo" 5 7)))
  (assert-equal "('foo'.length > 5)" (unparse-expression :length-gt :sql '("foo" 5)))
  (assert-equal "('foo'.length < 5)" (unparse-expression :length-lt :sql '("foo" 5)))
  (assert-equal "('first' + ' ' + 'last')" (unparse-expression :concatenate :sql '("first" " " "last")))
  (assert-equal "('first' + ' ' + 'last')" (unparse-expression :strcat :sql '("first" " " "last")))
  (assert-equal "1.even?" (unparse-expression :even :sql '(1)))
  (assert-equal "1.odd?"  (unparse-expression :odd :sql '(1)))
  (assert-equal "(1 <= 1)" (unparse-expression :<= :sql '(1 1)))
  (assert-equal "(1 < 1)" (unparse-expression :lt :sql '(1 1)))
  (assert-equal "(1 < 1)" (unparse-expression :< :sql '(1 1)))
  (assert-equal "(1 > 1)" (unparse-expression :gt :sql '(1 1)))
  (assert-equal "(1 > 1)" (unparse-expression :> :sql '(1 1)))
  (assert-equal "(1 >= 1)" (unparse-expression :>= :sql '(1 1)))
  (assert-equal "(1 == 1)" (unparse-expression := :sql '(1 1)))
  (assert-equal "(1 != 1)" (unparse-expression :!= :sql '(1 1)))
  (assert-equal "(1 != 1)" (unparse-expression :not-eql :sql '(1 1)))
  (assert-equal "(1 == 1)" (unparse-expression :eql :sql '(1 1)))
  (assert-equal "(1 || 1)" (unparse-expression :or :sql '(1 1)))
  (assert-equal "(1 && 1)" (unparse-expression :and :sql '(1 1)))
  (assert-signal 'error (unparse-expression :function :sql '(1 1)))
  (assert-equal "my_function(1)" (unparse-expression :call :sql '("my_function" 1)))
  (assert-signal 'error (unparse-expression :shared-method :sql '(1 1))) ;; not implemented yet
  (assert-equal "foo.my_method" (unparse-expression :method :sql '("my_method" foo)))
  (assert-equal "('string'.nil? && 'string'.empty?)" (unparse-expression :null :sql '("string")))
  (assert-equal "!('string'.nil?) && !('string'.empty?)" (unparse-expression :not-null :sql '("string")))
  (assert-signal 'error (unparse-expression :not-blank :sql '("string"))) ;; not implemented yet
  (assert-equal "(1 * 1) unless foo.my_method" (unparse-expression :unless :sql '(($method "my_method" foo) ($multiply 1 1))))
  (assert-equal "(1 * 1) if foo.my_method" (unparse-expression :when :sql '(($method "my_method" foo) ($multiply 1 1))))
  (assert-equal (format nil "if true~%  1~%else~%  2~%end") (unparse-expression :if :sql '(t 1 2)))
  (assert-equal (format nil "  if true~%    1~%  else~%    2~%  end") (let ((*nesting-level* 1)) (unparse-expression :if :sql '(t 1 2))))
  (assert-equal "(1 + 1)" (unparse-expression :add :sql '(1 1)))
  (assert-equal "(1 - 1)" (unparse-expression :subtract :sql '(1 1)))
  (assert-equal "(1 * 1)" (unparse-expression :multiply :sql '(1 1)))
  (assert-equal "(1 / 1)" (unparse-expression :divide :sql '(1 1)))
  (assert-equal "!(okay)" (unparse-expression :not :sql '(okay)))
  (assert-equal "Date.today" (unparse-expression :current-date :sql))
  (assert-equal "Time.now" (unparse-expression :current-time :sql))
  (assert-equal "Time.now" (unparse-expression :current-timestamp :sql))
  (assert-equal "100.to_s" (unparse-expression :to-string :sql '(100)))
  (assert-equal "the_date.strftime(\"%d %b, %Y\")" (unparse-expression :as-date :sql '(the_date)))
;  (assert-equal "(1 * 1)" (unparse-expression :interval :sql '(1 1)))
  (assert-equal "my_value.between?(5, 7)" (unparse-expression :between :sql '(my_value 5 7)))
  (assert-equal "complex.code(\"can not\", \"generate\"" (unparse-expression :literal :sql '("complex.code(\"can not\", \"generate\"")))
  (assert-equal "[1, 2, 3].min" (unparse-expression :min :sql '((1 2 3))))
  (assert-equal "[1, 2, 3].max" (unparse-expression :max :sql '((1 2 3))))
  )

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
