;;;===========================================================================
;;;
;;;  tests for code in unparsers/ruby/ruby.lisp
;;;  
;;;===========================================================================

(in-package :ruby-tests)

(define-test simple-unparse-tests
    (:tags '(simple-unparsing))
  (assert-equal "1" (unparse 1 :ruby))
  (assert-equal "true" (unparse t :ruby))
  (assert-equal "'1'" (unparse "1" :ruby))
  (assert-equal "nil" (unparse nil :ruby))
  (assert-equal "[1, 'foo', true]" (unparse '(1 "foo" t) :ruby)) ; this is just what it does, I don't think it is correct FIXME
  (assert-equal "[1, 2, 3]" (unparse-array '(1 2 3) :ruby))
  (assert-equal (format nil "{~%  :key1 => 1,~%  :key2 => 2,~%  :key3 => 3~%}") (unparse-hash '((:key1 1) (:key2 2) (:key3 3))))
  (assert-equal (format nil "{ :key1 => 1, :key2 => 2, :key3 => 3 }") (unparse-hash '((:key1 1) (:key2 2) (:key3 3)) :one-line t))
  (assert-equal (format nil "{~%  :key1 => 1,~%  :key4 =>~%  {~%    :key2 => 2,~%    :key3 => 3~%  }~%}") (unparse-hash '((:key1 1) (:key4 ((:key2 2) (:key3 3))))))
  (assert-equal (format nil "{ :key1 => 1, :key4 => { :key2 => 2, :key3 => 3 } }") (unparse-hash '((:key1 1) (:key4 ((:key2 2) (:key3 3)))) :one-line t)))

(define-test simple-operator-tests-by-key
    (:tags '(expressions operators simple-unparsing))
  (assert-equal "(1 + 1)" (unparse-expression '($add 1 1) :ruby))
  (assert-equal "(1 + 1)" (unparse-expression :add :ruby '(1 1)))
  (assert-equal "(1 - 1)" (unparse-expression '($subtract 1 1) :ruby))
  (assert-equal "(1 * 1)" (unparse-expression '($multiply 1 1) :ruby))
  (assert-equal "(1 / 1)" (unparse-expression '($divide 1 1) :ruby))
  (assert-equal "((1 * (5 + 4)) / 17)" (unparse-expression '($divide ($multiply 1 ($add 5 4)) 17) :ruby))
  (assert-equal "(1 || 1)" (unparse-expression :coalesce :ruby '(1 1)))
  (assert-equal "'abn' =~ /[0-9]{11}/" (unparse-expression :regex :ruby '("abn" "[0-9]{11}")))
  (assert-equal "['yes', 'no'].include?('yes')" (unparse-expression '(:in "yes" "yes" "no") :ruby))
  (assert-equal "['yes', 'no'].include?('yes')" (unparse-expression :in :ruby '("yes" "yes" "no")))
  (assert-equal "!['yes', 'no'].include?('nope')" (unparse-expression :not-in :ruby '("nope" "yes" "no")))
  (assert-equal "(foo.length == 3)" (unparse-expression :length :ruby '(foo 3)))
  (assert-equal "'foo'.length.between?(5, 7)" (unparse-expression :length-between :ruby '("foo" 5 7)))
  (assert-equal "('foo'.length > 5)" (unparse-expression :length-gt :ruby '("foo" 5)))
  (assert-equal "('foo'.length < 5)" (unparse-expression :length-lt :ruby '("foo" 5)))
  (assert-equal "('first' + ' ' + 'last')" (unparse-expression :concatenate :ruby '("first" " " "last")))
  (assert-equal "('first' + ' ' + 'last')" (unparse-expression :strcat :ruby '("first" " " "last")))
  (assert-equal "1.even?" (unparse-expression :even :ruby '(1)))
  (assert-equal "1.odd?"  (unparse-expression :odd :ruby '(1)))
  (assert-equal "(1 <= 1)" (unparse-expression :<= :ruby '(1 1)))
  (assert-equal "(1 < 1)" (unparse-expression :lt :ruby '(1 1)))
  (assert-equal "(1 < 1)" (unparse-expression :< :ruby '(1 1)))
  (assert-equal "(1 > 1)" (unparse-expression :gt :ruby '(1 1)))
  (assert-equal "(1 > 1)" (unparse-expression :> :ruby '(1 1)))
  (assert-equal "(1 >= 1)" (unparse-expression :>= :ruby '(1 1)))
  (assert-equal "(1 == 1)" (unparse-expression := :ruby '(1 1)))
  (assert-equal "(1 != 1)" (unparse-expression :!= :ruby '(1 1)))
  (assert-equal "(1 != 1)" (unparse-expression :not-eql :ruby '(1 1)))
  (assert-equal "(1 == 1)" (unparse-expression :eql :ruby '(1 1)))
  (assert-equal "(1 || 1)" (unparse-expression :or :ruby '(1 1)))
  (assert-equal "(1 && 1)" (unparse-expression :and :ruby '(1 1)))
  (assert-signal 'error (unparse-expression :function :ruby '(1 1)))
  (assert-equal "my_function(1)" (unparse-expression :call :ruby '("my_function" 1)))
  (assert-signal 'error (unparse-expression :shared-method :ruby '(1 1))) ;; not implemented yet
  (assert-equal "foo.my_method" (unparse-expression :method :ruby '("my_method" foo)))
  (assert-equal "('string'.nil? && 'string'.empty?)" (unparse-expression :null :ruby '("string")))
  (assert-equal "!('string'.nil?) && !('string'.empty?)" (unparse-expression :not-null :ruby '("string")))
  (assert-signal 'error (unparse-expression :not-blank :ruby '("string"))) ;; not implemented yet
  (assert-equal "(1 * 1) unless foo.my_method" (unparse-expression :unless :ruby '(($method "my_method" foo) ($multiply 1 1))))
  (assert-equal "(1 * 1) if foo.my_method" (unparse-expression :when :ruby '(($method "my_method" foo) ($multiply 1 1))))
  (assert-equal (format nil "if true~%  1~%else~%  2~%end") (unparse-expression :if :ruby '(t 1 2)))
  (assert-equal (format nil "  if true~%    1~%  else~%    2~%  end") (let ((*nesting-level* 1)) (unparse-expression :if :ruby '(t 1 2))))
  (assert-equal "(1 + 1)" (unparse-expression :add :ruby '(1 1)))
  (assert-equal "(1 - 1)" (unparse-expression :subtract :ruby '(1 1)))
  (assert-equal "(1 * 1)" (unparse-expression :multiply :ruby '(1 1)))
  (assert-equal "(1 / 1)" (unparse-expression :divide :ruby '(1 1)))
  (assert-equal "!(okay)" (unparse-expression :not :ruby '(okay)))
  (assert-equal "Date.today" (unparse-expression :current-date :ruby))
  (assert-equal "Time.now" (unparse-expression :current-time :ruby))
  (assert-equal "Time.now" (unparse-expression :current-timestamp :ruby))
  (assert-equal "100.to_s" (unparse-expression :to-string :ruby '(100)))
  (assert-equal "the_date.strftime(\"%d %b, %Y\")" (unparse-expression :as-date :ruby '(the_date)))
;  (assert-equal "(1 * 1)" (unparse-expression :interval :ruby '(1 1)))
  (assert-equal "my_value.between?(5, 7)" (unparse-expression :between :ruby '(my_value 5 7)))
  (assert-equal "complex.code(\"can not\", \"generate\"" (unparse-expression :literal :ruby '("complex.code(\"can not\", \"generate\"")))
  (assert-equal "[1, 2, 3].min" (unparse-expression :min :ruby '((1 2 3))))
  (assert-equal "[1, 2, 3].max" (unparse-expression :max :ruby '((1 2 3))))
  )

(define-pending-test methods-ranges-lambdas-data :tags '(unparsing))

#|
(define-test :ruby "simple formulas unparse in ruby nicely"
  (with-new-schema
      (define-entity ("Ent")
          :attributes (:code
                       :name
                       ("Quantity" :type quantity :default 1 :nullable? nil)
                       ("Rate"    :type money :formula ($divide Amount Quantity))
                       ("Amount" :type money :nullable? nil)))
    (resolve-attribute-derivations nil)
    (mapcar #':unparse-expression
            (list
             (list '$in (find-field :code :ent) "c1" "c2" "c3")
             (list '$not-in (find-field :code :ent) "c1" "c2" "c3")
             (list '$add (find-field :Quantity :ent) 2)
             (list '$multiply (find-field :Amount :ent) 1.1)
             (list '$lt (find-field :Amount :ent) 1000)
             (list '$as-money (find-field :Rate :ent))
             (list '$coalesce (find-field :name :ent) "(no name)")
             (list '$strcat (find-field :name :ent) " has the best rate: " (formula (find-field :Rate :ent)))
             (list '$regex (find-field :code :ent) "[A-Z]{3}"))))
  '("[\"c1\", \"c2\", \"c3\"].include? code"
    "[\"c1\", \"c2\", \"c3\"].exclude? code"
    "(quantity + 2)"
    "(amount * 1.1)"
    "(amount < 1000)"
    "number_to_currency((amount / quantity))"
    "(name || '(no name)')"
    "(name + ' has the best rate: ' + (amount / quantity).to_s)"
    "code =~ /[A-Z]{3}/"))

(define-test :ruby "row counting formulas unparse in ruby nicely"
  (with-new-schema
      (let ((*include-rails* t))
        (load-unparser "sql")
        (define-entity ("Ent")
            :attributes (:code
                         ("Quantity" :type quantity :default 1 :nullable? nil)
                         ("Rate"    :type money :formula ($divide Amount Quantity))
                         ("Amount" :type money :nullable? nil)))
        (resolve-attribute-derivations nil)
        (mapcar #'ruby::unparse-expression
                (list
                 (list '$rows-eql (find-entity :ent) 1)
                 (list '$min-rows (find-entity :ent) 5)
                 (list '$max-rows (find-entity :ent) 2 (list '$> (find-field :Amount :ent) 100))))))
  '("(Ent.count == 1)"
    "(Ent.count >= 5)"
    "(Ent.where(\"Ents.amount > 100\").count <= 2)"))

(define-test :ruby "if expressions unparse in ruby nicely"
  (with-new-schema
      (define-entity ("Ent")
          :attributes (:code
                       ("Quantity" :type quantity :default 1 :nullable? nil)
                       ("Rate"    :type money :formula ($divide Amount Quantity))
                       ("Amount" :type money :nullable? nil)
                       ("DoIt" :type text
                        :formula ($if ($>= Rate 100)
                                      ($strcat Rate " is way too much")
                                      ($concatenate "good deal! It will only cost " Amount)))))
    (resolve-attribute-derivations nil)
    (ruby::unparse-expression (expression (find-field :doit :ent))))
  "if ((amount / quantity) >= 100)
  ((amount / quantity).to_s + ' is way too much')
else
  ('good deal! It will only cost ' + amount.to_s)
end")

|#
;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
