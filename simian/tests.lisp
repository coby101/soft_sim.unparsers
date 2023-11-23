;;;===========================================================================
;;;
;;;  tests for unparsers/simian/simian.lisp
;;;
;;;===========================================================================

(in-package :simian-tests)

(define-test simple-unparse-tests
    (:tags '(simple-unparsing))
  (assert-equal "1" (unparse 1 :simian))
  (assert-equal "4.543" (unparse 4.543 :simian))
  (assert-equal "\"one\"" (unparse "one" :simian))
  (assert-equal "symbol" (unparse 'symbol :simian))
  (assert-equal "symbol" (unparse 'SymBol :simian))
  (assert-equal "t" (unparse t :simian))
  (assert-equal "nil" (unparse nil :simian))
  (assert-equal "(1 2 3 (\"a\" b 16))" (unparse '(1 2 3 ("a" b 16)) :simian))
  (let ((obj '(4 5 6 "seven"))) (assert-equal (unparse obj :simian) (unparse-array obj :simian)))
)

(define-test simple-unparse-expression-tests
    (:tags '(simple-unparsing))
  (assert-signal 'error (unparse-expression 1 :simian 2))
  (assert-equal "1" (unparse-expression 1 :simian))
  (assert-equal "4.543" (unparse-expression 4.543 :simian))
  (assert-equal "\"one\"" (unparse-expression "one" :simian))
  (assert-equal "symbol" (unparse-expression 'symbol :simian))
  (assert-equal "symbol" (unparse-expression 'SymBol :simian))
  (assert-equal "(1 2 3 (\"a\" b 16))" (unparse '(1 2 3 ("a" b 16)) :simian))
  (assert-equal "($add 1 1)" (unparse-expression '($add 1 1) :simian))
  (assert-equal "($subtract 1 1)" (unparse-expression '$subtract :simian '(1 1)))
  (assert-equal "($subtract 1 1)" (unparse-expression :subtract :simian '(1 1)))
  (let ((obj '(4 5 6 "seven"))) (assert-equal (unparse-expression obj :simian) (unparse-array obj :simian)))
  )


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
