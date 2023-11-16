;;;===========================================================================
;;; file:   lib/tests/unparsing-ruby.lisp
;;; auth:   Coby Beck
;;; date:   2021-07-07
;;;
;;;---------------------------------------------------------------------------
;;;  tests for code in unparsers/ruby.lisp
;;;  
;;;  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :simian)

(let ((sb-ext:*muffled-warnings*  'style-warning))
  (load-unparser "ruby"))

(define-test :ruby "simple formulas unparse in ruby nicely"
  (with-new-schema
      (define-entity ("Ent")
          :attributes (:code
                       :name
                       ("Quantity" :type quantity :default 1 :nullable? nil)
                       ("Rate"    :type money :formula ($divide Amount Quantity))
                       ("Amount" :type money :nullable? nil)))
    (resolve-attribute-derivations nil)
    (mapcar #'ruby::unparse-expression
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
      (let ((ruby::*include-rails* t))
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


;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
