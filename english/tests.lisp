;;;===========================================================================
;;; file:   lib/tests/unparsing-english.lisp
;;; auth:   Coby Beck
;;; date:   2021-07-07
;;;
;;;---------------------------------------------------------------------------
;;;  tests for code in unparsers/english.lisp
;;;  
;;;  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((sb-ext:*muffled-warnings*  'style-warning))
  (load-unparser "english"))

(in-package :simian)

(define-test :english "simple formulas unparse nicely"
  (with-new-schema
      (define-entity ("Ent") :attributes (:code :name
                                                ("Quantity" :type quantity :default 1 :nullable? nil)
                                                ("Rate"    :type money :formula ($divide Amount Quantity))
                                                ("Amount" :type money :nullable? nil)))
    (resolve-attribute-derivations nil)
    (mapcar #'english::unparse-expression
            (list
             (list '$in (find-field :code :ent) "c1" "c2" "c3")
             (list '$not-in (find-field :code :ent) "c1" "c2" "c3")
             (list '$add (find-field :Quantity :ent) 2)
             (list '$multiply (find-field :Amount :ent) 1.1)
             (list '$lt (find-field :Amount :ent) 1000)
             (list '$as-money (formula (find-field :Rate :ent)))
             (list '$coalesce (find-field :name :ent) "(no name)")
             (list '$strcat (find-field :name :ent) " has the best " (formula (find-field :Rate :ent)))
             (list '$regex (find-field :code :ent) "[A-Z]{3}"))))
  '("Ent Code is one of c1, c2, and c3" "Ent Code is not any of c1, c2, and c3"
    "Ent Quantity plus 2" "Ent Amount times 1.1" "Ent Amount is less than 1000"
    "Ent Amount divided by Ent Quantity written as currency"
    "the first non-null value out of Ent Name and \"(no name)\""
    "Ent Name, \" has the best \", and Ent Amount divided by Ent Quantity joined in a single string"
    "Ent Code matches the pattern \"[A-Z]{3}\""))


;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
