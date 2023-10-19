;;;========================================================================================================
;;;
;;;  - methods and functions related to writing non-technical english for simian generators
;;;   
;;;========================================================================================================

(in-package :english)

(defmethod unparse ((obj t))
  (warn "no unparse method written for objects of type ~a" (type-of obj))
  (simian::unparse obj))

(defmethod unparse ((obj string))
  obj)

(defmethod unparse ((obj number))
  obj)
(defmethod unparse ((obj symbol))
  (format nil "~(~a~)" obj))
(defmethod unparse ((obj (eql '$current-date)))
  "the current date")

(defmethod unparse-expression ((obj string) &optional args)
  (when args (error "we shouldn't have any args here...? (~a)" args))
  (format nil "~s" obj))

(defmethod unparse ((obj list))
  (if (and (or (typep (car obj) 'entity) (typep (car obj) 'relation))
           (typep (cadr obj) 'attribute))
      (format nil "~a~a" (humanized-append (short-name (car obj)) (long-name (cadr obj)) " ")
              (if (> (length obj) 2) ;; we have where exressions
                  (format nil " where ~a" (unparse-expression (third obj)))
                  ""))
      (format-english-list (mapcar #'unparse obj))))

(defmethod unparse ((obj attribute))
  (humanized-append (short-name (my-entity obj)) (long-name obj) " "))

(defmethod unparse ((obj entity))
  (short-name obj))

(defmethod unparse ((obj operator))
  (subseq (format nil "~:(~a~)" (operator-key obj)) 1))

(defmethod unparse ((obj constraint))
  (unparse (formula obj)))

(defmethod unparse ((obj entity-state))
  (unparse-expression (expression (predicate obj))))

(defmethod unparse ((obj formula))
  (let ((exp (expression obj)))
    (etypecase exp
      (string exp)
      (list (unparse-expression (car exp) (cdr exp)))
      (attribute (unparse exp)))))


(defmethod unparse-expression ((obj t) &optional args)
  (when args (error "we shouldn't have any args here...? (~a)" args))
  (unparse obj))

(defmethod unparse-expression ((obj string) &optional args)
  (when args (error "we shouldn't have any args here...? (~a)" args))
  (format nil "~s" obj))

(defmethod unparse-expression ((obj list) &optional args)
  (if (or (typep (car obj) 'operator) (operator-symbol? (car obj)))
      (progn
        (when args (error "we shouldn't have any args here...? (~a)" args))
        (unparse-expression (car obj) (cdr obj)))
      (unparse obj)))

(defmethod unparse-expression ((obj relation) &optional args)
  (format nil "~a from the ~a relation is true" (unparse-expression (car args)) (name obj)))

(defmethod unparse-expression ((operator symbol) &optional args)
  (if (operator-symbol? operator)
      (progn
        (warn "no english::unparse-expression method written for ~a" operator)
        (format nil "(~:(~a~) ~{~a~^ ~})" operator (mapcar #'unparse args)))
      (call-next-method)))

(defmethod unparse-expression ((operator operator) &optional args)
  (unparse-expression (operator-key operator) args))

(defmethod unparse-expression ((operator (eql '$IN)) &optional args)
  (format nil "~a is one of ~a" (unparse (car args)) (unparse-expression (cdr args))))
(defmethod unparse-expression ((operator (eql '$NOT-IN)) &optional args)
  (format nil "~a is not any of ~a" (unparse (car args)) (unparse-expression (cdr args))))

(defun unparse-two-arg-expression (args conector &optional preamble postscript)
  (format nil "~a~a ~a ~a~a"
          (if preamble (strcat preamble " ") "")
          (unparse-expression (car args)) conector (unparse-expression (cadr args))
          (if postscript (strcat " " postscript) "")))

(defun unparse-arg-with-rest-expression (args conector &key preamble postscript)
(unparse-two-arg-expression
 (list (car args)
       (mapcar #'unparse-expression
               (cdr args))) conector preamble postscript))

(defun unparse-and-rest-expression (args &key conector preamble postscript)
  (declare (ignorable conector)) ;; something like $add should be this with "plus" as conector
  (format nil "~a~a~a"
          (if preamble (strcat preamble " ") "")
          (format-english-list (mapcar #'unparse-expression args))
          (if postscript (strcat " " postscript) "")))

(defun unparse-one-arg-expression (args &key preamble postscript)
  (format nil "~a~a~a"
          (if preamble (strcat preamble " ") "")
          (unparse-expression (car args))
          (if postscript (strcat " " postscript) "")))

(defmethod unparse-expression ((operator (eql '$add)) &optional args)
  (unparse-two-arg-expression args "plus"))

(defmethod unparse-expression ((operator (eql '$multiply)) &optional args)
  (unparse-two-arg-expression args "times"))

(defmethod unparse-expression ((operator (eql '$subtract)) &optional args)
  (unparse-two-arg-expression args "minus"))

(defmethod unparse-expression ((operator (eql '$divide)) &optional args)
  (unparse-two-arg-expression args "divided by"))

(defmethod unparse-expression ((operator (eql '$coalesce)) &optional args)
  (unparse-and-rest-expression args :preamble "the first non-null value out of"))

(defmethod unparse-expression ((operator (eql '$max)) &optional args)
  (unparse-and-rest-expression args :preamble "the greatest value out of"))

(defmethod unparse-expression ((operator (eql '$min)) &optional args)
  (unparse-and-rest-expression args :preamble "the greatest value out of"))

(defmethod unparse-expression ((operator (eql '$regex)) &optional args)
  (unparse-two-arg-expression args "matches the pattern"))

(defmethod unparse-expression ((operator (eql '$length)) &optional args)
  (unparse-two-arg-expression args "is" "the length of"))
(defmethod unparse-expression ((operator (eql '$length-between)) &optional args)
  (format nil "~a and ~a" (unparse-two-arg-expression args "is between" "the length of") (caddr args)))

(defmethod unparse-expression ((operator (eql '$length-gt)) &optional args)
  (unparse-two-arg-expression args "is greater than" "the length of"))
(defmethod unparse-expression ((operator (eql '$concatenate)) &optional args)
  (unparse-and-rest-expression args :postscript "joined in a single string"))
(defmethod unparse-expression ((operator (eql '$strcat)) &optional args)
  (unparse-and-rest-expression args :postscript "joined in a single string"))
(defmethod unparse-expression ((operator (eql '$length-lt)) &optional args)
  (unparse-two-arg-expression args "is less than" "the length of"))

(defun choose-size-description (op datatype)
  (if (member datatype '(:datetime :date))
      (case op
        (($gt $>) "is later than")
        (($lt $<) "is earlier than")
        ($>= "isn't earlier than")
        ($<= "isn't later than"))
      (case op
        (($gt $>) "is greater than")
        (($lt $<) "is less than")
        ($>= "is greater than or equal to")
        ($<= "is less than or equal to"))))

(defmethod unparse-expression ((operator (eql '$lt)) &optional args)
  (unparse-two-arg-expression args (choose-size-description operator (data-type (car args)))))
(defmethod unparse-expression ((operator (eql '$<=)) &optional args)
  (unparse-two-arg-expression args (choose-size-description operator (data-type (car args)))))
(defmethod unparse-expression ((operator (eql '$<)) &optional args)
  (unparse-expression '$lt args))
(defmethod unparse-expression ((operator (eql '$gt)) &optional args)
  (unparse-two-arg-expression args (choose-size-description operator (data-type (car args)))))
(defmethod unparse-expression ((operator (eql '$>)) &optional args)
  (unparse-expression '$gt args))
(defmethod unparse-expression ((operator (eql '$>=)) &optional args)
  (unparse-two-arg-expression args (choose-size-description operator (data-type (car args)))))
(defmethod unparse-expression ((operator (eql '$=)) &optional args)
  (unparse-two-arg-expression args "equals"))
(defmethod unparse-expression ((operator (eql '$not-eql)) &optional args)
  (unparse-two-arg-expression args "does not equal"))
(defmethod unparse-expression ((operator (eql '$eql)) &optional args)
  (unparse-expression '$= args))

(defmethod unparse-expression ((operator (eql '$or)) &optional args)
  (unparse-and-rest-expression args :preamble "TRUE if any of" :postscript "is TRUE"))


(defmethod unparse-expression ((operator (eql '$and)) &optional args)
  (unparse-and-rest-expression args :preamble "TRUE if all of" :postscript "are TRUE"))
;; new definition, old is above for quick reversion
(defmethod unparse-expression ((operator (eql '$and)) &optional args)
  (format nil "~{~a~^ AND ~}" (mapcar #'unparse-expression args)))

(defmethod unparse-expression ((operator (eql '$even)) &optional args)
  (unparse-one-arg-expression args :postscript "is even"))
(defmethod unparse-expression ((operator (eql '$odd)) &optional args)
  (unparse-one-arg-expression args :postscript "is odd"))
(defmethod unparse-expression ((operator (eql '$function)) &optional args)
  (unparse-arg-with-rest-expression args "with" :preamble "call the function"))

(defmethod unparse-expression ((operator (eql '$call)) &optional args)
  (unparse-arg-with-rest-expression args "with" :preamble "call the function"))
(defmethod unparse-expression ((operator (eql '$shared-method)) &optional args)
  (unparse-arg-with-rest-expression args "with" :preamble "call the method"))
(defmethod unparse-expression ((operator (eql '$method)) &optional args)
  (unparse-arg-with-rest-expression args "with" :preamble "call the method"))
(defmethod unparse-expression ((operator (eql '$null)) &optional args)
  (unparse-one-arg-expression args :postscript "has no value"))
(defmethod unparse-expression ((operator (eql '$not-null)) &optional args)
  (unparse-one-arg-expression args :postscript "has a value"))
(defmethod unparse-expression ((operator (eql '$not-blank)) &optional args)
  (unparse-one-arg-expression args :postscript "is not blank"))
(defmethod unparse-expression ((operator (eql '$if)) &optional args)
  (format nil "IF (~a) THEN (~a)~a"
          (unparse-expression (car args))
          (unparse-expression (cadr args))
          (if (caddr args)
              (format nil " ELSE (~a)" (unparse-expression (caddr args)))
              "")))
(defun and-expression? (op)
  (typecase op
    (symbol (eql '$and op))
    (operator (and-expression? (operator-key op)))))

(defmethod unparse-expression ((operator (eql '$when)) &optional args)
  (format nil "WHEN ~a THEN ~a"
          (if (and-expression? (car args))
              (format nil "(~a)" (unparse-expression (car args)))
              (format nil "~a" (unparse-expression (car args))))
          (unparse-expression (cadr args))))
(defmethod unparse-expression ((operator (eql '$unless)) &optional args)
  (format nil "UNLESS (~a) THEN (~a)"
          (unparse-expression (car args))
          (unparse-expression (cadr args))))
(defmethod unparse-expression ((operator (eql '$not)) &optional args)
  (unparse-one-arg-expression args :postscript "is not true"))

(defmethod unparse-expression ((operator (eql '$unique-within)) &optional args)
  (unparse-two-arg-expression args "is unique among records with the same"))

(defmethod unparse-expression ((operator (eql '$current-time)) &optional args)
  (declare (ignorable args))
  "the current time")
(defmethod unparse-expression ((operator (eql '$current-timestamp)) &optional args)
  (declare (ignorable args))
  "the current time and date")
(defmethod unparse-expression ((operator (eql '$as-money)) &optional args)
  (unparse-one-arg-expression args :postscript "written as currency"))
(defmethod unparse-expression ((operator (eql '$as-quantity)) &optional args)
  (unparse-one-arg-expression args :postscript "written with commas and rounding"))
(defmethod unparse-expression ((operator (eql '$as-date)) &optional args)
  (unparse-one-arg-expression args :postscript "written in a standard date style"))
(defmethod unparse-expression ((operator (eql '$current-date)) &optional args)
  (declare (ignorable args))
  "the current date")
(defmethod unparse-expression ((operator (eql '$stop-delete)) &optional args)
  (declare (ignorable args))
  "prevent record deletion")
(defmethod unparse-expression ((operator (eql '$stop-change)) &optional args)
  (declare (ignorable args))
  "prevent record modification")
(defmethod unparse-expression ((operator (eql '$stop-add)) &optional args)
  (declare (ignorable args))
  "prevent record insertion")
(defmethod unparse-expression ((operator (eql '$unique)) &optional args)
  (unparse-one-arg-expression args :postscript "has a unique value"))

(defmethod unparse-expression ((operator (eql '$unchanged)) &optional args)
  (unparse-one-arg-expression args :postscript "is unchanged"))

(defmethod unparse-expression ((operator (eql '$old-value)) &optional args)
  (unparse-one-arg-expression args :preamble "the old value of"))

(defmethod unparse-expression ((operator (eql '$new-value)) &optional args)
  (unparse-one-arg-expression args :preamble "the new value of"))


(defmethod unparse-expression ((operator (eql '$between)) &optional args)
  (format nil "~a is between ~a and ~a"
          (unparse-expression (car args))
          (unparse-expression (cadr args))
          (unparse-expression (caddr args))))

'(defmethod unparse-expression ((operator (eql '$values)) &optional args)
  )

(defmethod unparse-expression ((operator (eql '$literal)) &optional args)
  (unparse-one-arg-expression args))
(defmethod unparse-expression ((operator (eql '$next-sequence-value)) &optional args)
  (unparse-one-arg-expression args :preamble "the next value provided by the defined sequence"))

'(defmethod unparse-expression ((operator (eql '$min-rows)) &optional args)
  (unparse-one-arg-expression args :preamble "has at least"
                              :postscript (cl-inflector::pluralize (car args) "row")))

(defmethod unparse-expression ((operator (eql '$max-rows)) &optional args)
  (format nil "the ~a table has no more than ~a ~a~a" (short-name (car args))
          (cadr args) (cl-inflector::pluralize (cadr args) "row")
          (if (= 2 (length args))
              (format nil " where ~a" (unparse-expression (second args)))
              "")))
(defmethod unparse-expression ((operator (eql '$rows)) &optional args)
  (format nil "the number of rows in the ~a table~a" (short-name (car args))
          (if (cadr args)
              (format nil " where ~a" (unparse-expression (cadr args)))
              "")))

;; check if this is needed at some point
'(defmethod unparse-expression ((operator symbol) &optional args)
  (if (next-method-p)
      (call-next-method)
      (let ((op (get-operator operator)))
        (cond ((= (min-args op) 2 (max-args op))
               (format nil "~a ~a and ~a"
                       (unparse op)
                       (unparse-expression (car args))
                       (unparse-expression (cadr args))))
              
))))

(defun with-article (word)
  (let ((stripped (remove #\" word)))
    (if (> (length stripped) 0)
        (if (member (elt stripped 0) '(#\a #\e #\i #\o #\A #\E #\I #\O))
            (strcat "an " word)
            (strcat "a " word))
        word)))

(defun designation-with-article (obj slot &optional (quote t))
  (let ((q-mark (if quote "\"" "")))
    (with-article
        (strcat q-mark (ignore-errors (funcall slot obj)) q-mark))))


;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
