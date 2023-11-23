;;;========================================================================================================
;;;
;;;  - methods and functions related to writing non-technical english for simian generators
;;;   
;;;========================================================================================================

(in-package :english)

(defmethod unparse ((obj t) (language (eql :english)))
  (warn "no unparse method written for objects of type ~a" (type-of obj))
  (simian::unparse obj))

(defmethod unparse ((obj string) (language (eql :english)))
  obj)

(defmethod unparse ((obj number) (language (eql :english)))
  obj)
(defmethod unparse ((obj symbol) (language (eql :english)))
  (format nil "~(~a~)" obj))
(defmethod unparse ((obj (eql :current-date)) (language (eql :english)))
  "the current date")

(defmethod unparse-expression ((obj string) (language (eql :english)) &optional args)
  (when args (error "we shouldn't have any args here...? (~a)" args))
  (format nil "~s" obj))

(defmethod unparse ((obj list) (language (eql :english)))
  (if (and (or (typep (car obj) 'entity) (typep (car obj) 'relation))
           (typep (cadr obj) 'attribute))
      (format nil "~a~a" (humanized-append (short-name (car obj)) (long-name (cadr obj)) " ")
              (if (> (length obj) 2) ;; we have where exressions
                  (format nil " where ~a" (unparse-expression (third obj) language))
                  ""))
      (format-english-list (mapcar #'(lambda (item) (unparse item language)) obj))))

(defmethod unparse ((obj attribute) (language (eql :english)))
  (humanized-append (short-name (my-entity obj)) (long-name obj) " "))

(defmethod unparse ((obj entity) (language (eql :english)))
  (short-name obj))

(defmethod unparse ((obj operator) (language (eql :english)))
  (subseq (format nil "~:(~a~)" (operator-key obj)) 1))

(defmethod unparse ((obj entity-state) (language (eql :english)))
  (unparse-expression (expression (predicate obj)) language))

(defmethod unparse-expression ((obj string) (language (eql :english)) &optional args)
  (when args (error "we shouldn't have any args here...? (~a)" args))
  (format nil "~s" obj))

(defmethod unparse-expression ((obj relation) (language (eql :english)) &optional args)
  (format nil "~a from the ~a relation is true" (unparse-expression (car args) language) (name obj)))

(defmethod unparse-expression ((operator (eql :in)) (language (eql :english)) &optional args)
  (format nil "~a is one of ~a" (unparse (car args) language) (unparse-expression (cdr args) language)))
(defmethod unparse-expression ((operator (eql :not-in)) (language (eql :english)) &optional args)
  (format nil "~a is not any of ~a" (unparse (car args) language) (unparse-expression (cdr args) language)))

(defun unparse-two-arg-expression (args conector &optional preamble postscript)
  (format nil "~a~a ~a ~a~a"
          (if preamble (strcat preamble " ") "")
          (unparse-expression (car args) :english) conector (unparse-expression (cadr args) :english)
          (if postscript (strcat " " postscript) "")))

(defun unparse-arg-with-rest-expression (args conector &key preamble postscript)
  (unparse-two-arg-expression
   (list (car args)
         (mapcar #'(lambda (arg)
                     (unparse-expression arg :english))
                 (cdr args)))
   conector preamble postscript))

(defun unparse-and-rest-expression (args &key conector preamble postscript)
  (declare (ignorable conector)) ;; something like $add should be this with "plus" as conector
  (format nil "~a~a~a"
          (if preamble (strcat preamble " ") "")
          (format-english-list (mapcar #'(lambda (arg) (unparse-expression arg :english)) args))
          (if postscript (strcat " " postscript) "")))

(defun unparse-one-arg-expression (args &key preamble postscript)
  (format nil "~a~a~a"
          (if preamble (strcat preamble " ") "")
          (unparse-expression (car args) :english)
          (if postscript (strcat " " postscript) "")))

(defmethod unparse-expression ((operator (eql :add)) (language (eql :english)) &optional args)
  (unparse-two-arg-expression args "plus"))

(defmethod unparse-expression ((operator (eql :multiply)) (language (eql :english)) &optional args)
  (unparse-two-arg-expression args "times"))

(defmethod unparse-expression ((operator (eql :subtract)) (language (eql :english)) &optional args)
  (unparse-two-arg-expression args "minus"))

(defmethod unparse-expression ((operator (eql :divide)) (language (eql :english)) &optional args)
  (unparse-two-arg-expression args "divided by"))

(defmethod unparse-expression ((operator (eql :coalesce)) (language (eql :english)) &optional args)
  (unparse-and-rest-expression args :preamble "the first non-null value out of"))

(defmethod unparse-expression ((operator (eql :max)) (language (eql :english)) &optional args)
  (unparse-and-rest-expression args :preamble "the greatest value out of"))

(defmethod unparse-expression ((operator (eql :min)) (language (eql :english)) &optional args)
  (unparse-and-rest-expression args :preamble "the greatest value out of"))

(defmethod unparse-expression ((operator (eql :regex)) (language (eql :english)) &optional args)
  (unparse-two-arg-expression args "matches the pattern"))

(defmethod unparse-expression ((operator (eql :length)) (language (eql :english)) &optional args)
  (unparse-two-arg-expression args "is" "the length of"))
(defmethod unparse-expression ((operator (eql :length-between)) (language (eql :english)) &optional args)
  (format nil "~a and ~a" (unparse-two-arg-expression args "is between" "the length of") (caddr args)))

(defmethod unparse-expression ((operator (eql :length-gt)) (language (eql :english)) &optional args)
  (unparse-two-arg-expression args "is greater than" "the length of"))
(defmethod unparse-expression ((operator (eql :concatenate)) (language (eql :english)) &optional args)
  (unparse-and-rest-expression args :postscript "joined in a single string"))
(defmethod unparse-expression ((operator (eql :strcat)) (language (eql :english)) &optional args)
  (unparse-and-rest-expression args :postscript "joined in a single string"))
(defmethod unparse-expression ((operator (eql :length-lt)) (language (eql :english)) &optional args)
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

(defmethod unparse-expression ((operator (eql :lt)) (language (eql :english)) &optional args)
  (unparse-two-arg-expression args (choose-size-description operator (data-type (car args)))))
(defmethod unparse-expression ((operator (eql :<=)) (language (eql :english)) &optional args)
  (unparse-two-arg-expression args (choose-size-description operator (data-type (car args)))))
(defmethod unparse-expression ((operator (eql :<)) (language (eql :english)) &optional args)
  (unparse-expression :lt language args))
(defmethod unparse-expression ((operator (eql :gt)) (language (eql :english)) &optional args)
  (unparse-two-arg-expression args (choose-size-description operator (data-type (car args)))))
(defmethod unparse-expression ((operator (eql :>)) (language (eql :english)) &optional args)
  (unparse-expression :gt language args))
(defmethod unparse-expression ((operator (eql :>=)) (language (eql :english)) &optional args)
  (unparse-two-arg-expression args (choose-size-description operator (data-type (car args)))))
(defmethod unparse-expression ((operator (eql :=)) (language (eql :english)) &optional args)
  (unparse-two-arg-expression args "equals"))
(defmethod unparse-expression ((operator (eql :not-eql)) (language (eql :english)) &optional args)
  (unparse-two-arg-expression args "does not equal"))
(defmethod unparse-expression ((operator (eql :eql)) (language (eql :english)) &optional args)
  (unparse-expression := language args))

(defmethod unparse-expression ((operator (eql :or)) (language (eql :english)) &optional args)
  (unparse-and-rest-expression args :preamble "TRUE if any of" :postscript "is TRUE"))


(defmethod unparse-expression ((operator (eql :and)) (language (eql :english)) &optional args)
  (unparse-and-rest-expression args :preamble "TRUE if all of" :postscript "are TRUE"))
;; new definition, old is above for quick reversion
(defmethod unparse-expression ((operator (eql :and)) (language (eql :english)) &optional args)
  (format nil "~{~a~^ AND ~}" (mapcar #'(lambda (arg) (unparse-expression arg language)) args)))

(defmethod unparse-expression ((operator (eql :even)) (language (eql :english)) &optional args)
  (unparse-one-arg-expression args :postscript "is even"))
(defmethod unparse-expression ((operator (eql :odd)) (language (eql :english)) &optional args)
  (unparse-one-arg-expression args :postscript "is odd"))
(defmethod unparse-expression ((operator (eql :function)) (language (eql :english)) &optional args)
  (unparse-arg-with-rest-expression args "with" :preamble "call the function"))

(defmethod unparse-expression ((operator (eql :call)) (language (eql :english)) &optional args)
  (unparse-arg-with-rest-expression args "with" :preamble "call the function"))
(defmethod unparse-expression ((operator (eql :shared-method)) (language (eql :english)) &optional args)
  (unparse-arg-with-rest-expression args "with" :preamble "call the method"))
(defmethod unparse-expression ((operator (eql :method)) (language (eql :english)) &optional args)
  (unparse-arg-with-rest-expression args "with" :preamble "call the method"))
(defmethod unparse-expression ((operator (eql :null)) (language (eql :english)) &optional args)
  (unparse-one-arg-expression args :postscript "has no value"))
(defmethod unparse-expression ((operator (eql :not-null)) (language (eql :english)) &optional args)
  (unparse-one-arg-expression args :postscript "has a value"))
(defmethod unparse-expression ((operator (eql :not-blank)) (language (eql :english)) &optional args)
  (unparse-one-arg-expression args :postscript "is not blank"))
(defmethod unparse-expression ((operator (eql :if)) (language (eql :english)) &optional args)
  (format nil "IF (~a) THEN (~a)~a"
          (unparse-expression (car args) language)
          (unparse-expression (cadr args) language)
          (if (caddr args)
              (format nil " ELSE (~a)" (unparse-expression (caddr args) language))
              "")))
(defun and-expression? (op)
  (typecase op
    (symbol (eql :and op))
    (operator (and-expression? (operator-key op)))))

(defmethod unparse-expression ((operator (eql :when)) (language (eql :english)) &optional args)
  (format nil "WHEN ~a THEN ~a"
          (if (and-expression? (car args))
              (format nil "(~a)" (unparse-expression (car args) language))
              (format nil "~a" (unparse-expression (car args) language)))
          (unparse-expression (cadr args) language)))
(defmethod unparse-expression ((operator (eql :unless)) (language (eql :english)) &optional args)
  (format nil "UNLESS (~a) THEN (~a)"
          (unparse-expression (car args) language)
          (unparse-expression (cadr args) language)))
(defmethod unparse-expression ((operator (eql :not)) (language (eql :english)) &optional args)
  (unparse-one-arg-expression args :postscript "is not true"))

(defmethod unparse-expression ((operator (eql :unique-within)) (language (eql :english)) &optional args)
  (unparse-two-arg-expression args "is unique among records with the same"))

(defmethod unparse-expression ((operator (eql :current-time)) (language (eql :english)) &optional args)
  (declare (ignorable args))
  "the current time")
(defmethod unparse-expression ((operator (eql :current-timestamp)) (language (eql :english)) &optional args)
  (declare (ignorable args))
  "the current time and date")
(defmethod unparse-expression ((operator (eql :as-money)) (language (eql :english)) &optional args)
  (unparse-one-arg-expression args :postscript "written as currency"))
(defmethod unparse-expression ((operator (eql :as-quantity)) (language (eql :english)) &optional args)
  (unparse-one-arg-expression args :postscript "written with commas and rounding"))
(defmethod unparse-expression ((operator (eql :as-date)) (language (eql :english)) &optional args)
  (unparse-one-arg-expression args :postscript "written in a standard date style"))
(defmethod unparse-expression ((operator (eql :current-date)) (language (eql :english)) &optional args)
  (declare (ignorable args))
  "the current date")
(defmethod unparse-expression ((operator (eql :stop-delete)) (language (eql :english)) &optional args)
  (declare (ignorable args))
  "prevent record deletion")
(defmethod unparse-expression ((operator (eql :stop-change)) (language (eql :english)) &optional args)
  (declare (ignorable args))
  "prevent record modification")
(defmethod unparse-expression ((operator (eql :stop-add)) (language (eql :english)) &optional args)
  (declare (ignorable args))
  "prevent record insertion")
(defmethod unparse-expression ((operator (eql :unique)) (language (eql :english)) &optional args)
  (unparse-one-arg-expression args :postscript "has a unique value"))

(defmethod unparse-expression ((operator (eql :unchanged)) (language (eql :english)) &optional args)
  (unparse-one-arg-expression args :postscript "is unchanged"))

(defmethod unparse-expression ((operator (eql :old-value)) (language (eql :english)) &optional args)
  (unparse-one-arg-expression args :preamble "the old value of"))

(defmethod unparse-expression ((operator (eql :new-value)) (language (eql :english)) &optional args)
  (unparse-one-arg-expression args :preamble "the new value of"))


(defmethod unparse-expression ((operator (eql :between)) (language (eql :english)) &optional args)
  (format nil "~a is between ~a and ~a"
          (unparse-expression (car args) language)
          (unparse-expression (cadr args) language)
          (unparse-expression (caddr args) language)))

'(defmethod unparse-expression ((operator (eql :values)) (language (eql :english)) &optional args)
  )

(defmethod unparse-expression ((operator (eql :literal)) (language (eql :english)) &optional args)
  (unparse-one-arg-expression args))
(defmethod unparse-expression ((operator (eql :next-sequence-value)) (language (eql :english)) &optional args)
  (unparse-one-arg-expression args :preamble "the next value provided by the defined sequence"))

'(defmethod unparse-expression ((operator (eql :min-rows)) (language (eql :english)) &optional args)
  (unparse-one-arg-expression args :preamble "has at least"
                              :postscript (cl-inflector::pluralize (car args) "row")))

(defmethod unparse-expression ((operator (eql :max-rows)) (language (eql :english)) &optional args)
  (format nil "the ~a table has no more than ~a ~a~a" (short-name (car args))
          (cadr args) (cl-inflector::pluralize (cadr args) "row")
          (if (= 2 (length args))
              (format nil " where ~a" (unparse-expression (second args) language))
              "")))
(defmethod unparse-expression ((operator (eql :rows)) (language (eql :english)) &optional args)
  (format nil "the number of rows in the ~a table~a" (short-name (car args))
          (if (cadr args)
              (format nil " where ~a" (unparse-expression (cadr args) language))
              "")))

;; check if this is needed at some point
'(defmethod unparse-expression ((operator symbol) (language (eql :english)) &optional args)
  (if (next-method-p)
      (call-next-method)
      (let ((op (get-operator operator)))
        (cond ((= (min-args op) 2 (max-args op))
               (format nil "~a ~a and ~a"
                       (unparse op language)
                       (unparse-expression (car args) language)
                       (unparse-expression (cadr args) language)))
              
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
