;;;========================================================================================
;;;
;;;  - functions and methods related to unparsing ruby code for simian generators
;;;
;;;========================================================================================

(in-package :ruby)

(defparameter *legal-name-chars*
  (coerce "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_"
          'list)
  "characters not in this list will not be allowed in method or variable names")

(defparameter *ruby-constants* '("true" "false" "nil" "self"))

(defun clean-name (name)
  (let ((chars (coerce name 'list)))
    (coerce (remove-if-not #'(lambda (char)
                               (member char *legal-name-chars*))
                           chars)
            'string)))

;; this will change names not only to be strictly legal but to be simple
(defun make-legal-name (str)
  (if (= 1 (length str))
      (strcat "_" (clean-name str) "_")
      (let* ((last-char (subseq str (1- (length str))))
             (first-char (subseq str 0 1))
             (middle (subseq str 1 (1- (length str))))
             (cleaned (clean-name middle)))
        (strcat 
         (if (find (coerce first-char 'character) "0123456789")
             (strcat "_" first-char)
             first-char)
         cleaned
         (if (member (coerce last-char 'character) (list #\! #\? #\=))
             last-char
             (clean-name last-char))))))

(defun make-indent (&optional (level *nesting-level*))
  (make-string (* 2 level) :initial-element #\Space))

(defun comment-out (stream str &rest args)
  (let ((comment (apply #'format nil str args)))
    (format stream
            (strcat "~a# "
                    (replace-all comment
                                 (format nil "~%")
                                 (format nil "~%~a# " (make-indent)))
                    (format nil "~%"))
            (make-indent))))

(defun indent-block (stream str &rest args)
  (let ((block (apply #'format nil str args)))
    (format stream
            (strcat (make-indent)
                    (replace-all block (format nil "~%") (format nil "~%~a" (make-indent)))))))

(defun comment-with-warning (stream str &rest args)
  (apply #'warn str args)
  (apply #'comment-out stream str args))

(defmethod unparse ((obj t))
  (warn "no unparse method written for objects of type ~a" (type-of obj))
  (simian::unparse obj))

(defmethod unparse-expression ((obj t) &optional args)
  (when args (error "we shouldn't have any args here...? (~a)" args))
  (unparse obj))

(defmethod unparse ((obj string)) (format nil "~s" obj))
(defmethod unparse ((obj float)) (format nil "~f" obj))
(defmethod unparse ((obj integer)) (format nil "~d" obj))

(defun case-sensitive-symbol-name(symbol)
  (let ((symbol-name (symbol-name symbol)))
    (if (equal symbol-name (string-upcase symbol-name))
        (string-downcase symbol-name)
        symbol-name)))

(defmethod unparse ((obj symbol))
  (cond ((eq obj :null) "nil")
        ((eq obj t) "true")
        ((eq obj nil) "nil")
        ((keywordp obj) (format nil ":~a" (snake-case (case-sensitive-symbol-name obj))))
        (t (format nil "~a" (case-sensitive-symbol-name obj)))))

;; this may not be appropriate anymore, much was moved into generators/rails/unparser.lisp
;; I think callers dealing with lists should be using unparse-array. We will know when
;; refactoring is complete and tests are written
(defmethod unparse ((obj list))
  (if (null obj)
      "nil"
      (mapcar #'unparse-expression obj)))

(defmethod unparse ((obj operator))
  (unparse (operator-key obj)))

(defmethod unparse ((obj formula))
  (let ((exp (expression obj)))
    (etypecase exp
      (string exp)
      (list (unparse-expression (car exp) (cdr exp)))
      (attribute (unparse exp)))))

(defun unparse-array (list)
  (format nil "[~{~a~^, ~}]" (mapcar #'unparse list)))

(defun unparse-hash-key (string)
  (format nil ":~(~a~)" string))

(defun hash-data(obj)
  (and obj (listp obj) (every #'listp obj)))

(defun unparse-hash (key-value-pairs &key one-line)
  (let* ((lvl1 (if one-line "" (make-indent)))
         (lvl2 (if one-line "" (with-nesting (format nil "~%~a" (make-indent)))))
         (frmt-str (format nil "~%~a{~a~a~~{~~a~~^,~a~~}~a~a~a}"
                           lvl1 (if one-line " " "") lvl2 lvl2 (if one-line "" #\Newline) lvl1 (if one-line " " ""))))
    (format nil frmt-str
            (mapcar #'(lambda(pair)
                        (with-nesting
                            (let ((key (car pair))
                                  (value (cadr pair)))
                              (format nil "~a =>~a" (unparse-expression key) (if (hash-data value) (unparse-hash value :one-line one-line) (format nil " ~a" (unparse-expression value)))))))
                    key-value-pairs))))

(defun is-range? (str)
  (and (char= #\( (elt str 0))
       (char= #\) (elt str (1- (length str))))
       (search ".." str)))

(defun unparse-range (min max)
  (format nil "(~a..~a)" (unparse min) (unparse max)))

(defmethod reference-field-name ((rel relation))
  (snake-case (strcat (name rel) "_id")))


(defun unparse-method (name args &rest statements)
  (with-output-to-string (code)
    (format code "def ~a~a~%" (snake-case name) (if args (format nil "(~{~a~^, ~})" args) ""))
    (when statements
      (with-nesting
          (let ((fmt-str (format nil "~~a~~{~~a~~^~~%~a~~}" (make-indent))))
            (format code fmt-str (make-indent) (mapcar #'unparse-expression statements)))))
    (format code "~&~aend~%" (make-indent))))

(defun unparse-lambda (arg-list &rest statements)
  (with-output-to-string (code)
    (let ((dec (format nil "-> (~{~a~^, ~}) { " arg-list)))
      (format code dec)
      (when statements
        (let ((fmt-str (format nil "~~{~~a~~^~~%~a~~}"
                                   (let ((*nesting-level* (/ (length dec) 2)))
                                     (make-indent)))))
              (format code fmt-str (mapcar #'unparse-expression statements))))
      (format code " }"))))

 
(defmethod unparse-data ((type t) (value t))
  (format nil "~a" value))

(defmethod unparse-data ((type (eql :float)) (value t))
  (format nil "~a.to_f" value))

(defmethod unparse-data ((type (eql :date)) (value t))
  (format nil "Date.parse(~s)" value))

(defmethod unparse-data ((type (eql :datetime)) (value t))
  (format nil "DateTime.parse(~s)" value))

(defmethod unparse-data ((type (eql :string)) (value t))
  (format nil "~s" value))

(defmethod unparse-data ((type (eql :boolean)) (value t))
  (format nil "~a" (if value "true" "false")))

(defmethod unparse-data ((type (eql :record)) (value t))
  (unparse-array value))

(defmethod unparse-expression ((obj t) &optional args)
  (when args (error "we shouldn't have any args here...? (~a)" args))
  (unparse obj))

(defmethod unparse-expression ((obj string) &optional args)
  (when args (error "we shouldn't have any args here...? (~a)" args))
  (format nil "'~a'" (escape-characters obj #\')))

(defmethod unparse-expression ((obj list) &optional args)
  (if (or (typep (car obj) 'operator) (operator-symbol? (car obj)))
      (progn
        (when args (error "we shouldn't have any args here...? (~a)" args))
        (unparse-expression (car obj) (cdr obj)))
      (if (null obj)
          "nil"
          (unparse-array obj))))
#|

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
(unparse obj))))|#

(defmethod unparse-expression ((operator symbol) &optional args)
  (if (operator-symbol? operator)
      (progn
        (warn "no ruby::unparse-expression method written for (~a ~a)" operator args)
        (format nil "(~:(~a~) ~{~a~^ ~})" operator (mapcar #'unparse args)))
      (call-next-method)))

(defmethod unparse-expression ((operator operator) &optional args)
  (unparse-expression (operator-key operator) args))

(defun negate-expression (exp)
  (if (atom exp)
      (list (get-operator '$not) exp)
      (let ((operator (operator-key (car exp))))
        (case operator
          ($not (cadr exp))
          ($null (list (get-operator '$not-null) (cadr exp)))
          ($not-null (list (get-operator '$null) (cadr exp)))
          (($eql $=) (list (get-operator '$not-eql) (cadr exp) (caddr exp)))
          (($not-eql $!=) (list (get-operator '$eql) (cadr exp) (caddr exp)))
          (($> $gt) (list (get-operator '$<=) (cadr exp) (caddr exp)))
          (($< $lt) (list (get-operator '$>=) (cadr exp) (caddr exp)))
          ($<= (list (get-operator '$>) (cadr exp) (caddr exp)))
          ($>= (list (get-operator '$<) (cadr exp) (caddr exp)))
          ($and (list* (get-operator '$or) (mapcar #'negate-expression (cdr exp))))
          ($or (list* (get-operator '$and) (mapcar #'negate-expression (cdr exp))))
          (otherwise (list (get-operator '$not) exp))))))

(defmethod unparse-expression ((operator (eql '$call)) &optional args)
  (format nil "~a~a"
          (first args)
          (if (cdr args)
              (format nil "(~{~a~^, ~})" (mapcar #'unparse-expression (cdr args)))
              "")))

(defmethod unparse-expression ((operator (eql '$literal)) &optional args)
  (format nil "~a" (first args)))

(defmethod unparse-expression ((operator (eql '$between)) &optional args)
  (format nil "~a.between?(~a, ~a)" (unparse-expression (first args))
          (unparse-expression (second args)) (unparse-expression (third args))))

(defmethod unparse-expression ((operator (eql '$length-between)) &optional args)
  (format nil "~a.length.between?(~a, ~a)" (unparse-expression (first args))
          (unparse-expression (second args)) (unparse-expression (third args))))

(defmethod unparse-expression ((operator (eql '$gt)) &optional args)
  (unparse-expression '$> args))
(defmethod unparse-expression ((operator (eql '$>)) &optional args)
  (format nil "(~a > ~a)" (unparse-expression (car args)) (unparse-expression (cadr args))))
(defmethod unparse-expression ((operator (eql '$>=)) &optional args)
  (format nil "(~a >= ~a)" (unparse-expression (car args)) (unparse-expression (cadr args))))
  
(defmethod unparse-expression ((operator (eql '$lt)) &optional args)
  (unparse-expression '$< args))
(defmethod unparse-expression ((operator (eql '$<)) &optional args)
  (format nil "(~a < ~a)" (unparse-expression (car args)) (unparse-expression (cadr args))))
(defmethod unparse-expression ((operator (eql '$<=)) &optional args)
  (format nil "(~a <= ~a)" (unparse-expression (car args)) (unparse-expression (cadr args))))

(defmethod unparse-expression ((operator (eql '$not-null)) &optional args)
  (let ((field-var (unparse-expression (first args))))
    (format nil "!(~a.nil?~a" field-var
            (if (eql :string (data-type (car args)))
                (format nil ") && !(~a.empty?)" field-var)
                ")"))))

(defmethod unparse-expression ((operator (eql '$null)) &optional args)
  (let ((field-var (unparse-expression (first args))))
    (format nil "(~a.nil?~a" field-var
            (if (eql :string (data-type (car args)))
                (format nil " && ~a.empty?)" field-var)
                ")"))))

(defmethod unparse-expression ((operator (eql '$!=)) &optional args)
  (unparse-expression '$not-eql args))
(defmethod unparse-expression ((operator (eql '$not-eql)) &optional args)
  (format nil "(~a != ~a)" (unparse-expression (car args))
          (unparse-expression (cadr args))))

(defmethod unparse-expression ((operator (eql '$not)) &optional args)
  (format nil "!(~a)" (unparse-expression (car args))))

(defmethod unparse-expression ((operator (eql '$=)) &optional args)
  (unparse-expression '$eql args))
(defmethod unparse-expression ((operator (eql '$eql)) &optional args)
  (format nil "(~a == ~a)" (unparse-expression (car args))
          (unparse-expression (cadr args))))

(defmethod unparse-expression ((operator (eql '$regex)) &optional args)
  (format nil "~a =~~ /~a/" (unparse-expression (car args)) (cadr args)))

(defmethod unparse-expression ((operator (eql '$length-between)) &optional args)
  (unparse-expression '$between
     (list (list '$literal
                 (format nil "~a.length" (unparse-expression (car args))))
           (unparse-expression (cadr args))
           (unparse-expression (caddr args)))))

(defmethod unparse-expression ((operator (eql '$length)) &optional args)
  (unparse-expression '$eql
     (list (list '$literal
                 (format nil "~a.length" (unparse-expression (car args))))
           (unparse-expression (cadr args)))))

(defmethod unparse-expression ((operator (eql '$length-gt)) &optional args)
  (unparse-expression '$gt
     (list (list '$literal
                 (format nil "~a.length" (unparse-expression (car args))))
           (unparse-expression (cadr args)))))

(defmethod unparse-expression ((operator (eql '$length-lt)) &optional args)
  (unparse-expression '$lt
     (list (list '$literal
                 (format nil "~a.length" (unparse-expression (car args))))
           (unparse-expression (cadr args)))))

(defmethod unparse-expression ((operator (eql '$divide)) &optional args)
  (format nil "(~{~a~^ / ~})" (mapcar #'unparse-expression args)))

(defmethod unparse-expression ((operator (eql '$multiply)) &optional args)
  (format nil "(~{~a~^ * ~})" (mapcar #'unparse-expression args)))

(defmethod unparse-expression ((operator (eql '$add)) &optional args)
  (format nil "(~{~a~^ + ~})" (mapcar #'unparse-expression args)))

(defmethod unparse-expression ((operator (eql '$and)) &optional args)
  (format nil "(~{~a~^ && ~})" (mapcar #'unparse-expression args)))

(defmethod unparse-expression ((operator (eql '$coalesce)) &optional args)
  (unparse-expression '$or args))
(defmethod unparse-expression ((operator (eql '$or)) &optional args)
  (format nil "(~{~a~^ || ~})" (mapcar #'unparse-expression args)))

(defmethod unparse-expression ((operator (eql '$subtract)) &optional args)
  (format nil "(~{~a~^ - ~})" (mapcar #'unparse-expression args)))

(defmethod unparse-expression ((obj (eql '$current-date)) &optional args)
  (unless (null args) (error "$current-date does not take any arguments"))
  "Date.today")

(defmethod unparse-expression ((obj (eql '$interval)) &optional args)
  (let ((interval-types '("hours" "months" "days" "years" "weeks")))
    (unless (or (integerp (car args))
                (eql (data-type (car args)) :integer))
      (error "$interval requires an integer as its first argument"))
    (unless (member (cadr args) interval-types :test #'string-equal)
      (error "$interval requires a type designator from this list: ~a" interval-types))
    (format nil "~a.~a" (car args) (cadr args))))

(defun unparse-if-statement (test consequent &optional alternate)
  (unparse-expression '$if (list test consequent alternate)))
(defmethod unparse-expression ((operator (eql '$if)) &optional args)
  (let ((lvl-1-indent (make-indent))
        (test (unparse-expression (car args)))
        (consequent (format nil "~a" (unparse-expression (cadr args))))
        (alternative (when (third args) (format nil "~a" (unparse-expression (third args))))))
    (with-nesting
      (format nil "if ~a~%~a~%~a~aend" test
              (indent-block nil consequent)
              (if alternative
                  (format nil "~aelse~%~a~%" lvl-1-indent (indent-block nil alternative))
                  "")
              lvl-1-indent))))


(defmethod unparse-expression ((operator (eql '$unless)) &optional args)
  (let ((test (unparse-expression (car args)))
        (consequent (format nil "~a" (unparse-expression (cadr args)))))
    (format nil "~a unless ~a" consequent test)))

(defmethod unparse-expression ((operator (eql '$in)) &optional args)
  (format nil "~a.include? ~a" (unparse-array (cdr args)) (unparse-expression (car args))))

(defmethod unparse-expression ((operator (eql '$not-in)) &optional args)
  (format nil "~a.exclude? ~a" (unparse-array (cdr args)) (unparse-expression (car args))))

(defmethod unparse-expression ((operator (eql '$as-date)) &optional args)
  (format nil "~a.strftime(\"%d %b, %Y\")" (unparse-expression (car args))))

(defmethod unparse-expression ((operator (eql '$as-quantity)) &optional args)
  (format nil "~a.to_s.reverse.gsub(/(\\d{3})(?=\\d)/, '\\\\1,').reverse"
          (unparse-expression (car args))))

(defmethod unparse-expression ((operator (eql '$concatenate)) &optional args)
  (unparse-expression '$strcat args))
(defmethod unparse-expression ((operator (eql '$strcat)) &optional args)
  (format nil "(~{~a~^ + ~})" (mapcar #'(lambda(exp)
                                          (unparse-expression (list '$to-string exp)))
                                      args)))
(defmethod unparse-expression ((operator (eql '$to-string)) &optional args)
  (if (returns-string? (car args))
      (unparse-expression (car args))
      (format nil "~a.to_s" (unparse-expression (car args)))))

(defmethod unparse-formatting ((data t) (type t))
  (format nil "~a.to_s" data))

(defmethod unparse-formatting ((data string) (type t))
  data)

(defmethod unparse-formatting ((data t) (type logical-type))
  (unparse-formatting data (id type)))

(defmethod unparse-datatype ((type logical-type))
  (or (unparse-datatype (id type))
      (unparse-datatype (data-type type))))

(defmethod unparse-datatype ((type symbol))
  nil)
;; https://stackoverflow.com/questions/11889048/is-there-documentation-for-the-rails-column-types
(defmethod unparse-datatype ((sym (eql :string)))
  "string")
(defmethod unparse-datatype ((sym (eql :memo)))
  "text")
(defmethod unparse-datatype ((sym (eql :long-text)))
  "text")
(defmethod unparse-datatype ((sym (eql :short-text)))
  "string")
(defmethod unparse-datatype ((sym (eql :label)))
  "string")
(defmethod unparse-datatype ((sym (eql :name)))
  "string")
(defmethod unparse-datatype ((sym (eql :code)))
  "string")
(defmethod unparse-datatype ((sym (eql :datetime)))
  "timestamp")
(defmethod unparse-datatype ((sym (eql :integer)))
  "integer")
(defmethod unparse-datatype ((sym (eql :date)))
  "date")
(defmethod unparse-datatype ((sym (eql :boolean)))
  "boolean")
(defmethod unparse-datatype ((sym (eql :float)))
  "decimal")
(defmethod unparse-datatype ((sym (eql :money)))
  "decimal")
(defmethod unparse-datatype ((sym (eql :sequence)))
  "integer")
(defmethod unparse-datatype ((sym (eql :float)))
  "decimal")
(defmethod unparse-datatype ((sym (eql :float)))
  "decimal")
(defmethod unparse-datatype ((sym (eql :float)))
  "decimal")

;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
