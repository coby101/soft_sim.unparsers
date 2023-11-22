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

(defun case-sensitive-symbol-name(symbol)
  (let ((symbol-name (symbol-name symbol)))
    (if (equal symbol-name (string-upcase symbol-name))
        (string-downcase symbol-name)
        symbol-name)))

(defmethod unparse ((obj symbol) (language (eql :ruby)))
  (cond ((eq obj :null) "nil")
        ((eq obj t) "true")
        ((eq obj nil) "nil")
        ((keywordp obj) (format nil ":~a" (snake-case (case-sensitive-symbol-name obj))))
        (t (format nil "~a" (case-sensitive-symbol-name obj)))))

; seems wrong headed
;(defmethod unparse ((obj operator) (language (eql :ruby)))
;  (unparse (operator-key obj) language))

; I don't think this should be necessary, delete after refactor
(defmethod unparse ((obj formula) (language (eql :ruby)))
  (let ((exp (expression obj)))
    (etypecase exp
      (string exp)
      (list (unparse-expression (car exp) language (cdr exp)))
      (attribute (unparse exp language)))))

(defmethod unparse-array ((list list) (language (eql :ruby)))
  (format nil "[~{~a~^, ~}]" (mapcar #'(lambda (elt) (unparse elt :ruby)) list)))

(defmethod unparse ((obj list) (language (eql :ruby)))
  (unparse-array obj :ruby))

(defmethod unparse ((obj string) (language (eql :ruby)))
  (format nil "'~a'" obj))

(defun unparse-hash-key (string)
  (format nil ":~(~a~)" string))

(defun hash-data(obj)
  (and obj (listp obj) (every #'listp obj)))

(defun unparse-hash (key-value-pairs &key one-line)
  (let* ((lvl1 (if one-line "" (make-indent)))
         (lvl2 (if one-line " " (with-nesting (format nil "~%~a" (make-indent)))))
         (new-hash-start (or (and one-line lvl2) #\Newline))
         (frmt-str (format nil "~a{~a~a~~{~~a~~^,~a~~}~a~a~a}"
                           lvl1 (if one-line "" "") lvl2 lvl2 (if one-line "" #\Newline) lvl1 (if one-line " " ""))))
    (format nil frmt-str
            (mapcar #'(lambda(pair)
                        (with-nesting
                            (let ((key (car pair))
                                  (value (cadr pair)))
                              (format nil "~a =>~a" (unparse-hash-key key) 
                                      (if (hash-data value) (format nil "~a~a" new-hash-start (unparse-hash value :one-line one-line)) (format nil " ~a" (unparse value :ruby)))))))
                    key-value-pairs))))

;(unparse-hash '((:a 1) (:b 2) (:c ((:d 4) (:e 5)))))
;(unparse-hash '((:a 1) (:b 2) (:c ((:d 4) (:e 5)))) :one-line t)

(defun is-range? (str)
  (and (char= #\( (elt str 0))
       (char= #\) (elt str (1- (length str))))
       (search ".." str)))

(defun unparse-range (min max)
  (format nil "(~a..~a)" (unparse min :ruby) (unparse max :ruby)))

(defun unparse-method (name args &rest statements)
  (with-output-to-string (code)
    (format code "def ~a~a~%" (snake-case name) (if args (format nil "(~{~a~^, ~})" args) ""))
    (when statements
      (with-nesting
          (let ((fmt-str (format nil "~~a~~{~~a~~^~~%~a~~}" (make-indent))))
            (format code fmt-str (make-indent) (mapcar #'(lambda (statement) (unparse-expression statement :ruby)) statements)))))
    (format code "~&~aend~%" (make-indent))))

(defun unparse-lambda (arg-list &rest statements)
  (with-output-to-string (code)
    (let ((dec (format nil "-> (~{~a~^, ~}) { " arg-list)))
      (format code dec)
      (when statements
        (let ((fmt-str (format nil "~~{~~a~~^~~%~a~~}"
                                   (let ((*nesting-level* (/ (length dec) 2)))
                                     (make-indent)))))
              (format code fmt-str (mapcar #'(lambda (statement) (unparse-expression statement :ruby)) statements))))
      (format code " }"))))

(defmethod unparse-data ((type t)               (value t)) (format nil "~a" value))
(defmethod unparse-data ((type (eql :float))    (value t)) (format nil "~a.to_f" value))
(defmethod unparse-data ((type (eql :date))     (value t)) (format nil "Date.parse(~s)" value))
(defmethod unparse-data ((type (eql :datetime)) (value t)) (format nil "DateTime.parse(~s)" value))
(defmethod unparse-data ((type (eql :string))   (value t)) (format nil "~s" value))
(defmethod unparse-data ((type (eql :boolean))  (value t)) (format nil "~a" (if value "true" "false")))
(defmethod unparse-data ((type (eql :record))   (value t)) (unparse-array value :ruby))

(defmethod unparse-expression ((obj string) (language (eql :ruby)) &optional args)
  (when args (error "we shouldn't have any args here...? (~a)" args))
  (format nil "'~a'" (escape-characters obj #\')))

(defmethod unparse-expression ((operator (eql :call)) (language (eql :ruby)) &optional args)
  (format nil "~a~a" (first args)
              (if (cdr args)
                  (format nil "(~{~a~^, ~})" (mapcar #'(lambda (arg) (unparse-expression arg language)) (cdr args)))
                  "")))

(defmethod unparse-expression ((operator (eql :method)) (language (eql :ruby)) &optional args)
  (format nil "~a.~a" (unparse-expression (second args) language) (first args)))

(defmethod unparse-expression ((operator (eql :between)) (language (eql :ruby)) &optional args)
  (format nil "~a.between?(~a, ~a)" (unparse-expression (first args) language)
          (unparse-expression (second args) language) (unparse-expression (third args) language)))

(defmethod unparse-expression ((operator (eql :length-between)) (language (eql :ruby)) &optional args)
  (format nil "~a.length.between?(~a, ~a)" (unparse-expression (first args) language)
          (unparse-expression (second args) language) (unparse-expression (third args) language)))

(defmethod unparse-expression ((operator (eql :gt)) (language (eql :ruby)) &optional args)
  (unparse-expression :> language args))
(defmethod unparse-expression ((operator (eql :>)) (language (eql :ruby)) &optional args)
  (format nil "(~a > ~a)" (unparse-expression (car args) language) (unparse-expression (cadr args) language)))
(defmethod unparse-expression ((operator (eql :>=)) (language (eql :ruby)) &optional args)
  (format nil "(~a >= ~a)" (unparse-expression (car args) language) (unparse-expression (cadr args) language)))
  
(defmethod unparse-expression ((operator (eql :lt)) (language (eql :ruby)) &optional args)
  (unparse-expression :< language args))
(defmethod unparse-expression ((operator (eql :<)) (language (eql :ruby)) &optional args)
  (format nil "(~a < ~a)" (unparse-expression (car args) language) (unparse-expression (cadr args) language)))
(defmethod unparse-expression ((operator (eql :<=)) (language (eql :ruby)) &optional args)
  (format nil "(~a <= ~a)" (unparse-expression (car args) language) (unparse-expression (cadr args) language)))

(defmethod unparse-expression ((operator (eql :not-null)) (language (eql :ruby)) &optional args)
  (let ((field-var (unparse-expression (first args) language)))
    (format nil "!(~a.nil?~a" field-var
            (if (eql :string (data-type (car args)))
                (format nil ") && !(~a.empty?)" field-var)
                ")"))))

(defmethod unparse-expression ((operator (eql :null)) (language (eql :ruby)) &optional args)
  (let ((field-var (unparse-expression (first args) language)))
    (format nil "(~a.nil?~a" field-var
            (if (eql :string (data-type (car args)))
                (format nil " && ~a.empty?)" field-var)
                ")"))))

(defmethod unparse-expression ((operator (eql :!=)) (language (eql :ruby)) &optional args)
  (unparse-expression :not-eql language args))
(defmethod unparse-expression ((operator (eql :not-eql)) (language (eql :ruby)) &optional args)
  (format nil "(~a != ~a)" (unparse-expression (car args) language)
          (unparse-expression (cadr args) language)))

(defmethod unparse-expression ((operator (eql :not)) (language (eql :ruby)) &optional args)
  (format nil "!(~a)" (unparse-expression (car args) language)))

(defmethod unparse-expression ((operator (eql :even)) (language (eql :ruby)) &optional args)
  (format nil "~a.even?" (unparse-expression (car args) language)))

(defmethod unparse-expression ((operator (eql :odd)) (language (eql :ruby)) &optional args)
  (format nil "~a.odd?" (unparse-expression (car args) language)))

(defmethod unparse-expression ((operator (eql :=)) (language (eql :ruby)) &optional args)
  (unparse-expression :eql language args))
(defmethod unparse-expression ((operator (eql :eql)) (language (eql :ruby)) &optional args)
  (format nil "(~a == ~a)" (unparse-expression (car args) language)
          (unparse-expression (cadr args) language)))

(defmethod unparse-expression ((operator (eql :regex)) (language (eql :ruby)) &optional args)
  (format nil "~a =~~ /~a/" (unparse-expression (car args) language) (cadr args)))

(defmethod unparse-expression ((operator (eql :length-between)) (language (eql :ruby)) &optional args)
  (unparse-expression :between language
     (list (list :literal (format nil "~a.length" (unparse-expression (car args) language)))
           (list :literal (unparse-expression (cadr args) language))
           (list :literal (unparse-expression (caddr args) language)))))

(defmethod unparse-expression ((operator (eql :length)) (language (eql :ruby)) &optional args)
  (unparse-expression :eql language
     (list (list :literal (format nil "~a.length" (unparse-expression (car args) language)))
           (list :literal (unparse-expression (cadr args) language)))))

(defmethod unparse-expression ((operator (eql :length-gt)) (language (eql :ruby)) &optional args)
  (unparse-expression :gt language
     (list (list :literal (format nil "~a.length" (unparse-expression (car args) language)))
           (list :literal (unparse-expression (cadr args) language)))))

(defmethod unparse-expression ((operator (eql :length-lt)) (language (eql :ruby)) &optional args)
  (unparse-expression :lt language
     (list (list :literal (format nil "~a.length" (unparse-expression (car args) language)))
           (list :literal (unparse-expression (cadr args) language)))))

(defmethod unparse-expression ((operator (eql :divide)) (language (eql :ruby)) &optional args)
  (format nil "(~{~a~^ / ~})" (mapcar #'(lambda (arg) (unparse-expression arg language)) args)))

(defmethod unparse-expression ((operator (eql :multiply)) (language (eql :ruby)) &optional args)
  (format nil "(~{~a~^ * ~})" (mapcar #'(lambda (arg) (unparse-expression arg language)) args)))

(defmethod unparse-expression ((operator (eql :add)) (language (eql :ruby)) &optional args)
  (format nil "(~{~a~^ + ~})" (mapcar #'(lambda (arg) (unparse-expression arg language)) args)))

(defmethod unparse-expression ((operator (eql :and)) (language (eql :ruby)) &optional args)
  (format nil "(~{~a~^ && ~})" (mapcar #'(lambda (arg) (unparse-expression arg language)) args)))

(defmethod unparse-expression ((operator (eql :coalesce)) (language (eql :ruby)) &optional args)
  (unparse-expression :or language args))
(defmethod unparse-expression ((operator (eql :or)) (language (eql :ruby)) &optional args)
  (format nil "(~{~a~^ || ~})" (mapcar #'(lambda (arg) (unparse-expression arg language)) args)))

(defmethod unparse-expression ((operator (eql :subtract)) (language (eql :ruby)) &optional args)
  (format nil "(~{~a~^ - ~})" (mapcar #'(lambda (arg) (unparse-expression arg language)) args)))

(defmethod unparse-expression ((operator (eql :min)) (language (eql :ruby)) &optional args)
  (format nil "~a.min" (unparse-array (car args) :ruby)))

(defmethod unparse-expression ((operator (eql :max)) (language (eql :ruby)) &optional args)
  (format nil "~a.max" (unparse-array (car args) :ruby)))

(defmethod unparse-expression ((obj (eql :current-date)) (language (eql :ruby)) &optional args)
  (unless (null args) (error "$current-date does not take any arguments"))
  "Date.today")

(defmethod unparse-expression ((obj (eql :current-time)) (language (eql :ruby)) &optional args)
  (unless (null args) (error "$current-date does not take any arguments"))
  "Time.now")

(defmethod unparse-expression ((obj (eql :current-timestamp)) (language (eql :ruby)) &optional args)
  (unless (null args) (error "$current-date does not take any arguments"))
  "Time.now")

(defmethod unparse-expression ((obj (eql :interval)) (language (eql :ruby)) &optional args)
  (let ((interval-types '("hours" "months" "days" "years" "weeks")))
    (unless (or (integerp (car args))
                (eql (data-type (car args)) :integer))
      (error "$interval requires an integer as its first argument"))
    (unless (member (cadr args) interval-types :test #'string-equal)
      (error "$interval requires a type designator from this list: ~a" interval-types))
    (format nil "~a.~a" (car args) (cadr args))))

(defun unparse-if-statement (test consequent &optional alternate)
  (unparse-expression :if :ruby (list test consequent alternate)))
(defmethod unparse-expression ((operator (eql :if)) (language (eql :ruby)) &optional args)
  (let ((lvl-1-indent (make-indent))
        (test (unparse-expression (car args) language))
        (consequent (format nil "~a" (unparse-expression (cadr args) language)))
        (alternative (when (third args) (format nil "~a" (unparse-expression (third args) language)))))
    (with-nesting
      (format nil "~aif ~a~%~a~%~a~aend" lvl-1-indent test
              (indent-block nil consequent)
              (if alternative
                  (format nil "~aelse~%~a~%" lvl-1-indent (indent-block nil alternative))
                  "")
              lvl-1-indent))))

(defmethod unparse-expression ((operator (eql :unless)) (language (eql :ruby)) &optional args)
  (let ((test (unparse-expression (car args) language))
        (consequent (format nil "~a" (unparse-expression (cadr args) language))))
    (format nil "~a unless ~a" consequent test)))

(defmethod unparse-expression ((operator (eql :when)) (language (eql :ruby)) &optional args)
  (let ((test (unparse-expression (car args) language))
        (consequent (format nil "~a" (unparse-expression (cadr args) language))))
    (format nil "~a if ~a" consequent test)))

(defmethod unparse-expression ((operator (eql :in)) (language (eql :ruby)) &optional args)
  (format nil "~a.include?(~a)" (unparse-array (cdr args) :ruby) (unparse-expression (car args) language)))

(defmethod unparse-expression ((operator (eql :not-in)) (language (eql :ruby)) &optional args)
  (format nil "!~a.include?(~a)" (unparse-array (cdr args) :ruby) (unparse-expression (car args) language)))

(defmethod unparse-expression ((operator (eql :as-date)) (language (eql :ruby)) &optional args)
  (format nil "~a.strftime(\"%d %b, %Y\")" (unparse-expression (car args) language)))

(defmethod unparse-expression ((operator (eql :as-quantity)) (language (eql :ruby)) &optional args)
  (format nil "~a.to_s.reverse.gsub(/(\\d{3})(?=\\d)/, '\\\\1,').reverse"
          (unparse-expression (car args) language)))

(defmethod unparse-expression ((operator (eql :concatenate)) (language (eql :ruby)) &optional args)
  (unparse-expression :strcat language args))
(defmethod unparse-expression ((operator (eql :strcat)) (language (eql :ruby)) &optional args)
  (format nil "(~{~a~^ + ~})" (mapcar #'(lambda(exp)
                                          (unparse-expression (list :to-string exp) language))
                                      args)))
(defmethod unparse-expression ((operator (eql :to-string)) (language (eql :ruby)) &optional args)
  (if (returns-string? (car args))
      (unparse-expression (car args) language)
      (format nil "~a.to_s" (unparse-expression (car args) language))))

(defmethod unparse-formatting ((data t) (type t))
  (format nil "~a.to_s" data))

(defmethod unparse-formatting ((data string) (type t))
  data)

(defmethod unparse-formatting ((data t) (type logical-type))
  (unparse-formatting data (id type)))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:
