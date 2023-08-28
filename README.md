# soft_sim.unparsers
Unparsers for outputting components of a soft_sim generatable application in various languages or formats

As a general rule, each language is a .lisp file and is its own package. "unparse" methods should be written for various soft_sim objects, such as FORMULA, ATTRIBUTE, OPERATOR, CONSTRAINT and various data types.

Development and usage of the different unparsers require a lisp environment with the software.simian code base loaded.

As a suggested starting point, here is a template for an example unparser for format "new"

BEGIN

(defpackage simian.new-unparser
  (:nicknames :new)
  (:use :simian :cl)
  (:export #:comment
           #:unparse
           #:unparse-method
           #:unparse-expression
           #:unparse-datatype
           #:unparse-array
           #:unparse-hash
           #:unparse-range))

(in-package :new)

(defmethod unparse ((obj t))
  (warn "no unparse method written for objects of type ~a" (type-of obj))
  (simian::unparse obj))

(defmethod unparse ((obj string)) (format nil "~a" obj))
(defmethod unparse ((obj number)) obj)

(defmethod unparse-method ((obj t))
  (warn "no unparse-method method written for objects of type ~a" (type-of obj))
  (simian::unparse-method obj))

(defmethod unparse-expression ((obj t))
  (warn "no unparse-expression method written for objects of type ~a" (type-of obj))
  (simian::unparse-expression obj))

(defmethod unparse-datatype ((obj t))
  (warn "no unparse-datatype method written for objects of type ~a" (type-of obj))
  (simian::unparse-datatype obj))

(defmethod unparse-array ((obj t))
  (warn "no unparse-array method written for objects of type ~a" (type-of obj))
  (simian::unparse-array obj))

(defmethod unparse-hash ((obj t))
  (warn "no unparse-hash method written for objects of type ~a" (type-of obj))
  (simian::unparse-hash obj))

(defmethod unparse-range ((obj t))
  (warn "no unparse-range method written for objects of type ~a" (type-of obj))
  (simian::unparse-range obj))


;;;===========================================================================
;;; Local variables:
;;; tab-width: 2
;;; indent-tabs-mode: nil
;;; End:

END

