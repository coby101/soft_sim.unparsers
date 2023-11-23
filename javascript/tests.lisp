;;;===========================================================================
;;;
;;;  tests for code in unparsers/javascript/javascript.lisp
;;;  
;;;===========================================================================

(in-package :javascript-tests)


(define-test simple-unparse-tests
    (:tags '(simple-unparsing))
  (assert-equal "// **********
// first line
// second line
// ***********
" (javascript:comment-out nil "~a~%~a~%~a~%~a" "**********" "first line" "second line" "***********"))
  )
