;;; -*- Lisp -*-

(defpackage "PLATFORMER"
  (:shadowing-import-from "COMMON-LISP" "PI")
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES" "DEFUN" "FUNCALL" "LET*" "MULTIPLE-VALUE-BIND")
  (:use "COMMON-LISP" "LINEAR-FRACTIONAL-TRANSFORMATION" "NAMED-LET" "SERIES"))
