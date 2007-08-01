;; -*- Lisp -*-
(defpackage :septeql 
  (:nicknames "7QL")
  (:use "CL")
  (:export #:to-sql))

(in-package :septeql)
(defvar *tests-file*
  (merge-pathnames "tests.lisp"
		   (asdf:component-pathname (asdf:find-system :septeql))))
