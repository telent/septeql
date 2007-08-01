;; -*- Lisp -*-
(defpackage :septeql-system
  (:use "CL" "ASDF"))

(in-package :septeql-system)

(defsystem :septeql
  :version "0"
  :components ((:file "defpackage")
	       (:file "septeql" :depends-on ("defpackage"))
	       (:file "run-tests" :depends-on ("defpackage"))))		

;;; to test, call (do-tests).  XXX should write a test-op but I can't
;;; remember how
