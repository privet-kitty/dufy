;;;; -*- Mode: Lisp;-*-

(in-package :cl-user)


(asdf:defsystem :dufy-test
  :description "Test system for Dufy"
  :author "Hugo I."
  :license "MIT"
  :depends-on (:dufy :fiveam :cl-csv :parse-float)
  :components ((:file "dufy-test"))
  :perform (asdf:test-op (o s)
			 (uiop:symbol-call :fiveam :run! :dufy-suite)))
