;;;; clcl.asd -*- Mode: Lisp;-*-

(in-package :cl-user)

(asdf:defsystem :clcl
  :version "0.01"
  :description "Color library for Common Lisp"
  :author "Hugo I."
  :license "MIT"
  :serial t
  :depends-on (:cl-ppcre)
  :components ((:file "clcl")
	       (:file "munsell-renotation-data")
	       (:file "munsell" :depends-on ("clcl" "munsell-renotation-data"))))
