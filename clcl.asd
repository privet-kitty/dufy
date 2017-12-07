;;;; clcl.asd -*- Mode: Lisp;-*-

(in-package :cl-user)

(asdf:defsystem :clcl
  :version "0.0.3"
  :description "Color library for Common Lisp"
  :author "Hugo I."
  :license "MIT"
  :serial t
  :depends-on (:cl-ppcre :alexandria)
  :components ((:file "clcl")
	       (:file "deltae" :depends-on ("clcl"))
	       (:file "munsell-renotation-data")
	       (:file "munsell" :depends-on ("clcl" "munsell-renotation-data" "deltae"))))
