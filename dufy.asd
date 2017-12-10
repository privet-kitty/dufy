;;;; dufy.asd -*- Mode: Lisp;-*-

(in-package :cl-user)

(asdf:defsystem :dufy
  :version "0.0.4"
  :description "Color library for Common Lisp"
  :author "Hugo I."
  :license "MIT"
  :serial t
  :depends-on (:cl-ppcre :alexandria)
  :components ((:file "dufy")
	       (:file "deltae" :depends-on ("dufy"))
	       (:file "munsell-renotation-data")
	       (:file "munsell" :depends-on ("dufy" "munsell-renotation-data" "deltae"))))
