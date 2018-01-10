;;;; dufy.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :dufy
  :version "0.1.0"
  :description "Color library for Common Lisp"
  :author "Hugo I."
  :license "MIT"
  :serial t
  :depends-on (:cl-ppcre :alexandria)
  :components ((:file "package")
	       (:file "fundamental-data" :depends-on ("package"))
	       (:file "dufy" :depends-on ("package" "fundamental-data"))
	       (:file "deltae" :depends-on ("package" "dufy"))
	       (:file "munsell-renotation-data")
	       (:file "munsell" :depends-on ("package" "dufy" "munsell-renotation-data" "deltae")))
  :in-order-to ((test-op (test-op dufy-test))))
