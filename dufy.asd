;;;; dufy.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :dufy
  :version "0.1.11"
  :description "Color library for Common Lisp"
  :author "Hugo I."
  :license "MIT"
  :serial t
  :depends-on (:cl-ppcre :alexandria)
  :components ((:module "dat")
	       (:module "src"
		:components
		((:file "package")
		 (:file "fundamental-data" :depends-on ("package"))
		 (:file "general" :depends-on ("package"))
		 (:file "xyz" :depends-on ("package" "general" "fundamental-data"))
		 (:file "dufy" :depends-on ("package" "fundamental-data" "xyz" "general"))
		 (:file "deltae" :depends-on ("package" "dufy"))
		 (:file "munsell-renotation-data")
		 (:file "munsell" :depends-on ("package" "dufy" "munsell-renotation-data" "deltae" "general" "xyz")))))
  :in-order-to ((test-op (test-op dufy-test))))

