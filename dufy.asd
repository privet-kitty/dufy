;;;; dufy.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :dufy
  :version "0.1.14"
  :description "Color library for Common Lisp"
  :author "Hugo I."
  :license "MIT"
  :serial t
  :depends-on (:cl-ppcre :alexandria)
  :components ((:module "dat")
	       (:module "src"
		:components
		((:file "package")
		 (:file "fundamental-data")
		 (:file "general")
		 (:file "xyz")
		 (:file "rgb")
		 (:file "lab-and-luv")
		 (:file "cat")
		 (:file "deltae")
		 (:file "munsell-renotation-data")
		 (:file "munsell"))))
  :in-order-to ((test-op (test-op dufy-test))))

