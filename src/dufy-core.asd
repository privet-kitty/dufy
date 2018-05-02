;;;; dufy.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :dufy-core
  :serial t
  :depends-on (:alexandria)
  :components ((:module "core"
		:components
		((:file "package")
		 (:file "cmf-data")
		 (:file "general")
		 (:file "xyz")
		 (:file "rgb")
		 (:file "lab-and-luv")
		 (:file "cat")
		 (:file "deltae")))))

