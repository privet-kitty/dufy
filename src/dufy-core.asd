;;;; dufy-core.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :dufy-core
  :serial t
  :depends-on (:alexandria :dufy-internal)
  :components ((:module "core"
		:components
		((:file "package")
		 (:file "cmf-data")
		 (:file "xyz")
		 (:file "rgb")
		 (:file "lab-and-luv")
		 (:file "cat")
		 (:file "deltae")))))

