;;;; dufy.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :dufy-munsell
  :serial t
  :depends-on (:dufy-core :cl-ppcre)
  :components ((:module "munsell"
		:components
		((:file "munsell-renotation-data")
		 (:file "munsell")))))

