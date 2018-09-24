;;;; dufy-core.asd -*- Mode: Lisp;-*-

(defsystem "dufy-core"
  :serial t
  :depends-on ("alexandria" "dufy-internal")
  :components ((:module "core"
		:components
		((:file "package")
                 (:file "colorspace")
		 (:file "cmf-data")
		 (:file "xyz")
                 (:file "illuminants-data")
		 (:file "rgb")
		 (:file "lab-and-luv")
		 (:file "cat")
                 (:file "rgbspaces-data")
		 (:file "deltae")))))

