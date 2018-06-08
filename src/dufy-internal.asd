;;;; dufy-develop.asd -*- Mode: Lisp;-*-

(cl:in-package :cl-user)

(asdf:defsystem :dufy-internal
  :description "Common definitions which are not external in the main package"
  :serial t
  :depends-on (:alexandria)
  :components ((:module "internal"
                :components
                ((:file "package")
                 (:file "utilities")
                 (:file "matrix")))))

