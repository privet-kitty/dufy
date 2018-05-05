;;;; dufy-develop.asd -*- Mode: Lisp;-*-

(cl:in-package :cl-user)

(asdf:defsystem :dufy-internal
  :serial t
  :depends-on (:alexandria)
  :components ((:module "internal"
                :components
                ((:file "package")
                 (:file "general")
                 (:file "matrix")))))

