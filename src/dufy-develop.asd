;;;; dufy-develop.asd -*- Mode: Lisp;-*-

(cl:in-package :cl-user)

(asdf:defsystem :dufy-develop
  :serial t
  :depends-on (:alexandria :clgplot)
  :components ((:module "develop"
                        :components
                        ((:file "tools")))))

