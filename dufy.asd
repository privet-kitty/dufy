;;;; dufy.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :dufy
  :version "0.2.3"
  :description "Color library for Common Lisp"
  :author "Hugo I."
  :license "MIT"
  :serial t
  :depends-on (:dufy-core :dufy-munsell)
  :components ((:module "dat")
               (:module "src"
                :components ((:file "package"))))
  :in-order-to ((test-op (test-op dufy-test))))

