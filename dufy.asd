;;;; -*- Mode: Lisp;-*-

(defsystem "dufy"
  :version "0.2.8"
  :description "Color library for Common Lisp"
  :author "Hugo I."
  :license "MIT"
  :serial t
  :depends-on ("dufy-core" "dufy-munsell")
  :components ((:module "dat"
                :components ((:static-file "ciede2000-test-data.csv")))
               (:module "src"
                :components ((:file "package"))))
  :in-order-to ((test-op (test-op dufy-test))))
