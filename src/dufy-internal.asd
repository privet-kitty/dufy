;;;; -*- Mode: Lisp;-*-

(defsystem "dufy-internal"
  :description "Common definitions that are not exported in the main package"
  :serial t
  :depends-on ("alexandria")
  :components ((:module "internal"
                :components
                ((:file "package")
                 (:file "utilities")
                 (:file "matrix")))))
