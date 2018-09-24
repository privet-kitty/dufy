;;;; dufy-develop.asd -*- Mode: Lisp;-*-

(defsystem "dufy-internal"
  :description "Common definitions which are not external in the main package"
  :serial t
  :depends-on ("alexandria")
  :components ((:module "internal"
                :components
                ((:file "package")
                 (:file "utilities")
                 (:file "matrix")))))

