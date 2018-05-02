;;;; dufy-examples.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :dufy-examples
    :description "Examples of dufy"
    :author "Hugo I."
    :license "MIT"
    :serial t
    :depends-on (:dufy :lispbuilder-sdl :iterate :alexandria)
    :components ((:module "examples" :components
                          ((:file "packages")
                           (:file "show-munsell-space")))))

