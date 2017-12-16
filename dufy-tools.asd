;;;; dufy-tools.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :dufy-tools
  :description "DUFY examples"
  :author "privet-kitty"
  :license "MIT"
  :serial t
  :depends-on (:dufy :alexandria :fast-io)
  :components ((:file "munselltool"))
)
