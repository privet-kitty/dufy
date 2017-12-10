;;;; dufy-tools.asd -*- Mode: Lisp;-*-

(in-package :cl-user)

(asdf:defsystem :dufy-tools
  :description "DUFY examples"
  :author "privet-kitty"
  :license "MIT"
  :serial t
  :depends-on (:dufy :nibbles :alexandria :fast-io)
  :components ((:file "munselltool"))
)
